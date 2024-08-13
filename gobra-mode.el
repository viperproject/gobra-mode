;;; gobra-mode.el --- Support for Gobra in Emacs -*- lexical-binding: t -*-

;; Copyright (c) 2022- ETH Zurich.

;; Keywords: lisp
;; Version: 0.0.1
;; URL: https://github.com/viperproject/gobra-mode
;; Package-Requires: ((emacs "26.2"))

;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

;;; Commentary:

;; Adds syntax highlighting for Gobra and interaction capabilities
;; with the Gobra executable.

;;; Code:

(require 'param-config)
(require 'gobra-params)

(defvar-local gobra-async-buffer nil "Keeps the async buffer in which gobra runs.")
(defvar-local gobra-buffer nil "The buffer for which gobra runs.")
(defvar-local gobra-highlight-overlays nil "Keeps the highlight overlays of errors.")
(defvar-local gobra-is-verified nil "Keeps the status of the program regarding the verification.\nIt is nil if the verification hasn't ran, 1 if the program is verified and 2 if it has failed to verify.")
(defvar gobra-output-buffer-prelude "echo \"Gobra output buffer actions:\"; echo \"For next error press 'n'\"; echo \"For previous error press 'p'\" ; echo \"For visiting current error press 'RET'\"" "Holds the stuff to print in the Gobra output buffer.")
(defvar gobra-current-async-buffer nil "Holds the buffer where Gobra is currently running.")
(defvar gobra-development-path nil "Holds the path of the gobra development src directory.")
(defvar gobra-z3-path nil "Holds the path to Z3 binary.")
(defvar gobra-jar-path nil "Holds the path to gobra jar file.")
(defvar gobra-actions-before-go-mode (lambda () nil) "Function ran before the go mode hooks kick in.")
(defvar gobra-verification-hook nil "Things to do when verification finishes.")
(defvar gobra-enable-verification-hook nil "If non-nil, run verification hook when verification finishes.")
(defvar gobra-prevent-verification-buffer-popup nil "If non-nil, the verification buffer won't be popped.")
(defvar-local gobra-ghost-overlays nil "Holds the overlays for ghost code folding.")

;; faces

(defgroup gobra-faces nil
  "Gobra highlight faces."
  :group 'tools)

(defface gobra-error
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "Red1"))
    (t
     :underline t :inherit error))
  "Gobra face for errors."
  :group 'gobra-faces)

(defface gobra-verified-face
  '((t (:weight bold :foreground "Green")))
  "The face used to highlight succesful verification.")

(defface gobra-unverified-face
  '((t (:weight bold :foreground "Red")))
  "The face used to highlight failed verification.")

(defface gobra-notran-face
  '((t (:weight bold :foreground "Orange")))
  "The face used to highlight not run verification.")

(defface gobra-argument-face
  '((t (:foreground "Grey")))
  "The face used to distinguish args from args of args in the arguments construction buffer.")

(defface gobra-spec-face
  '((t (:slant italic :foreground "forest green")))
  "The face for gobra specification in go buffers.")


;; ==============
;; comment bypass
;; ==============

(defconst gobra-comment-regexp-prefixes '("// ?@" "/\\* ?@"))

(defun gobra-update-nth (l n i)
  "Return a copy of the list L with the Nth element updated to I."
  (append (cl-subseq l 0 n) (list i) (nthcdr (1+ n) l)))

(defun gobra-font-lock-syntactic-face-function (orig-fun state)
  ;; If we are in a comment and it starts with one of the `gobra-comment-regexp-prefixes'.
  (if (and (nth 4 state)
           (save-excursion
             (goto-char (nth 8 state))
             (cl-reduce (lambda (a b) (or a (looking-at b)))
                        gobra-comment-regexp-prefixes
                        :initial-value
                        nil)))
      ;; Return the gobra-spec-face
      'gobra-spec-face
    ;; Default behavior.
    (funcall orig-fun state)))

;; =====
;; logic
;; =====

(defun gobra-all-buffers ()
  "Find all .go and .gobra buffers."
  (cl-map 'list
          'car
          (seq-filter
           (lambda (x)
             (when (cdr x)
               (or (string-match ".*\\.gobra$" (cdr x)) (string-match ".*\\.go$" (cdr x)))))
           (cl-map 'list
                   (lambda (x)
                     (cons x (buffer-file-name x)))
                   (buffer-list)))))

(defun gobra-find-gobra-buffer (file)
  "Find the buffer corresponding to FILE."
  (let ((b (seq-filter
            (lambda (x)
              (when (cdr x)
                (equal (cdr x) file)))
            (cl-map 'list
                    (lambda (x)
                      (cons x (buffer-file-name x)))
                    (buffer-list)))))
    (when b
      (car (car b)))))

(defun gobra-parse-error (l)
  "Parse error in line L."
  (let ((success (string-match ".*<\\(.*\\):\\([0123456789]*\\):\\([0123456789]*\\)>\\(.*\\)" l)))
    (when success
      (let ((file (match-string 1 l))
            (l (string-to-number (match-string 2 l)))
            (c (string-to-number (match-string 3 l)))
            (err (match-string 4 l)))
        (when (and file l c err)
          (let ((buf (gobra-find-gobra-buffer file)))
            (when buf
              (with-current-buffer buf
                (setq-local gobra-is-verified 2)
                (save-excursion
                  (goto-char (point-min))
                  (forward-line (1- l))
                  (let ((curline (thing-at-point 'line)))
                    (forward-char (if (string-match "^[[:blank:]]*//\\( ?\\)@.*" curline)
                                      (+ (+ 3 (length (match-string 1 curline))) (1- c))
                                    (1- c))))
                  (let ((start (point)))
                    (end-of-line)
                    (list start (point) file err)))))))))))

(defun gobra-parse-errors (data)
  "Parse gobra output errors in DATA."
  (cl-map 'list
          (lambda (l)
            (let* ((info (gobra-parse-error l))
                   (start (car info))
                   (end (nth 1 info))
                   (file (nth 2 info))
                   (err (nth 3 info)))
              (when info
                (let ((buf (gobra-find-gobra-buffer file)))
                  (when buf
                    (with-current-buffer buf
                      (let ((ov (make-overlay
                                 start
                                 end)))
                        (push ov gobra-highlight-overlays)
                        (overlay-put ov 'face 'gobra-error)
                        (overlay-put ov 'help-echo (substring-no-properties err))
                        (overlay-put ov
                                     'cursor-sensor-functions
                                     (list
                                      (lambda (window pos action)
                                        (when (eq action 'entered)
                                          (message "%s" (substring-no-properties err))))))
                        (message "%s" (substring-no-properties err)))))))))
          data))

(defun gobra-extract-num-errors (data)
  "Find the useful part of gobra output in DATA."
  (let ((res 0))
    (while data
      (let ((s (car data)))
        (when (string-match ".* - Gobra has found \\([0123456789]*\\) error(s).*" s)
          (let ((num (match-string 1 s)))
            (setq res (+ res
                         (if num
                             (string-to-number num)
                           0))))))
      (setq data (cdr data)))
    res))

(defun gobra-read-sentinel (proc signal)
  "Sentinel waiting for async process PROC of gobra to finish verification with SIGNAL."
  (with-current-buffer (if gobra-async-buffer gobra-async-buffer gobra-current-async-buffer)
    (read-only-mode 1)
    (let ((out (buffer-string)))
      (let ((useful (split-string out "\n")))
        (when useful
          (let ((numerrors (gobra-extract-num-errors useful)))
            (cl-map 'list
                    (lambda (buf)
                      (with-current-buffer buf
                        (seq-do #'delete-overlay gobra-highlight-overlays)))
                    (gobra-all-buffers))
            (if (equal numerrors 0)
                (progn
                  (message "Program verified succesfully!")
                  (with-current-buffer gobra-buffer
                    (setq-local gobra-is-verified 1)))
              (gobra-parse-errors (cdr useful))
              (with-current-buffer gobra-buffer
                (setq-local gobra-is-verified 2)))))))
    (when (and gobra-enable-verification-hook (or (equal (string-trim signal) "exited abnormally with code 1") (equal (string-trim signal) "finished")))
      (cl-map 'list #'funcall gobra-verification-hook))))

(defun gobra-printvpr-sentinel (proc signal)
  "Sentinel waiting for async process PROC of gobra to finish the production of vpr code with SIGNAL."
  (with-current-buffer (if gobra-async-buffer gobra-async-buffer gobra-current-async-buffer)
    (read-only-mode 1)
    (let ((out (buffer-string)))
      (let ((useful (split-string out "\n")))
        (when useful
          (let (numerrors (gobra-extract-num-errors useful))
            (cl-map 'list
                    (lambda (buf)
                      (with-current-buffer buf
                        (seq-do #'delete-overlay gobra-highlight-overlays)))
                    (gobra-all-buffers))
            (if (equal numerrors 0)
                (progn
                  (message "Program verified succesfully!")
                  (with-current-buffer gobra-buffer
                    (setq-local gobra-is-verified 1)))
              (gobra-parse-errors (cdr useful))
              (with-current-buffer gobra-buffer
                (setq-local gobra-is-verified 2)))))))
    (when (and gobra-enable-verification-hook (or (equal (string-trim signal) "exited abnormally with code 1") (equal (string-trim signal) "finished")))
      (cl-map 'list #'funcall gobra-verification-hook)))
  (with-current-buffer gobra-buffer
    (find-file-other-window (concat (buffer-file-name) ".vpr"))
    (viperlanguage-mode)))

(defun gobra-verify ()
  "Verify current buffer."
  (interactive)
  (if gobra-prevent-verification-buffer-popup
      (save-window-excursion
        (gobra-verify-helper))
    (gobra-verify-helper)))

(defun gobra-verify-helper ()
  (setq-local gobra-buffer (current-buffer))
  (when gobra-z3-path
    (setenv "Z3_EXE" gobra-z3-path))
  (let* ((cmd (format "java -jar -Xss1g %s %s" gobra-jar-path (gobra-args-serialize)))
         (buf (get-buffer-create "*Gobra Command Output*"))
         (_ (with-current-buffer buf (read-only-mode 0)))
         (b (format "%s" (async-shell-command (format "%s ; echo ; echo \"Gobra command: %s\"; echo ; time %s" gobra-output-buffer-prelude cmd cmd) buf))))
    (string-match "window [1234567890]* on \\(.*\\)>" b)
    (setq-local gobra-async-buffer (match-string 1 b))
    (setq gobra-current-async-buffer (match-string 1 b))
    (setq-local gobra-is-verified 3)
    (let ((gb (current-buffer)))
      (with-current-buffer gobra-async-buffer
        (gobra-output-mode)
        (setq-local gobra-buffer gb)))
    (let ((proc (get-buffer-process gobra-async-buffer)))
      (when (process-live-p proc)
        (set-process-sentinel proc #'gobra-read-sentinel)))))

(defun gobra-verify-line ()
  "Verify current buffer."
  (interactive)
  (if gobra-prevent-verification-buffer-popup
      (save-window-excursion
        (gobra-verify-line-helper))
    (gobra-verify-line-helper)))

(defun gobra-verify-line-helper ()
  "Verify current buffer."
  (setq-local gobra-buffer (current-buffer))
  (when gobra-z3-path
    (setenv "Z3_EXE" gobra-z3-path))
  (let* ((cmd (format "java -jar -Xss1g %s %s" gobra-jar-path (gobra-args-serialize (cons (buffer-file-name) (line-number-at-pos)))))
         (buf (get-buffer-create "*Gobra Command Output*"))
         (_ (with-current-buffer buf (read-only-mode 0)))
         (b (format "%s" (async-shell-command (format "%s ; echo ; echo \"Gobra command: %s\"; echo ; time %s" gobra-output-buffer-prelude cmd cmd) buf))))
    (string-match "window [1234567890]* on \\(.*\\)>" b)
    (setq-local gobra-async-buffer (match-string 1 b))
    (setq gobra-current-async-buffer (match-string 1 b))
    (setq-local gobra-is-verified 3)
    (let ((gb (current-buffer)))
      (with-current-buffer gobra-async-buffer
        (gobra-output-mode)
        (setq-local gobra-buffer gb)))
    (let ((proc (get-buffer-process gobra-async-buffer)))
      (when (process-live-p proc)
        (set-process-sentinel proc #'gobra-read-sentinel)))))

(defun gobra-printvpr ()
  "Open viper file for current buffer."
  (interactive)
  (if gobra-prevent-verification-buffer-popup
      (save-window-excursion
        (gobra-printvpr-helper))
    (gobra-printvpr-helper)))

(defun gobra-printvpr-helper ()
  "Open viper file for current buffer."
  (interactive)
  (setq-local gobra-buffer (current-buffer))
  (setenv "Z3_EXE" gobra-z3-path)
  (let* ((extra-arg (if (not (member "printVpr" gobra-args-set))
                        " --printVpr "
                      ""))
         (cmd (format "java -jar -Xss1g %s %s" gobra-jar-path (gobra-args-serialize)))
         (buf (get-buffer-create "*Gobra Command Output*"))
         (_ (with-current-buffer buf (read-only-mode 0)))
         (b (format "%s" (async-shell-command (format "%s ; echo ; echo \"Gobra command: %s %s\"; echo ; time %s %s" gobra-output-buffer-prelude cmd extra-arg cmd extra-arg) buf))))
    (string-match "window [1234567890]* on \\(.*\\)>" b)
    (setq-local gobra-async-buffer (match-string 1 b))
    (setq gobra-current-async-buffer (match-string 1 b))
    (setq-local gobra-is-verified 3)
    (let ((gb (current-buffer)))
      (with-current-buffer gobra-async-buffer
        (gobra-output-mode)
        (setq-local gobra-buffer gb)))
    (let ((proc (get-buffer-process gobra-async-buffer)))
      (when (process-live-p proc)
        (set-process-sentinel proc #'gobra-printvpr-sentinel)))))

(defun gobra-print-run-command ()
  "Print the gobra command that should run in an sbt shell and push it to the kill ring."
  (interactive)
  (let ((c (format "run%s" (gobra-args-serialize))))
    (message c)
    (kill-new c)))

(defun gobra-mode-line ()
  "Return the mode line string."
  (if gobra-minor-mode
      (if (not gobra-is-verified)
          (concat "[" (propertize "Unknown" 'face 'gobra-notran-face) (if gobra-enable-verification-hook " 🔔" "") "]")
        (if (equal gobra-is-verified 1)
            (concat "[" (propertize "Verified" 'face 'gobra-verified-face) (if gobra-enable-verification-hook " 🔔" "") "]")
          (if (equal gobra-is-verified 2)
              (concat "[" (propertize "Unverified" 'face 'gobra-unverified-face) (if gobra-enable-verification-hook " 🔔" "") "]")
            (concat "[" (propertize "Verifying..." 'face 'gobra-notran-face) (if gobra-enable-verification-hook " 🔔" "") "]"))))
    ""))

;; Gobra ghost code folding and unfolding (comments only)

(defun gobra-is-annotation-line ()
  "Determine if the current line is annotation or not."
  (string-match-p "^[[:blank:]]*// ?@.*" (thing-at-point 'line)))

(defun gobra-expand-ghost-region ()
  "Select a chunk of ghost code that has the cursor inside."
  (if (gobra-is-annotation-line)
      (save-excursion
        (let ((orig (point))
              (forward (progn (end-of-line) (point)))
              (backward (progn (beginning-of-line) (point))))
          (forward-line)
          (while (and (gobra-is-annotation-line) (not (equal (point) (point-max))))
            (setq forward (progn (end-of-line) (point)))
            (forward-line))
          (when (gobra-is-annotation-line)
            (setq forward (progn (end-of-line) (point))))
          (goto-char orig)
          (forward-line -1)
          (while (and (gobra-is-annotation-line) (not (equal (point) (point-min))))
            (setq backward (progn (beginning-of-line) (point)))
            (forward-line -1))
          (when (gobra-is-annotation-line)
            (setq backward (progn (beginning-of-line) (point))))
          (if (and forward backward)
              (cons backward forward)
            nil)))
    nil))

(defun gobra-hide ()
  "Hide the region with ghost code under cursor."
  (let* ((region (gobra-expand-ghost-region))
         (start (car region))
         (end (cdr region))
         (offset (save-excursion
                   (goto-char start)
                   (let* ((s (thing-at-point 'line))
                          (m (string-match "^\\([[:blank:]]*\\)//.*" s)))
                     (match-string 1 s))))
         ov)
    (when region
      (setq ov (make-overlay start end))
      (push ov gobra-ghost-overlays)
      (overlay-put ov 'display (propertize (format "%sghost..." offset) 'face 'gobra-spec-face))
      (overlay-put ov 'invisible t)
      (goto-char start))))

(defun gobra-show-all ()
  "Show all the hidden regions with ghost code."
  (interactive)
  (seq-do #'delete-overlay gobra-ghost-overlays)
  (setq-local gobra-ghost-overlays nil))

(defun gobra-show ()
  "Show the hidden ghost code near cursor."
  (let* ((data (gobra-show-helper gobra-ghost-overlays nil nil))
         (newl (car data))
         (change (cdr data)))
    (if change
        (progn
          (setq-local gobra-ghost-overlays newl)
          t)
      nil)))

(defun gobra-show-helper (l change newl)
  "Helper for gobra-show.  L is the list of overlays.  CHANGE is t when a change has occured.  NEWL is the aggregation list for tail recursion."
  (if l
      (let ((start (overlay-start (car l)))
            (end (overlay-end (car l))))
        (if (or (> 2 (abs (- (point) start))) (> 2 (abs (- (point) end))))
            (progn (delete-overlay (car l))
                   (gobra-show-helper (cdr l) t newl))
          (gobra-show-helper (cdr l) change (cons (car l) newl))))
    (cons newl change)))

(defun gobra-fold-unfold ()
  "Folds or unfolds the ghost code near point."
  (interactive)
  (unless (gobra-show)
    (gobra-hide)))

(defun gobra-get-longest-spec-token (start end)
  "Return nil if not currently in a function contract.  Otherwise get the longest token in region START - END."
  (save-excursion
    (let (flag
          (m 0))
      (goto-char start)
      (while (and (<= (point) end) (not (eobp)))
        (let ((s (thing-at-point 'line)))
          (when (string-match-p "^[[:blank:]]*// ?@ requires .*" s)
            (when (not (equal m 3))
              (setq m 2)
              (setq flag t)))
          (when (string-match-p "^[[:blank:]]*// ?@ preserves .*" s)
            (setq m 3)
            (setq flag t))
          (when (string-match-p "^[[:blank:]]*// ?@ decreases .*" s)
            (setq m 3)
            (setq flag t))
          (when (string-match-p "^[[:blank:]]*// ?@ decreases[[:blank:]]*" s)
            (setq m 3)
            (setq flag t)))
        (forward-line))
      (when flag m))))

(defun gobra-add-spec-spaces (longest start end)
  "Add the necessary spaces after requires, ensures, preserves in region START - END given LONGEST."
  (save-excursion
    (goto-char start)
    (while (and (<= (point) end) (not (eobp)))
      (let ((s (thing-at-point 'line)))
        (when longest
          (when (string-match "^[[:blank:]]*// ?@ ensures\\([[:blank:]]+\\)\\([^[:blank:]]+.*\\)" s)
            (beginning-of-line)
            (forward-char (match-beginning 2))
            (delete-char (- (length (match-string 1 s))))
            (insert (substring "   " 0 longest)))
          (when (string-match "^[[:blank:]]*// ?@ requires\\([[:blank:]]+\\)\\([^[:blank:]]+.*\\)" s)
            (beginning-of-line)
            (forward-char (match-beginning 2))
            (delete-char (- (length (match-string 1 s))))
            (insert (substring "   " 0 (1- longest))))
          (when (string-match "^[[:blank:]]*// ?@ preserves\\([[:blank:]]+\\)\\([^[:blank:]]+.*\\)" s)
            (beginning-of-line)
            (forward-char (match-beginning 2))
            (delete-char (- (length (match-string 1 s))))
            (insert " ")))
        (when (string-match "^[[:blank:]]*//\\(@\\).*" s)
          (beginning-of-line)
          (forward-char (match-beginning 1))
          (insert " ")))
      (forward-line))))

(defun gobra-format-spec ()
  "Format the current function spec aligning the requires, preserves and ensures arguments."
  (interactive)
  (let* ((region (gobra-expand-ghost-region))
         (start (car region))
         (end (cdr region)))
    (when region
      (let ((longest (gobra-get-longest-spec-token start end)))
        (gobra-add-spec-spaces longest start end)))))

(defun gobra-find-ghost (forward)
  "If FORWARD is nil search for the next chunk of ghost code backwards, else forward."
  (let ((inc (if forward 1 -1))
        (check (if forward 'eobp 'bobp)))
    (push-mark)
    (while (and (gobra-is-annotation-line) (not (funcall check)))
      (forward-line inc))
    (while (and (not (gobra-is-annotation-line)) (not (funcall check)))
      (forward-line inc))))

(defun gobra-next-ghost ()
  "Find next chunk of ghost code."
  (interactive)
  (gobra-find-ghost t))

(defun gobra-prev-ghost ()
  "Find previous chunk of ghost code."
  (interactive)
  (gobra-find-ghost nil))

(defun gobra-format-all-spec ()
  "Format all ghost code in the file."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (gobra-format-spec)
      (gobra-next-ghost))))

;; Keymaps

(define-derived-mode gobra-mode go-mode
  "gobra mode"
  "Major mode for editing Go programs verified by Gobra"
  (delay-mode-hooks
    (funcall gobra-actions-before-go-mode)
    (cursor-sensor-mode)
    (gobra-args-initialize))
  (run-mode-hooks))

(defvar gobra-keywords '("invariant"
                         "requires"
                         "ensures"
                         "preserves"
                         "trusted"
                         "share"
			                   "opaque"
			                   "reveal"
                         "outline"
                         "pred"
                         "pure"
                         "exists"
                         "assume"
                         "apply"
                         "inhale"
                         "exhale"
                         "assert"
                         "ghost"
                         "implements"
                         "unfolding"
                         "let"
                         "fold"
                         "unfold"
                         "decreases")
  "Holds all the gobra keywords which should be highlighted.")

(defvar gobra-comment-char ?┆)

(defvar gobra-multiline-comment-open-char ?┆)

(defvar gobra-multiline-comment-close-char ?┆)

(defvar gobra-pretty-symbols
  `(("<=" . 8804)
    (">=" . 8805)
    ("!=" . 8800)
    (":=" . 8788)
    ("&&" . 10043)
    ("||" . 8744)
    ("=>" . 8658)
    ("<==>" . 10234)
    ("==>" . 10233)
    ("<==" . 10232)
    ("exists" . 8707)
    ("::" . 8729)
    ("in" . 8712)
    ("==" . 65309)
    ("===" . 8801)
    ("!==" . 8802)
    ("// @" . ,gobra-comment-char)
    ("//@" . ,gobra-comment-char)
    ("/* @" . ,gobra-multiline-comment-open-char)
    ("/*@" . ,gobra-multiline-comment-open-char)
    ("@ */" . ,gobra-multiline-comment-close-char)
    ("@*/" . ,gobra-multiline-comment-close-char))
  "Symbols that gobra-mode will automatically prettify")


(defhydra gobra-minor-mode-hydra (:hint
                                  nil
                                  :exit
                                  t)
  "
^Verification^         ^Arguments^          ^Folding^          ^Ghost^               ^gobra-mode^
^^^^^^^^-----------------------------------------------------------------------------------------
_v_: verify            _a_: edit args       _h_: fold/unfold   _f_  : format spec    _b_: toggle notifications
_l_: verify line       _s_: print command   _j_: show all      _n_  : next ghost     _k_: toggle buffer popup
_c_: verify + viper                                        _p_  : prev ghost
                                                         _C-f_: format all spec
"
  ("v" gobra-verify)
  ("a" gobra-edit-args)
  ("s" gobra-print-run-command)
  ("h" gobra-fold-unfold :color red)
  ("j" gobra-show-all)
  ("l" gobra-verify-line)
  ("c" gobra-printvpr)
  ("f" gobra-format-spec :color red)
  ("n" gobra-next-ghost :color red)
  ("p" gobra-prev-ghost :color red)
  ("C-f" gobra-format-all-spec)
  ("b" gobra-toggle-bell)
  ("k" gobra-toggle-popup)
  ("q" nil "cancel" :color blue))


(define-minor-mode gobra-minor-mode
  "Minor mode for gobra (used primarily in go files)."
  :init-value nil
  :lighter " Gobra"
  :keymap (list (cons (kbd "C-c g") 'gobra-minor-mode-hydra/body))
  (cursor-sensor-mode)
  (gobra-args-initialize)
  (add-function :around (local 'font-lock-syntactic-face-function)
                 #'gobra-font-lock-syntactic-face-function)
  (setq-local prettify-symbols-compose-predicate
              (lambda (start end _match)
                (let* ((syntaxes-beg (if (memq (char-syntax (char-after start)) '(?w ?_))
                                         '(?w ?_) '(?. ?\\)))
                       (syntaxes-end (if (memq (char-syntax (char-before end)) '(?w ?_))
                                         '(?w ?_) '(?. ?\\))))
                  (not (or (memq (char-syntax (or (char-before start) ?\s)) syntaxes-beg)
                           (memq (char-syntax (or (char-after end) ?\s)) syntaxes-end))))))
  (font-lock-add-keywords nil (list
                               (cons (concat "\\<" (regexp-opt gobra-keywords) "\\>")
                                     '((0 font-lock-builtin-face)))
                               (list "\\(\\_<forall\\_>\\).*?::"
                                     '(1 (compose-region (match-beginning 1) (match-end 1) ?∀))
                                     '(1 font-lock-builtin-face append))
                               (list "\\(-\\)\\(-\\)\\(*\\)"
                                     '(1 (compose-region (match-beginning 1) (match-end 1) ?‒))
                                     '(1 (compose-region (match-beginning 2) (match-end 2) ?‒))
                                     '(1 (compose-region (match-beginning 3) (match-end 3) ?✻)))))
  (setq-local prettify-symbols-alist gobra-pretty-symbols)
  (prettify-symbols-mode)
  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings (font-lock-fontify-buffer))))
  (setq global-mode-string (or global-mode-string '("")))
  (unless (member '(:eval (gobra-mode-line)) global-mode-string)
    (setq global-mode-string (append global-mode-string '((:eval (gobra-mode-line)))))))

;; argument selection buffer major mode

(defun gobra-args-initialize ()
  "Set the initial value of set of arguments and arguments of arguments variables."
  (when (not (assoc "input" gobra-mode-config))
    (setq-local gobra-mode-config (cons (list "input" (buffer-file-name (current-buffer))) gobra-mode-config))))

(defun gobra-get-gobra-files (dir)
  "Get all '.go' or '.gobra' files in DIR."
  (let ((files (seq-filter (lambda (f) (and (string-match-p "\\(.+\\.go$\\)\\|\\(.*\\.gobra$\\)" f) (not (file-directory-p f)))) (directory-files dir t))))
    (apply 'concatenate (cons 'string (cl-map 'list (lambda (f) (concat f " ")) files)))))

(defun gobra-insert-file-line (file-line args-of-args)
  "FILE-LINE is a file with a corresponding line which we want to verify.  Insert that line to the corresponding file in ARGS-OF-ARGS."
  (let ((file (car file-line))
        (line (cdr file-line))
        (files (split-string args-of-args " ")))
    (apply 'concat
           (cl-map 'list
                   (lambda (f)
                     (if (equal file f)
                         (format "%s@%s " file line)
                       (concat f " ")))
                   files))))

(defun gobra-has-header (f)
  (with-temp-buffer
    (insert-file-contents f)
    (buffer-string)
    (goto-char (point-min))
    (let ((found nil)
          (esc nil))
      (while (and (not (eobp)) (not found) (not esc))
        (let ((line (thing-at-point 'line)))
          (when (string-match-p "^package .*" line)
            (setq esc t))
          (when (string-match-p "^// \\+gobra" line)
            (setq found t))
          (forward-line)))
      found)))

(defun gobra-transform-params (file-line)
  (if (and file-line
           (assoc "directory" gobra-mode-config))
      (let* ((directories (cdr (assoc "directory" gobra-mode-config)))
             (params (seq-filter (lambda (c) (not (equal (car c) "directory"))) gobra-mode-config))
             (files (apply 'append
                           (cl-map
                            'list
                            (lambda (directory)
                              (seq-filter
                               (lambda (fname)
                                 (or
                                  (string-match-p ".*\\.gobra$" fname)
                                  (string-match-p ".*\\.go$" fname)))
                               (directory-files directory t)))
                            directories))))
        (if (assoc "onlyFilesWithHeader" gobra-mode-config)
            (cons (cons "input"
                        (seq-filter
                         'gobra-has-header
                         files))
                  params)
          (cons (cons "input" files) params)))
    gobra-mode-config))

(defun gobra-args-serialize (&optional file-line)
  "Return the arguments string.  If the optional argument FILE-LINE is specified as a tuple (filename line), the specific line will be appended to the corresponding file for specific member verification."
  (let* ((params (gobra-transform-params file-line))
         (s ""))
    (while params
      (let ((cur (car params)))
        (setq s (string-trim-right
                 (format "%s --%s %s"
                         s
                         (car cur)
                         (or
                          (string-trim-left
                           (cl-reduce
                            (lambda (sofar arg)
                              (concat sofar
                                      " "
                                      (if (and (equal (car cur) "input")
                                               file-line
                                               (equal (car file-line) arg))
                                          (format "%s@%s" arg (cdr file-line))
                                        arg)))
                            (cdr cur)
                            :initial-value ""))
                          ""))))
        (setq params (cdr params))))
    s))

(defun gobra-edit-args ()
  "Spawn the construction buffer for the arguments."
  (interactive)
  (param-config-edit-params gobra-params 'gobra-mode-config))

(defun gobra-toggle-bell ()
  "Toggles the verification hook use."
  (interactive)
  (setq gobra-enable-verification-hook (not gobra-enable-verification-hook)))

(defun gobra-toggle-popup ()
  "Toggles the popping up of the verification buffer"
  (interactive)
  (setq gobra-prevent-verification-buffer-popup (not gobra-prevent-verification-buffer-popup)))

;; major mode for gobra output buffer

(defun gobra-output-next-error ()
  "Jumps to the next error in the gobra output buffer."
  (interactive)
  (let (c)
    (save-excursion
      (while (and (not (eobp)) (not c))
        (forward-line)
        (when (string-match ".*ERROR.*<.*:[0123456789]*:[0123456789]*>.*" (thing-at-point 'line))
          (setq c (point)))))
    (if c
        (goto-char c)
      (message "No further errors"))))

(defun gobra-output-prev-error ()
  "Jumps to the previous error in the gobra output buffer."
  (interactive)
  (let (c)
    (save-excursion
      (while (and (not (bobp)) (not c))
        (forward-line -1)
        (when (string-match ".*ERROR.*<.*:[0123456789]*:[0123456789]*>.*" (thing-at-point 'line))
          (setq c (point)))))
    (if c
        (goto-char c)
      (message "No further errors"))))

(defun gobra-output-goto-error ()
  "Jump to the error that corresponds to this line in the output buffer."
  (interactive)
  (let ((line (thing-at-point 'line)))
    (let ((success (string-match ".*<\\(.*\\):\\([0123456789]*\\):[0123456789]*>.*" line)))
      (if success
          (let ((file (match-string 1 line))
                (l (string-to-number (match-string 2 line))))
            (when (and file l)
              (let ((buf (gobra-find-gobra-buffer file)))
                (when (not buf)
                  (setq buf (find-file-other-window file)))
                (pop-to-buffer buf)
                (goto-char (point-min))
                (forward-line (1- l)))))
        (when gobra-development-path
          (let ((success (string-match ".*\\(viper\\.gobra\\..*[ABCDEFGHIJKLMNOPQRSTUVWXYZ].*\\)\\..*(.*:\\([0123456789]*\\))" line)))
            (when success
              (let ((file (concat (file-name-as-directory gobra-development-path) (replace-regexp-in-string "\\." "/" (match-string 1 line)) ".scala"))
                    (l (string-to-number (match-string 2 line))))
                (when (and file l (file-exists-p file))
                  (let ((buf (find-file-other-window file)))
                    (pop-to-buffer buf)
                    (goto-char (point-min))
                    (setq gobra-is-verified 2)
                    (forward-line (1- l))))))))))))

(defvar gobra-output-mode-map nil "Keymap for gobra-construct-args.")

(when (not gobra-output-mode-map)
  (setq gobra-output-mode-map (make-sparse-keymap))
  (define-key gobra-output-mode-map (kbd "n") 'gobra-output-next-error)
  (define-key gobra-output-mode-map (kbd "p") 'gobra-output-prev-error)
  (define-key gobra-output-mode-map (kbd "RET") 'gobra-output-goto-error))

(defface gobra-output-error-face
  '((t (:foreground "Red")))
  "The face used to highlight errors in Gobra output.")

(defface gobra-output-file-face
  '((t (:foreground "Orange")))
  "The face used to highlight files in Gobra output.")

(defface gobra-output-time-face
  '((t (:foreground "Green")))
  "The face used to highlight time in Gobra output.")

(setq gobra-output-buffer-highlights
      '(("ERROR" . ''gobra-output-error-face)
        ("Gobra has found \\([123456789][0123456789]* error(s)\\)" 1 ''gobra-output-error-face)
        ("Gobra has found \\(0 error(s)\\)" 1 ''gobra-output-time-face)
        ("<\\(.*:[0123456789]*:[0123456789]*\\)>" 1 ''gobra-output-file-face)
        ("<.*:[0123456789]*:[0123456789]*>\\(.*\\)" 1 ''gobra-output-error-face)
        ("^\\(real\\|user\\|sys\\).*s" 0 ''gobra-output-time-face)
        ("^\\(Gobra command:\\) .*s" 1 ''gobra-output-time-face)))

(define-derived-mode gobra-output-mode shell-mode
  "gobra-output mode"
  "Major mode for interacting with gobra output"
  :interactive nil
  (setq font-lock-defaults '(gobra-output-buffer-highlights))
  (hl-line-mode)
  (use-local-map gobra-output-mode-map))

;; add .gobra files to auto-mode-alist and provide package

;;;###autoload

(add-to-list 'auto-mode-alist '("\\.gobra$" . gobra-mode))

(provide 'gobra-mode)
;;; gobra-mode.el ends here
