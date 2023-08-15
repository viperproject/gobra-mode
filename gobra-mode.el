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
(defvar-local gobra-async-buffer nil "Keeps the async buffer in which gobra runs.")
(defvar-local gobra-buffer nil "The buffer for which gobra runs.")
(defvar-local gobra-highlight-overlays nil "Keeps the highlight overlays of errors.")
(defvar-local gobra-is-verified nil "Keeps the status of the program regarding the verification.\nIt is nil if the verification hasn't ran, 1 if the program is verified and 2 if it has failed to verify.")
(defvar-local gobra-additional-arguments "" "Stores any additional arguments passed to gobra.")
(defvar gobra-output-buffer-prelude "echo \"Gobra output buffer actions:\"; echo \"For next error press 'n'\"; echo \"For previous error press 'p'\" ; echo \"For visiting current error press 'RET'\"" "Holds the stuff to print in the Gobra output buffer.")
(defvar gobra-current-async-buffer nil "Holds the buffer where Gobra is currently running.")
(defvar gobra-development-path nil "Holds the path of the gobra development src directory.")
(defvar gobra-z3-path nil "Holds the path to Z3 binary.")
(defvar gobra-jar-path nil "Holds the path to gobra jar file.")
(defvar gobra-actions-before-go-mode (lambda () nil) "Function ran before the go mode hooks kick in.")
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

;; logic

(defun gobra-all-buffers ()
  "Find all .go and .gobra buffers."
  (map 'list
       'car
       (seq-filter
        (lambda (x)
          (when (cdr x)
            (or (string-match ".*\\.gobra$" (cdr x)) (string-match ".*\\.go$" (cdr x)))))
        (map 'list
             (lambda (x)
               (cons x (buffer-file-name x)))
             (buffer-list)))))

(defun gobra-find-gobra-buffer (file)
  "Find the buffer corresponding to FILE."
  (let ((b (seq-filter
            (lambda (x)
              (when (cdr x)
                (equal (cdr x) file)))
            (map 'list
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
  (map 'list
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
            (map 'list
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
                (setq-local gobra-is-verified 2)))))))))

(defun gobra-printvpr-sentinel (proc signal)
  "Sentinel waiting for async process PROC of gobra to finish the production of vpr code with SIGNAL."
  (with-current-buffer (if gobra-async-buffer gobra-async-buffer gobra-current-async-buffer)
    (read-only-mode 1)
    (let ((out (buffer-string)))
      (let ((useful (split-string out "\n")))
        (when useful
          (let (numerrors (gobra-extract-num-errors useful))
            (map 'list
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
                (setq-local gobra-is-verified 2))))))))
  (with-current-buffer gobra-buffer
    (find-file-other-window (concat (buffer-file-name) ".vpr"))
    (viperlanguage-mode)))

(defun gobra-verify ()
  "Verify current buffer."
  (interactive)
  (setq-local gobra-buffer (current-buffer))
  (when gobra-z3-path
    (setenv "Z3_EXE" gobra-z3-path))
  (let* ((cmd (format "java -jar -Xss128m %s %s" gobra-jar-path (gobra-args-serialize)))
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
  (setq-local gobra-buffer (current-buffer))
  (when gobra-z3-path
    (setenv "Z3_EXE" gobra-z3-path))
  (let* ((cmd (format "java -jar -Xss128m %s %s" gobra-jar-path (gobra-args-serialize (cons (buffer-file-name) (line-number-at-pos)))))
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
  (setq-local gobra-buffer (current-buffer))
  (setenv "Z3_EXE" gobra-z3-path)
  (let* ((extra-arg (if (not (member "printVpr" gobra-args-set))
                        " --printVpr "
                      ""))
         (cmd (format "java -jar -Xss128m %s %s" gobra-jar-path (gobra-args-serialize)))
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
          (concat "[" (propertize "Unknown" 'face 'gobra-notran-face) "]")
        (if (equal gobra-is-verified 1)
            (concat "[" (propertize "Verified" 'face 'gobra-verified-face) "]")
          (if (equal gobra-is-verified 2)
              (concat "[" (propertize "Unverified" 'face 'gobra-unverified-face) "]")
            (concat "[" (propertize "Verifying..." 'face 'gobra-notran-face) "]"))))
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
         ov)
    (when region
      (setq ov (make-overlay start end))
      (push ov gobra-ghost-overlays)
      (overlay-put ov 'display (propertize "ghost..." 'face 'font-lock-comment-face))
      (overlay-put ov 'invisible t))))

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

(defvar-local gobra-args-set nil "Arguments set for gobra executable.")
(defvar-local gobra-args-of-args nil "Arguments of gobra arguments.")

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
                         "pred"
                         "pure"
                         "forall"
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


(defhydra gobra-minor-mode-hydra (:hint
                                  nil
                                  :exit
                                  t)
  "
^Verification^         ^Arguments^          ^Folding^          ^Ghost^
^^^^^^^^-----------------------------------------------------------------------------
_v_: verify            _a_: edit args       _h_: fold/unfold   _f_  : format spec
_l_: verify line       _s_: print command   _j_: show all      _n_  : next ghost
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
  ("q" nil "cancel" :color blue))


(define-minor-mode gobra-minor-mode
  "Minor mode for gobra (used primarily in go files)."
  nil
  :lighter " Gobra"
  :keymap (list (cons (kbd "C-c g") 'gobra-minor-mode-hydra/body))
  (cursor-sensor-mode)
  (gobra-args-initialize)
  (font-lock-add-keywords nil (list
                               (cons (concat "\\<" (regexp-opt gobra-keywords) "\\>")
                                     '((0 font-lock-builtin-face)))))
  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings (font-lock-fontify-buffer))))
  (setq global-mode-string (or global-mode-string '("")))
  (unless (member '(:eval (gobra-mode-line)) global-mode-string)
    (setq global-mode-string (append global-mode-string '((:eval (gobra-mode-line)))))))

;; argument selection buffer major mode

(defvar gobra-args-doc
  '(("assumeInjectivityOnInhale" . "Assumes injectivity of the receiver expression when inhaling quantified permissions, instead of checking it, like in Viper versions previous to 2022.02 (default)")
    ("noassumeInjectivityOnInhale" . "Does not assume injectivity on inhales (this will become the default in future versions)")
    ("backend" . "Needs <arg>. Specifies the used Viper backend. The default is SILICON. Choices: SILICON, CARBON, VSWITHSILICON, VSWITHCARBON")
    ("boogieExe" . "Needs <arg>. The Boogie executable")
    ("cacheFile" . "Needs <arg>. Cache file to be used by Viper Server")
    ("checkConsistency" . "Perform consistency checks on the generated Viper code")
    ("chop" . "Needs <arg>. Number of parts the generated verification condition is split into (at most)")
    ("debug" . "Output additional debug information")
    ("disableMoreCompleteExhale" . "Disables the flag --enableMoreCompleteExhale passed by default to Silicon")
    ("enableLazyImport" . "Enforces that Gobra parses depending packages only when necessary. Note that this disables certain language features such as global variables.")
    ("directory" . "Needs <arg...>. List of directories to verify")
    ("eraseGhost" . "Print the input program without ghost code")
    ("excludePackages" . "Needs <arg...>. Packages to ignore. These packages will not be verified, even if they are found in the specified directories.")
    ("gobraDirectory" . "Needs <arg>. Output directory for Gobra")
    ("goify" . "Print the input program with the ghost code commented out")
    ("include" . "Needs <arg...>. Uses the provided directories to perform package-related lookups before falling back to $GOPATH")
    ("includePackages" . "Needs <arg...>. Packages to verify. All packages found in the specified directories are verified by default.")
    ("input" . "Needs <arg...>. List of files to verify. Optionally, specific members can be verified by passing their line numbers (e.g. foo.gobra@42,111 corresponds to the members in lines 42 and 111) ")
    ("int32" . "Run with 32-bit sized integers (the default is 64-bit ints)")
    ("logLevel" . "Needs <arg>. Specifies the log level. The default is OFF. Choices: ALL, TRACE, DEBUG, INFO, WARN, ERROR, OFF")
    ("module" . "Needs <arg>. Name of current module that should be used for resolving imports")
    ("noStreamErrors" . "Do not stream errors produced by Gobra but instead print them all organized by package in the end.")
    ("noVerify" . "Skip the verification step performed after encoding the Gobra program into Viper.")
    ("onlyFilesWithHeader" . "When enabled, Gobra only looks at files that contain the header comment '// +gobra'")
    ("overflow" . "Find expressions that may lead to integer overflow")
    ("packageTimeout" . "Needs <arg>. Duration till the verification of a package times out")
    ("parallelizeBranches" . "Performs parallel branch verification if the chosen backend is either SILICON or VSWITHSILICON")
    ("parseOnly" . "Perform only the parsing step")
    ("printInternal" . "Print the internal program representation")
    ("printVpr" . "Print the encoded Viper program")
    ("projectRoot" . "Needs <arg>. The root directory of the project")
    ("recursive" . "Verify nested packages recursively")
    ("unparse" . "Print the parsed program")
    ("z3Exe" . "Needs <arg>. The Z3 executable")
    ("help" . "Show help message")
    ("version" . "Show version of this program"))
  "Documentation of the gobra arguments.")

(defvar gobra-args-that-need-args
  '(("backend" . (lambda ()
                   (completing-read "Backend: "
                                    '("SILICON" "CARBON" "VSWITHSILICON" "VSWITHCARBON") nil t)))
    ("boogieExe" . (lambda ()
                     (read-file-name "Boogie Exe path: ")))
    ("cacheFile" . (lambda ()
                     (read-file-name "Cache file: ")))
    ("chop" . (lambda ()
                (call-interactively
                 (lambda (arg)
                   "dummy docstring"
                   (interactive "nNumber of parts: ")
                   arg))))
    ("directory" . (lambda ()
                     (let ((s (read-directory-name "Directory: ")))
                       (while (y-or-n-p "Enter more directories? ")
                         (setq s (concat s " " (read-directory-name "Directory: "))))
                       s)))
    ("excludePackages" . (lambda ()
                           (read-string "Packages: ")))
    ("gobraDirectory" . (lambda ()
                          (read-directory-name "Gobra output directory: ")))
    ("include". (lambda ()
                  (let ((s (read-directory-name "Directory: ")))
                    (while (y-or-n-p "Enter more directories? ")
                      (setq s (concat s " " (read-directory-name "Directory: "))))
                    s)))
    ("includePackages" . (lambda ()
                           (read-string "Packages: ")))
    ("input" . (lambda ()
                 (let ((s (read-file-name "File: ")))
                   (while (y-or-n-p "Enter more files? ")
                     (setq s (concat s " " (read-file-name "File: "))))
                   s)))
    ("logLevel" . (lambda ()
                    (completing-read "Log level: "
                                     '("ALL" "TRACE" "DEBUG" "INFO" "WARN" "ERROR" "OFF") nil t)))
    ("module" . (lambda ()
                  (read-string "Module: ")))
    ("packageTimeout" . (lambda ()
                          (call-interactively
                           (lambda (arg)
                             "dummy docstring"
                             (interactive "sPackage timeout: ") arg))))
    ("projectRoot" . (lambda ()
                       (read-file-name "Project root: ")))
    ("z3Exe" . (lambda ()
                 (read-file-name "Z3 Exe path: "))))
  "Saves all gobra arguments that need arguments and a way to get their values.")

(defvar gobra-args-that-need-many-args
  '("directory" "excludePackages" "include" "includePackages" "input")
  "Saves all gobra arguments that need an arbitrary amount of arguments.")

(defvar-local gobra-args-original-buffer nil "Holds the name of the gobra file that corresponds to a gobra arguments construction buffer.")

(defun gobra-args-initialize ()
  "Set the initial value of set of arguments and arguments of arguments variables."
  (when (not (member "input" gobra-args-set))
    (setq-local gobra-args-set (cons "input" gobra-args-set))
    (setq-local gobra-args-of-args (cons (cons "input" (buffer-file-name (current-buffer))) gobra-args-of-args))))

(defun gobra-get-gobra-files (dir)
  "Get all '.go' or '.gobra' files in DIR."
  (let ((files (seq-filter (lambda (f) (and (string-match-p "\\(.+\\.go$\\)\\|\\(.*\\.gobra$\\)" f) (not (file-directory-p f)))) (directory-files dir t))))
    (apply 'concatenate (cons 'string (map 'list (lambda (f) (concat f " ")) files)))))

(defun gobra-transform-args (file-line)
  "Transforms the arguments opening the directories if FILE-LINE is set and leaking everything to input."
  (if file-line
      (progn
        (let ((args-set gobra-args-set)
              (args-of-args gobra-args-of-args))
          (when (member "directory" args-set)
            (when (not (member "input" args-set))
              (setq args-set (cons "input" args-set)))
            (setq args-set (delete "directory" args-set))
            (let ((new-input " ")
                  (dirs (cdr (assoc "directory" args-of-args))))
              (when dirs
                (let ((dirs-list (split-string dirs " ")))
                  (while dirs-list
                    (setq new-input (concat new-input (gobra-get-gobra-files (car dirs-list)) " "))
                    (setq dirs-list (cdr dirs-list))))
                (let ((input-sofar (assoc "input" args-of-args)))
                  (when input-sofar
                    (setq new-input (concat input-sofar new-input))))
                (setq args-of-args (cons (cons "input" new-input) (assoc-delete-all "directory" (assoc-delete-all "input" args-of-args)))))))
          (cons args-set args-of-args)))
    (cons gobra-args-set gobra-args-of-args)))

(defun gobra-insert-file-line (file-line args-of-args)
  "FILE-LINE is a file with a corresponding line which we want to verify.  Insert that line to the corresponding file in ARGS-OF-ARGS."
  (let ((file (car file-line))
        (line (cdr file-line))
        (files (split-string args-of-args " ")))
    (apply 'concat
           (map 'list
                (lambda (f)
                  (if (equal file f)
                      (format "%s@%s " file line)
                    (concat f " ")))
                files))))

(defun gobra-args-serialize (&optional file-line)
  "Return the arguments string.  If the optional argument FILE-LINE is specified as a tuple (filename line), the specific line will be appended to the corresponding file for specific member verification."
  (let* ((transformed (gobra-transform-args file-line))
         (i (car transformed))
         (all-args-of-args (cdr transformed))
         (s ""))
    (while i
      (let ((cur (car i))
            (next (cdr i)))
        (setq s (format "%s --%s" s cur))
        (when (assoc cur gobra-args-that-need-args)
          (let ((args-of-args (cdr (assoc cur all-args-of-args))))
            (if (and (equal cur "input") file-line)
                (setq args-of-args (gobra-insert-file-line file-line args-of-args)))
            (setq s (format "%s %s" s args-of-args))))
        (setq i next)))
    s))

(defun gobra-edit-args ()
  "Spawn the construction buffer for the arguments."
  (interactive)
  (let ((cur-buf (buffer-name))
        (arg-buf (format "%s%s" (current-buffer) "~args"))
        (arg-set gobra-args-set)
        (args-of-args gobra-args-of-args))
    (with-current-buffer (get-buffer-create arg-buf)
      (gobra-args-mode)
      (setq-local gobra-args-original-buffer cur-buf)
      (setq-local gobra-args-set arg-set)
      (setq-local gobra-args-of-args args-of-args)
      (gobra-populate-args-buffer))
    (pop-to-buffer arg-buf)))

(defun gobra-dump (data filename)
  "Dump DATA in the file FILENAME."
  (with-temp-file filename
    (prin1 data (current-buffer))))

(defun gobra-load (filename)
  "Restore data from the file FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (cl-assert (bobp))
    (read (current-buffer))))

(defun gobra-args-save ()
  "Save the current argument configuration to the disk."
  (interactive)
  (let ((f (read-file-name "File to save configuration: ")))
    (gobra-dump (cons gobra-args-set gobra-args-of-args) f)))

(defun gobra-args-load ()
  "Load an argument configuration from the disk."
  (interactive)
  (let* ((f (read-file-name "File name to load configuration: "))
         (data (gobra-load f))
         (args (car data))
         (args-of-args (cdr data)))
    (setq-local gobra-args-set args)
    (setq-local gobra-args-of-args args-of-args)
    (gobra-args-transfer)
    (gobra-populate-args-buffer)))

(defun gobra-populate-args-buffer ()
  "Insert the prelude and arguments wit their values so far in the current buffer."
  (setq-local buffer-read-only nil)
  (erase-buffer)
  (goto-char (point-min))
  (insert "Gobra argument selection buffer.\nCheck any argument needed with 'c'.\nAdd arguments to an argument with 'a'\nEdit an argument of argument with 'e'\nPrint documentation of argument with 'd'.\nSave configuraton with 's'.\nLoad configuration with 'l'.\nPress 'q' to exit.\n\n")
  (let ((start-pos (point)))
    (let ((i gobra-args-doc))
      (while i
        (let ((cur (car i))
              (next (cdr i)))
          (insert-char ?\[)
          (if (member (car cur) gobra-args-set)
              (insert (propertize "X" 'face 'gobra-verified-face))
            (insert-char ? ))
          (insert "] ")
          (insert (car cur))
          (when (and (member (car cur) gobra-args-set) (assoc (car cur) gobra-args-that-need-args))
            (insert (concat ": " (propertize (format "%s" (cdr (assoc (car cur) gobra-args-of-args))) 'face 'gobra-argument-face))))
          (insert-char ?\n)
          (setq i next))))
    (setq-local buffer-read-only t)
    (goto-char start-pos)))

(defun gobra-args-transfer ()
  "Transfer the change to the arguments at the main gobra buffer."
  (let ((args gobra-args-set)
        (args-of-args gobra-args-of-args))
    (with-current-buffer gobra-args-original-buffer
      (setq-local gobra-args-set args)
      (setq-local gobra-args-of-args args-of-args))))

(defun gobra-args-add-arg (arg)
  "Add argument ARG to the argument list."
  (when (not (member arg gobra-args-set))
    (setq-local gobra-args-set (cons arg gobra-args-set))
    (gobra-args-transfer)))

(defun gobra-args-region-after-colon ()
  "Return the beginning and and of the region after ':' in the construction buffer at the current line."
  (save-excursion
    (beginning-of-line)
    (if (equal (char-after) ?\[)
        (progn
          (forward-char 4)
          (let ((s (point)))
            (while (and (not (equal (char-after) ?\n)) (not (equal (char-after) ?:)) (not (eobp)))
              (forward-char))
            (when (equal (char-after) ?:)
              (let ((s1 (point)))
                (while (and (not (equal (char-after) ?\n)) (not (eobp)))
                  (forward-char))
                (cons s1 (point))))))
      nil)))

(defun gobra-args-remove-arg (arg)
  "Remove argument ARG from the argument list."
  (setq-local gobra-args-set (delete arg gobra-args-set))
  (setq-local gobra-args-of-args (assoc-delete-all arg gobra-args-of-args))
  (let ((r (gobra-args-region-after-colon)))
    (when r
      (delete-region (car r) (cdr r))))
  (gobra-args-transfer))

(defun gobra-args-get-arg ()
  "Return the argument text contained in a line of the args construction buffer."
  (save-excursion
    (beginning-of-line)
    (let ((c (char-after)))
      (if c
          (if (eq c ?\[)
              (progn
                (forward-char 4)
                (let ((s (point)))
                  (while (and (not (equal (char-after) ?\n)) (not (equal (char-after) ?:)) (not (eobp)))
                    (forward-char))
                  (buffer-substring s (point))))
            nil)
        nil))))

(defun gobra-args-print-doc ()
  "Print the documentation of the argument under point."
  (interactive)
  (message "%s" (cdr (assoc (gobra-args-get-arg) gobra-args-doc 'equal))))

(defun gobra-args-check-uncheck-arg (&optional append)
  "Toggle the appearance of the argument in the current line of the construction buffer in the argument list.  When APPEND is set to t, args of args are appended to the current arg."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (equal (char-after) ?\[)
      (forward-char)
      (setq-local buffer-read-only nil)
      (if (or (eq (char-after) ? ) append)
          (progn
            (let ((sofar "")
                  (reg (gobra-args-region-after-colon)))
              (delete-char 1)
              (insert (propertize "X" 'face 'gobra-verified-face))
              (forward-char 2)
              (let ((arg (gobra-args-get-arg)))
                (when (and append reg (member arg gobra-args-that-need-many-args))
                  (setq sofar (buffer-substring (1+ (car reg)) (cdr reg))))
                (gobra-args-add-arg arg)
                (when (assoc arg gobra-args-that-need-args)
                  (let ((arg-of-arg (concat (format "%s" (funcall (cdr (assoc arg gobra-args-that-need-args)))) sofar)))
                    (setq-local gobra-args-of-args (cons (cons arg arg-of-arg) (assoc-delete-all arg gobra-args-of-args)))
                    (when reg
                      (delete-region (car reg) (cdr reg)))
                    (end-of-line)
                    (insert (concat ": " (propertize (format "%s" arg-of-arg) 'face 'gobra-argument-face)))))))
            (gobra-args-transfer))
        (delete-char 1)
        (insert-char ? )
        (gobra-args-remove-arg (gobra-args-get-arg))))
    (setq-local buffer-read-only t)))

(defun gobra-args-add-arg-of-arg ()
  "Toggle on the argument in this line and if it already has arguments, add to the existing ones."
  (interactive)
  (gobra-args-check-uncheck-arg t))

(defun gobra-args-find-arg-line (arg)
  "Find the line where ARG is on."
  (save-excursion
    (goto-char (point-min))
    (let ((found nil)
          (line nil))
      (while (and (not found) (not (eobp)))
        (let ((farg (gobra-args-get-arg)))
          (when farg
            (when (equal arg farg)
              (setq line (line-number-at-pos))
              (setq found t)))
          (forward-line)))
      line)))

(defun gobra-args-update-arg-of-arg (arg arg-of-arg)
  "Update the ARG-OF-ARG of ARG."
  (save-excursion
    (let ((line (gobra-args-find-arg-line arg)))
      (when line
        (goto-char (point-min))
        (forward-line (1- line))
        (let ((reg (gobra-args-region-after-colon)))
          (setq-local buffer-read-only nil)
          (when reg
            (delete-region (car reg) (cdr reg)))
          (end-of-line)
          (insert (concat ": " (propertize (format "%s" arg-of-arg) 'face 'gobra-argument-face)))
          (setq-local buffer-read-only nil)))))
  (gobra-args-add-arg arg)
  (setq-local gobra-args-of-args (cons (cons arg arg-of-arg) (assoc-delete-all arg gobra-args-of-args)))
  (gobra-args-transfer))

(defun gobra-args-quit ()
  "Quit the arguments construction buffer."
  (interactive)
  (let ((og gobra-args-original-buffer))
    (kill-buffer)
    (pop-to-buffer og)))

(defvar gobra-args-mode-map nil "Keymap for gobra-args.")

(when (not gobra-args-mode-map)
  (setq gobra-args-mode-map (make-sparse-keymap))
  (define-key gobra-args-mode-map (kbd "n") 'next-line)
  (define-key gobra-args-mode-map (kbd "p") 'previous-line)
  (define-key gobra-args-mode-map (kbd "c") 'gobra-args-check-uncheck-arg)
  (define-key gobra-args-mode-map (kbd "a") 'gobra-args-add-arg-of-arg)
  (define-key gobra-args-mode-map (kbd "d") 'gobra-args-print-doc)
  (define-key gobra-args-mode-map (kbd "q") 'gobra-args-quit)
  (define-key gobra-args-mode-map (kbd "s") 'gobra-args-save)
  (define-key gobra-args-mode-map (kbd "l") 'gobra-args-load)
  (define-key gobra-args-mode-map (kbd "e") 'gobra-construct-args-spawn))

(define-derived-mode gobra-args-mode fundamental-mode
  "gobra-args mode"
  "Major mode for selecting arguments passed to gobra in a construction buffer"
  (use-local-map gobra-args-mode-map)
  (read-only-mode t))

;; major mode for sub construction buffers for args of args
(defvar-local gobra-construct-args-original-buffer nil "Holds the name of the gobra argument buffer.")
(defvar-local gobra-construct-args-arg nil "Name of gobra argument being constructed.")

(defun gobra-construct-args-buffer-name ()
  "Generate a name for the args of args construction buffer."
  (format "*%s~%s*" (buffer-file-name (get-buffer gobra-args-original-buffer)) (gobra-args-get-arg)))

(defun gobra-construct-args-spawn ()
  "Spawn a construction buffer for the args of args for the argument currently hovered on."
  (interactive)
  (let ((arg (gobra-args-get-arg))
        (buf (current-buffer)))
    (when (assoc arg gobra-args-that-need-args)
      (let* ((arg-of-arg (if (assoc arg gobra-args-of-args)
                             (cdr (assoc arg gobra-args-of-args))
                           ""))
             (arg-buf (gobra-construct-args-buffer-name)))
        (with-current-buffer (get-buffer-create arg-buf)
          (gobra-construct-args-mode)
          (setq-local gobra-construct-args-original-buffer buf)
          (setq-local gobra-construct-args-arg arg)
          (erase-buffer)
          (insert (substring-no-properties arg-of-arg)))
        (pop-to-buffer arg-buf)))))

(defun gobra-construct-args-accept ()
  "Accept the change to the current argument."
  (interactive)
  (let ((arg gobra-construct-args-arg)
        (arg-of-arg (buffer-substring-no-properties (point-min) (point-max)))
        (buf gobra-construct-args-original-buffer))
    (message "%s | %s | %s" arg arg-of-arg buf)
    (with-current-buffer buf
      (gobra-args-update-arg-of-arg arg arg-of-arg))
    (kill-buffer (current-buffer))
    (pop-to-buffer buf)))

(defun gobra-construct-args-abort ()
  "Abort the change to the current argument."
  (interactive)
  (let ((buf gobra-construct-args-original-buffer))
    (kill-buffer (current-buffer))
    (pop-to-buffer buf)))

(defvar gobra-construct-args-mode-map nil "Keymap for gobra-construct-args.")

(when (not gobra-construct-args-mode-map)
  (setq gobra-construct-args-mode-map (make-sparse-keymap))
  (define-key gobra-construct-args-mode-map (kbd "C-c C-c") 'gobra-construct-args-accept)
  (define-key gobra-construct-args-mode-map (kbd "C-c C-k") 'gobra-construct-args-abort))

(define-derived-mode gobra-construct-args-mode fundamental-mode
  "gobra-construct-args mode"
  "Major mode for editing arguments passed to gobra in a construction buffer"
  (use-local-map gobra-construct-args-mode-map)
  (setq header-line-format
	  (substitute-command-keys
	   "Edit, then exit with `\\[gobra-construct-args-accept]' or abort with \
`\\[gobra-construct-args-abort]'")))

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
