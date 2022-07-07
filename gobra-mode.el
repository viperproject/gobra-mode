;;; gobra-mode.el --- Syntax highlighting for Viper -*- lexical-binding: t -*-

;; Copyright (C) 2021 Dionisios Spiliopoulos

;; Author: Dionisios Spiliopoulos <dennisspiliopoylos@gmail.com>
;; Keywords: lisp
;; Version: 0.0.1
;; URL: https://github.com/Dspil/gobra-mode
;; Package-Requires: ((emacs "26.2"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Defines syntax highlighting for the
;; Viper language.

;;; Code:
(defvar-local gobra-async-buffer nil "Keeps the async buffer in which gobra runs.")
(defvar-local gobra-buffer nil "The buffer for which gobra runs.")
(defvar-local gobra-highlight-overlays nil "Keeps the highlight overlays of errors.")
(defvar-local gobra-is-verified nil "Keeps the status of the program regarding the verification.\nIt is nil if the verification hasn't ran, 1 if the program is verified and 2 if it has failed to verify.")
(defvar-local gobra-additional-arguments "" "Stores any additional arguments passed to gobra.")
(defvar gobra-z3-path nil "Holds the path to Z3 binary.")
(defvar gobra-jar-path nil "Holds the path to gobra jar file.")

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

;; logic

(defun gobra-extract-num-errors (s)
  "Find number of errors reported by gobra in S."
  (string-match ".* - Gobra has found \\([0123456789]*\\) error(s).*" s)
  (let ((num (match-string 1 s)))
    (if num
        (string-to-number num)
      0)))

(defun gobra-find-start-end (err)
  "Find start and end position of ERR."
  (string-match ".*<.*:\\([0123456789]*\\):\\([0123456789]*\\)>.*" err)
  (let ((l (string-to-number (match-string 1 err)))
        (c (string-to-number (match-string 2 err))))
    (with-current-buffer gobra-buffer
      (save-excursion
        (beginning-of-buffer)
        (forward-line (1- l))
        (forward-char (1- c))
        (let ((start (point)))
          (end-of-line)
          (cons start (point)))))))

(defun gobra-parse-error (data)
  "Parse a gobra output line error in DATA."
  (let ((err (car data))
        (text (nth 1 data)))
    (with-current-buffer gobra-buffer
      (let* ((startend (gobra-find-start-end err))
             (start (car startend))
             (end (cdr startend)))
        (let ((ov (make-overlay
                   start
                   end)))
          (push ov gobra-highlight-overlays)
          (overlay-put ov 'face 'gobra-error)
          (overlay-put ov 'help-echo text)
          (overlay-put ov
                       'cursor-sensor-functions
                       (list
                        (lambda (window pos action)
                          (when (eq action 'entered)
                            (message "%s" text)))))
          (message "%s" text))))))

(defun gobra-read-sentinel (proc signal)
  "Sentinel waiting for async process PROC of gobra to finish verification with SIGNAL."
  (with-current-buffer gobra-async-buffer
    (let ((out (buffer-string)))
      (let* ((splitted (split-string out "\n"))
             (useful (cdr (cdr (cdr splitted))))
             (numerrors (gobra-extract-num-errors (car useful))))
        (message "NUM ERRORS: %s" numerrors)
        (with-current-buffer gobra-buffer
          (seq-do #'delete-overlay gobra-highlight-overlays))
        (if (equal numerrors 0)
            (progn
              (message "Program verified succesfully!")
              (with-current-buffer gobra-buffer
                (setq-local gobra-is-verified 1)))
          (gobra-parse-error (cdr useful))
          (with-current-buffer gobra-buffer
            (setq-local gobra-is-verified 2)))))))

(defun gobra-goify-sentinel (proc signal)
  "Sentinel waiting for async process PROC of gobra to finish goifying with SIGNAL."
  (with-current-buffer gobra-async-buffer
    (let ((out (buffer-string)))
      (let* ((splitted (split-string out "\n"))
             (useful (cdr (cdr splitted)))
             (numerrors (gobra-extract-num-errors (car useful))))
        (with-current-buffer gobra-buffer
          (seq-do #'delete-overlay gobra-highlight-overlays))
        (if (equal numerrors 0)
            (progn
              (message "Program verified succesfully!")
              (with-current-buffer gobra-buffer
                (setq-local gobra-is-verified 1)))
          (gobra-parse-error (cdr useful))
          (with-current-buffer gobra-buffer
            (setq-local gobra-is-verified 2))))))
  (with-current-buffer gobra-buffer
    (find-file-other-window (concat (buffer-file-name) ".go"))))

(defun gobra-printvpr-sentinel (proc signal)
  "Sentinel waiting for async process PROC of gobra to finish the production of vpr code with SIGNAL."
  (with-current-buffer gobra-async-buffer
    (let ((out (buffer-string)))
      (let* ((splitted (split-string out "\n"))
             (useful (cdr (cdr splitted)))
             (numerrors (gobra-extract-num-errors (car useful))))
        (with-current-buffer gobra-buffer
          (seq-do #'delete-overlay gobra-highlight-overlays))
        (if (equal numerrors 0)
            (progn
              (message "Program verified succesfully!")
              (with-current-buffer gobra-buffer
                (setq-local gobra-is-verified 1)))
          (gobra-parse-error (cdr useful))
          (with-current-buffer gobra-buffer
            (setq-local gobra-is-verified 2))))))
  (with-current-buffer gobra-buffer
    (find-file-other-window (concat (buffer-file-name) ".vpr"))
    (viperlanguage-mode)))

(defun gobra-verify ()
  "Verify current buffer."
  (interactive)
  (setq-local gobra-buffer (current-buffer))
  (when gobra-z3-path
    (setenv "Z3_EXE" gobra-z3-path))
  (let ((b (format "%s" (async-shell-command (format "java -jar -Xss128m %s %s" gobra-jar-path (gobra-args-serialize))))))
    (string-match "window [1234567890]* on \\(.*\\)>" b)
    (setq-local gobra-async-buffer (match-string 1 b))
    (setq-local gobra-is-verified 3)
    (let ((gb (current-buffer)))
      (with-current-buffer gobra-async-buffer
        (setq-local gobra-buffer gb)))
    (let ((proc (get-buffer-process gobra-async-buffer)))
      (when (process-live-p proc)
        (set-process-sentinel proc #'gobra-read-sentinel)))))

(defun gobra-goify ()
  "Goify current buffer."
  (interactive)
  (setq-local gobra-buffer (current-buffer))
  (setenv "Z3_EXE" gobra-z3-path)
  (let ((b (format "%s" (async-shell-command (format "java -jar -Xss128m %s --goify -i %s" gobra-jar-path (buffer-file-name))))))
    (string-match "window [1234567890]* on \\(.*\\)>" b)
    (setq-local gobra-async-buffer (match-string 1 b))
    (setq-local gobra-is-verified 3)
    (let ((gb (current-buffer)))
      (with-current-buffer gobra-async-buffer
        (setq-local gobra-buffer gb)))
    (let ((proc (get-buffer-process gobra-async-buffer)))
      (when (process-live-p proc)
        (set-process-sentinel proc #'gobra-goify-sentinel)))))

(defun gobra-printvpr ()
  "Goify current buffer."
  (interactive)
  (setq-local gobra-buffer (current-buffer))
  (setenv "Z3_EXE" gobra-z3-path)
  (let* ((extra-arg (if (not (member "printVpr" gobra-args-set))
                        " --printVpr "
                      ""))
         (b (format "%s" (async-shell-command (format "java -jar -Xss128m %s %s %s" gobra-jar-path extra-arg (gobra-args-serialize))))))
    (string-match "window [1234567890]* on \\(.*\\)>" b)
    (setq-local gobra-async-buffer (match-string 1 b))
    (setq-local gobra-is-verified 3)
    (let ((gb (current-buffer)))
      (with-current-buffer gobra-async-buffer
        (setq-local gobra-buffer gb)))
    (let ((proc (get-buffer-process gobra-async-buffer)))
      (when (process-live-p proc)
        (set-process-sentinel proc #'gobra-printvpr-sentinel)))))

(defun gobra-mode-line ()
  "Return the mode line string."
  (if (equal major-mode 'gobra-mode)
      (if (not gobra-is-verified)
          (concat "[" (propertize "Unknown" 'face 'gobra-notran-face) "]")
        (if (equal gobra-is-verified 1)
            (concat "[" (propertize "Verified" 'face 'gobra-verified-face) "]")
          (if (equal gobra-is-verified 2)
              (concat "[" (propertize "Unverified" 'face 'gobra-unverified-face) "]")
            (concat "[" (propertize "Verifying..." 'face 'gobra-notran-face) "]"))))
    ""))

(defvar gobra-mode-map nil "Keymap for gobra-mode.")
(defvar-local gobra-args-set nil "Arguments set for gobra executable.")
(defvar-local gobra-args-of-args nil "Arguments of gobra arguments.")

(when (not gobra-mode-map)
  (setq gobra-mode-map (make-sparse-keymap))
  (define-key gobra-mode-map (kbd "C-c C-v") 'gobra-verify)
  (define-key gobra-mode-map (kbd "C-c C-c") 'gobra-printvpr)
  (define-key gobra-mode-map (kbd "C-c C-a") 'gobra-edit-args))

(define-derived-mode gobra-mode go-mode
  "gobra mode"
  "Major mode for editing Go programs verified by Gobra"
  (cursor-sensor-mode)
  (setq global-mode-string (or global-mode-string '("")))
  (font-lock-add-keywords nil
                          '(;
                            ("invariant\\|requires\\|ensures\\|preserves\\|trusted\\|pred\\|pure\\|forall\\|exists\\|assume\\|inhale\\|exhale\\|assert\\|ghost\\|implements\\|unfolding\\|fold\\|unfold\\|decreases" (0 font-lock-builtin-face))))
  (unless (member '(:eval (gobra-mode-line)) global-mode-string)
    (setq global-mode-string (append global-mode-string '((:eval (gobra-mode-line))))))
  (gobra-args-initialize))

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
    ("onlyFilesWithHeader" . "When enabled, Gobra only looks at files that contain the header comment '// +gobra'")
    ("overflow" . "Find expressions that may lead to integer overflow")
    ("packageTimeout" . "Needs <arg>. Duration till the verification of a package times out")
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
                             (interactive "nPackage timeout: ") arg))))
    ("projectRoot" . (lambda ()
                       (read-file-name "Project root: ")))
    ("z3Exe" . (lambda ()
                 (read-file-name "Z3 Exe path: "))))
  "Saves all gobra arguments that need arguments and a way to get their values.")

(defun gobra-args-get-multiple (inputf prompt &optional esc)
  (if (not esc)
      (if (y-or-n-p "More directories? "))))

(defvar-local gobra-args-original-buffer nil "Holds the name of the gobra file that corresponds to a gobra arguments construction buffer.")

(defun gobra-args-initialize ()
  "Set the initial value of set of arguments and arguments of arguments variables."
  (setq-local gobra-args-set (cons "input" gobra-args-set))
  (setq-local gobra-args-of-args (cons (cons "input" (buffer-file-name (current-buffer))) gobra-args-of-args)))

(defun gobra-args-serialize ()
  "Return the arguments string."
  (let ((i gobra-args-set)
        (s ""))
    (while i
      (let ((cur (car i))
            (next (cdr i)))
        (setq s (format "%s --%s" s cur))
        (when (assoc cur gobra-args-that-need-args)
          (setq s (format "%s %s" s (cdr (assoc cur gobra-args-of-args)))))
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
  (interactive)
  (let ((f (read-file-name "File to save configuration: ")))
    (gobra-dump (cons gobra-args-set gobra-args-of-args) f)))

(defun gobra-args-load ()
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
  (insert "Gobra argument selection buffer.\nCheck any argument needed with 'c'.\nPrint documentation of argument with 'd'.\nSave configuraton with 's'.\nLoad configuration with 'l'.\nPress 'q' to exit.\n\n")
  (let ((i gobra-args-doc))
    (while i
      (let ((cur (car i))
            (next (cdr i)))
        (insert-char ?\[)
        (if (member (car cur) gobra-args-set)
            (insert-char ?X)
          (insert-char ? ))
        (insert "] ")
        (insert (car cur))
        (when (and (member (car cur) gobra-args-set) (assoc (car cur) gobra-args-that-need-args))
          (insert (format ": %s" (cdr (assoc (car cur) gobra-args-of-args)))))
        (insert-char ?\n)
        (setq i next))))
  (setq-local buffer-read-only t))

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
    (forward-char 4)
    (let ((s (point)))
      (while (and (not (equal (char-after) ?\n)) (not (equal (char-after) ?:)) (not (eobp)))
        (forward-char))
      (when (equal (char-after) ?:)
        (delete-char 1)
        (let ((s1 (point)))
          (while (and (not (equal (char-after) ?\n)) (not (eobp)))
            (forward-char))
          (cons s1 (point)))))))

(defun gobra-args-remove-arg (arg)
  "Remove argument ARG from the argument list."
  (setq-local gobra-args-set (delete arg gobra-args-set))
  (setq-local gobra-args-of-args (assoc-delete-all arg gobra-args-of-args))
  (let ((r (gobra-args-region-after-colon)))
    (delete-region (car r) (cdr r)))
  (gobra-args-transfer))

(defun gobra-args-get-arg ()
  "Return the argument text contained in a line of the args construction buffer."
  (save-excursion
    (beginning-of-line)
    (forward-char 4)
    (let ((s (point)))
      (while (and (not (equal (char-after) ?\n)) (not (equal (char-after) ?:)) (not (eobp)))
        (forward-char))
      (buffer-substring s (point)))))

(defun gobra-args-print-doc ()
  "Print the documentation of the argument under point."
  (interactive)
  (message "%s" (cdr (assoc (gobra-args-get-arg) gobra-args-doc 'equal))))

(defun gobra-args-check-uncheck-arg ()
  "Toggle the appearance of the argument in the current line of the construction buffer in the argument list."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (forward-char)
    (setq-local buffer-read-only nil)
    (if (eq (char-after) ? )
        (progn
          (delete-char 1)
          (insert-char ?X)
          (forward-char 2)
          (let ((arg (gobra-args-get-arg)))
            (gobra-args-add-arg arg)
            (when (assoc arg gobra-args-that-need-args)
              (let ((arg-of-arg (funcall (cdr (assoc arg gobra-args-that-need-args)))))
                (setq-local gobra-args-of-args (cons (cons arg arg-of-arg) (assoc-delete-all arg gobra-args-of-args)))
                (let ((r (gobra-args-region-after-colon)))
                  (when r
                    (delete-region (car r) (cdr r))))
                (end-of-line)
                (insert (format ": %s" arg-of-arg)))))
          (gobra-args-transfer))
      (delete-char 1)
      (insert-char ? )
      (forward-char 2)
      (gobra-args-remove-arg (gobra-args-get-arg)))
    (setq-local buffer-read-only t)))

(defun gobra-args-quit ()
  "Quit the arguments construction buffer."
  (interactive)
  (let ((og gobra-args-original-buffer))
    (kill-buffer)
    (message "%s" og)
    (pop-to-buffer og)))

(defvar gobra-args-mode-map nil "Keymap for gobra-args.")

(when (not gobra-args-mode-map)
  (setq gobra-args-mode-map (make-sparse-keymap))
  (define-key gobra-args-mode-map (kbd "n") 'next-line)
  (define-key gobra-args-mode-map (kbd "p") 'previous-line)
  (define-key gobra-args-mode-map (kbd "c") 'gobra-args-check-uncheck-arg)
  (define-key gobra-args-mode-map (kbd "d") 'gobra-args-print-doc)
  (define-key gobra-args-mode-map (kbd "q") 'gobra-args-quit)
  (define-key gobra-args-mode-map (kbd "s") 'gobra-args-save)
  (define-key gobra-args-mode-map (kbd "l") 'gobra-args-load))

(define-derived-mode gobra-args-mode fundamental-mode
  "gobra-args mode"
  "Major mode for selecting arguments passed to gobra in a construction buffer"
  (use-local-map gobra-args-mode-map)
  (read-only-mode t))

;; add .gobra files to auto-mode-alist and provide package

;;;###autoload

(add-to-list 'auto-mode-alist '("\\.gobra" . gobra-mode))

(provide 'gobra-mode)
;;; gobra-mode.el ends here
