;;; gobra-mode.el --- Syntax highlighting for Viper -*- lexical-binding: t -*-

;; Copyright (C) 2021 Dionisios Spiliopoulos

;; Author: Dionisios Spiliopoulos <dennisspiliopoylos@gmail.com>
;; Keywords: lisp
;; Version: 0.0.1
;; URL: https://github.com/Dspil/gobra-mode
;; Package-Requires: ((emacs "26.2") (request "20211107.1907"))

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
(setq-local gobra-async-buffer nil)
(setq-default gobra-async-buffer nil)
(setq-local gobra-buffer nil)

(setq-local gobra-highlight-overlays nil)
(setq-default gobra-highlight-overlays nil)

(setq gobra-jar-path nil)
(setq gobra-z3-path nil)

;; faces

(defgroup gobra-faces nil
  "Gobra highlight faces.")

(defface gobra-error
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "Red1"))
    (t
     :underline t :inherit error))
  "Gobra face for errors."
  :group 'gobra-faces)

;; logic

(defun gobra-extract-num-errors (s)
  (string-match ".* - Gobra has found \\([0123456789]*\\) error(s):" s)
  (let ((num (match-string 1 s)))
    (if num
        (string-to-number num)
      0)))

(defun gobra-find-start-end (err)
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
          (message "FUCKING start: %s and FUCKING end: %s" start (point))
          (cons start (point)))))))

(defun gobra-parse-error (data)
  (let ((err (car data))
        (text (nth 1 data)))
    (with-current-buffer gobra-buffer
      (message "Error starting at: %s with text %s" err text)
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
  (with-current-buffer gobra-async-buffer
    (let ((out (buffer-string)))
      (let* ((splitted (split-string out "\n"))
             (useful (cdr (cdr splitted)))
             (numerrors (gobra-extract-num-errors (car useful))))
        (seq-do #'delete-overlay gobra-highlight-overlays)  
        (if (equal numerrors 0)
            (message "Program verified succesfully!")
          (gobra-parse-error (cdr useful)))))))

(defun gobra-verify ()
  (interactive)
  (setq gobra-buffer (current-buffer))
  (setenv "Z3_EXE" gobra-z3-path)
  (setq b (format "%s" (async-shell-command (format "java -jar -Xss128m %s -i %s" gobra-jar-path (buffer-file-name)))))
  (string-match "window [1234567890]* on \\(.*\\)>" b)
  (setq gobra-async-buffer (match-string 1 b))
  (let ((proc (get-buffer-process gobra-async-buffer)))
    (when (process-live-p proc)
      (set-process-sentinel proc #'gobra-read-sentinel))))

;;;###autoload

(defvar gobra-mode-map nil "Keymap for gobra-mode.")

(when (not gobra-mode-map)
  (setq gobra-mode-map (make-sparse-keymap))
  (define-key gobra-mode-map (kbd "C-c C-v") 'gobra-verify))

(define-derived-mode gobra-mode go-mode
  "gobra mode"
  "Major mode for editing Go programs verified by Gobra"
  (cursor-sensor-mode))

(add-to-list 'auto-mode-alist '("\\.gobra" . gobra-mode))

(provide 'gobra-mode)
;;; gobra-mode.el ends here
