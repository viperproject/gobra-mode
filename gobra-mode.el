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
(defvar-local gobra-async-buffer nil "Keeps the async buffer in which gobra runs.")
(defvar-local gobra-buffer nil "The buffer for which gobra runs.")
(defvar-local gobra-highlight-overlays nil "Keeps the highlight overlays of errors.")
(defvar-local gobra-is-verified nil "Keeps the status of the program regarding the verification.\nIt is nil if the verification hasn't ran, 1 if the program is verified and 2 if it has failed to verify.")


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
          (cons start (point)))))))

(defun gobra-parse-error (data)
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
            (setq-local gobra-is-verified 2)))))))

(defun gobra-verify ()
  (interactive)
  (setq-local gobra-buffer (current-buffer))
  (setenv "Z3_EXE" gobra-z3-path)
  (let ((b (format "%s" (async-shell-command (format "java -jar -Xss128m %s -i %s" gobra-jar-path (buffer-file-name))))))
    (string-match "window [1234567890]* on \\(.*\\)>" b)
    (setq-local gobra-async-buffer (match-string 1 b))
    (let ((gb (current-buffer)))
      (with-current-buffer gobra-async-buffer
        (setq-local gobra-buffer gb)))
    (let ((proc (get-buffer-process gobra-async-buffer)))
      (when (process-live-p proc)
        (set-process-sentinel proc #'gobra-read-sentinel)))))

(defun gobra-mode-line ()
  (if (equal major-mode 'gobra-mode)
      (if (not gobra-is-verified)
          (concat "[" (propertize "Unknown" 'face 'gobra-notran-face) "]")
        (if (equal gobra-is-verified 1)
            (concat "[" (propertize "Verified" 'face 'gobra-verified-face) "]")
          (concat "[" (propertize "Unverified" 'face 'gobra-unverified-face) "]")))
    ""))

;;;###autoload

(defvar gobra-mode-map nil "Keymap for gobra-mode.")

(when (not gobra-mode-map)
  (setq gobra-mode-map (make-sparse-keymap))
  (define-key gobra-mode-map (kbd "C-c C-v") 'gobra-verify))

(define-derived-mode gobra-mode go-mode
  "gobra mode"
  "Major mode for editing Go programs verified by Gobra"
  (cursor-sensor-mode)
  (setq global-mode-string (or global-mode-string '("")))
  (unless (member '(:eval (gobra-mode-line)) global-mode-string)
    (setq global-mode-string (append global-mode-string '((:eval (gobra-mode-line)))))))

(add-to-list 'auto-mode-alist '("\\.gobra" . gobra-mode))

(provide 'gobra-mode)
;;; gobra-mode.el ends here
