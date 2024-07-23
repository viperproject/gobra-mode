;;; lsp-gobra.el --- Support for Gobra in Emacs -*- lexical-binding: t -*-

;; Copyright (c) 2022- ETH Zurich.

;; Keywords: lisp
;; Version: 0.0.1
;; URL: https://github.com/viperproject/gobra-mode
;; Package-Requires: ((emacs "26.2"))

;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

;;; Commentary:

;; Add LSP support for gobra

;;; Code:

(require 'lsp-protocol)
(require 'lsp-mode)

(defconst lsp-gobra/status-ok 1)
(defconst lsp-gobra/status-warn 2)
(defconst lsp-gobra/status-error 3)

(defgroup lsp-gobra nil
  "Gobra language server group."
  :group 'lsp-mode
  :tag "Lsp Gobra")

(defcustom lsp-gobra-server nil
  "Path to the gobra-server language server."
  :group 'lsp-gobra
  :risky t
  :type 'file)

(defun lsp-gobra-server-command (port)
  "Create arguments to start gobra language server in TCP mode on PORT."
  `("java" "-jar" "-Xss128m" ,lsp-gobra-server "--port" ,(number-to-string port)))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-tcp-connection 'lsp-gobra-server-command)
  :major-modes '(gobra-mode)
  ;:async-request-handlers (ht ("gobraServer/verify" #'lsp-gobra--confirm-local))
  :server-id 'gobra-server))

(lsp-consistency-check lsp-gobra)

(provide 'lsp-gobra)
;;; lsp-gobra.el ends here
