;;; gobra-params.el --- Parameters for gobra mode -*- lexical-binding: t -*-

;; Copyright (c) 2022- ETH Zurich.

;; Keywords: lisp
;; Version: 0.0.1
;; URL: https://github.com/viperproject/gobra-mode
;; Package-Requires: ((emacs "26.2"))

;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

;;; Commentary:

;; The parameters of the gobra executable

;;; Code:

(require 'param-config)

;; Temporary
(setq-local gobra-mode-config '())

(defvar gobra-params
            `(,(make-param-config-param
                :name "assumeInjectivityOnInhale"
                :doc "Assumes injectivity of the receiver expression when inhaling quantified permissions, instead of checking it, like in Viper versions previous to 2022.02 (default)")
              ,(make-param-config-param
                :name "noassumeInjectivityOnInhale"
                :doc "Does not assume injectivity on inhales (this will become the default in future versions)")
              ,(make-param-config-param
                :name "backend"
                :doc "Needs <arg>. Specifies the used Viper backend. The default is SILICON. Choices: SILICON, CARBON, VSWITHSILICON, VSWITHCARBON"
                :args t
                :getter (lambda ()
                          (completing-read "Backend: "
                                           '("SILICON" "CARBON" "VSWITHSILICON" "VSWITHCARBON") nil t)))))
(setq-local params
            `(,(make-param-config-param
                :name "assumeInjectivityOnInhale"
                :doc "Assumes injectivity of the receiver expression when inhaling quantified permissions, instead of checking it, like in Viper versions previous to 2022.02 (default)")
              ,(make-param-config-param
                :name "noassumeInjectivityOnInhale"
                :doc "Does not assume injectivity on inhales (this will become the default in future versions)")
              ,(make-param-config-param
                :name "backend"
                :doc "Needs <arg>. Specifies the used Viper backend. The default is SILICON. Choices: SILICON, CARBON, VSWITHSILICON, VSWITHCARBON"
                :args t
                :getter (lambda ()
                          (completing-read "Backend: "
                                           '("SILICON" "CARBON" "VSWITHSILICON" "VSWITHCARBON") nil t)))
              ,(make-param-config-param
                :name "boogieExe"
                :doc "Needs <arg>. The Boogie executable"
                :args t
                :getter (lambda ()
                          (read-file-name "Boogie Exe path: ")))
              ,(make-param-config-param
                :name "cacheFile"
                :doc "Needs <arg>. Cache file to be used by Viper Server"
                :args t
                :getter (lambda ()
                          (read-file-name "Cache file: ")))
              ,(make-param-config-param
                :name "checkConsistency"
                :doc "Perform consistency checks on the generated Viper code")
              ,(make-param-config-param
                :name "chop"
                :doc "Needs <arg>. Number of parts the generated verification condition is split into (at most)"
                :args t
                :getter (lambda ()
                          (call-interactively
                           (lambda (arg)
                             "dummy docstring"
                             (interactive "nNumber of parts: ")
                             arg))))
              ,(make-param-config-param
                :name "debug"
                :doc "Output additional debug information")
              ,(make-param-config-param
                :name "disableMoreCompleteExhale"
                :doc "Disables the flag --enableMoreCompleteExhale passed by default to Silicon")
              ,(make-param-config-param
                :name "enableLazyImport"
                :doc "Enforces that Gobra parses depending packages only when necessary. Note that this disables certain language features such as global variables.")
              ,(make-param-config-param
                :name "directory"
                :doc "Needs <arg...>. List of directories to verify"
                :args t
                :getter (lambda ()
                          (read-directory-name "Directory: "))
                :repeating t)
              ,(make-param-config-param
                :name "eraseGhost"
                :doc "Print the input program without ghost code")
              ,(make-param-config-param
                :name "excludePackages"
                :doc "Needs <arg...>. Packages to ignore. These packages will not be verified, even if they are found in the specified directories."
                :args t
                :getter (lambda ()
                          (read-string "Package: "))
                :repeating t)
              ,(make-param-config-param
                :name "gobraDirectory"
                :doc "Needs <arg>. Output directory for Gobra"
                :args t
                :getter (lambda ()
                          (read-directory-name "Gobra output directory: ")))
              ,(make-param-config-param
                :name "goify"
                :doc "Print the input program with the ghost code commented out")
              ,(make-param-config-param
                :name "include"
                :doc "Needs <arg...>. Uses the provided directories to perform package-related lookups before falling back to $GOPATH"
                :args t
                :getter (lambda ()
                          (read-directory-name "Directory: "))
                :repeating t)
              ,(make-param-config-param
                :name "includePackages"
                :doc "Needs <arg...>. Packages to verify. All packages found in the specified directories are verified by default."
                :args t
                :getter (lambda ()
                          (read-string "Package: "))
                :repeating t)
              ,(make-param-config-param
                :name "input"
                :doc "Needs <arg...>. List of files to verify. Optionally, specific members can be verified by passing their line numbers (e.g. foo.gobra@42,111 corresponds to the members in lines 42 and 111) "
                :args t
                :getter (lambda ()
                          (let ((s (read-file-name "File: ")))
                            (while (y-or-n-p "Enter more files? ")
                              (setq s (concat s " " (read-file-name "File: "))))
                            s))
                :repeating t)
              ,(make-param-config-param
                :name "int32"
                :doc "Run with 32-bit sized integers (the default is 64-bit ints)")
              ,(make-param-config-param
                :name "logLevel"
                :doc "Needs <arg>. Specifies the log level. The default is OFF. Choices: ALL, TRACE, DEBUG, INFO, WARN, ERROR, OFF"
                :args t
                :getter (lambda ()
                          (completing-read "Log level: "
                                           '("ALL" "TRACE" "DEBUG" "INFO" "WARN" "ERROR" "OFF") nil t)))
              ,(make-param-config-param
                :name "module"
                :doc "Needs <arg>. Name of current module that should be used for resolving imports"
                :args t
                :getter (lambda ()
                          (read-string "Module: ")))
              ,(make-param-config-param
                :name "noStreamErrors"
                :doc "Do not stream errors produced by Gobra but instead print them all organized by package in the end.")
              ,(make-param-config-param
                :name "noVerify"
                :doc "Skip the verification step performed after encoding the Gobra program into Viper.")
              ,(make-param-config-param
                :name "onlyFilesWithHeader"
                :doc "When enabled, Gobra only looks at files that contain the header comment '// +gobra'")
              ,(make-param-config-param
                :name "overflow"
                :doc "Find expressions that may lead to integer overflow")
              ,(make-param-config-param
                :name "packageTimeout"
                :doc "Needs <arg>. Duration till the verification of a package times out"
                :args t
                :getter (lambda ()
                          (call-interactively
                           (lambda (arg)
                             "dummy docstring"
                             (interactive "sPackage timeout: ") arg))))
              ,(make-param-config-param
                :name "parallelizeBranches"
                :doc "Performs parallel branch verification if the chosen backend is either SILICON or VSWITHSILICON")
              ,(make-param-config-param
                :name "parseOnly"
                :doc "Perform only the parsing step")
              ,(make-param-config-param
                :name "printInternal"
                :doc "Print the internal program representation")
              ,(make-param-config-param
                :name "printVpr"
                :doc "Print the encoded Viper program")
              ,(make-param-config-param
                :name "projectRoot"
                :doc "Needs <arg>. The root directory of the project"
                :args t
                :getter (lambda ()
                          (read-file-name "Project root: ")))
              ,(make-param-config-param
                :name "recursive"
                :doc "Verify nested packages recursively")
              ,(make-param-config-param
                :name "unparse"
                :doc "Print the parsed program")
              ,(make-param-config-param
                :name "z3Exe"
                :doc "Needs <arg>. The Z3 executable"
                :args t
                :getter (lambda ()
                          (read-file-name "Z3 Exe path: ")))
              ,(make-param-config-param
                :name "help"
                :doc "Show help message")
              ,(make-param-config-param
                :name "version"
                :doc "Show version of this program")))

(provide 'gobra-params)
;;; gobra-params.el ends here
