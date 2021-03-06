;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author: Anton Strilchuk <anton@env.sh>                           ;;
;; URL: http://ype.env.sh                                           ;;
;; Created: 06-11-2014                                              ;;
;;; Last-Updated: 15-01-2015                                       ;;;
;;;  Update #: 13                                                  ;;;
;;;   By: Anton Strilchuk <anton@env.sh>                           ;;;
;;                                                                  ;;
;; Filename: init-go                                                ;;
;; Version:                                                         ;;
;; Description:                                                     ;;
;;                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setenv "GOPATH" "~/Dev/go")

(require-package 'go-mode)
(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook (lambda () (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)))
(add-hook 'go-mode-hook (lambda () (local-set-key (kbd "C-c C-i") 'go-goto-imports)))

;;GOlint
(add-to-list 'load-path (concat (getenv "GOPATH") "/src/github.com/golang/lint/misc/emacs"))
(require 'golint)

;;Errcheck
(require-package 'go-errcheck)

;;Auto-Complete-GO
(add-to-list 'load-path (concat (getenv "GOPATH") "/src/github.com/nsf/gocode/emacs"))
(require 'go-autocomplete)
(require 'auto-complete-config)

(require-package 'go-eldoc)
(add-hook 'go-mode-hook 'go-eldoc-setup)

;; Org-Mode GOlang
(el-get-bundle pope/ob-go)


(provide 'init-go)
