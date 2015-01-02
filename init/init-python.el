;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author: Anton Strilchuk <anton@ilyfa.cc>                         ;;
;; URL: http://ype.env.sh                                           ;;
;; Created: 23-07-2014                                              ;;
;;; Last-Updated: 22-12-2014                                       ;;;
;;;  Update #: 93                                                  ;;;
;;;   By: Anton Strilchuk <anton@env.sh>                           ;;;
;;                                                                  ;;
;; Filename: init-python                                            ;;
;; Version:                                                         ;;
;; Description:                                                     ;;
;;                                                                  ;;
;; Package Requires: ()                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
(require 'python)
(require-package 'elpy)
(require-package 'python-environment)
(require-package 'jedi)
(require-package 'nose)
(require-package 'pydoc-info)
(require-package 'python-info)

(when (require 'elpy nil t)
  (elpy-enable))

;; enable flycheck instead of flymake
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; disable some elpy modes
(setq elpy-modules (delq 'elpy-module-highlight-indentation elpy-modules))

;; set default virtualenv
(add-hook 'python-mode-hook (lambda () (setq pyvenv-workon "ypesci")))

;; Jedi
(setq jedi:complete t
      jedi:complete-on-dot t)
(add-hook 'python-mode-hook 'jedi:setup)

;; Nose
(add-hook 'python-mode-hook (lambda () (require 'nose) (nose-mode t)))

;;+=============================+;;
;;| EIN: Emacs iPython Notebook |;;
;;+=============================+;;
(require-package 'ein)
(setq ein:use-auto-complete t)
(setq ein:use-smartrep nil)

;; Pip-requirements
(require-package 'pip-requirements)
(add-hook 'pip-requirements-mode-hook #'pip-requirements-auto-complete-setup)

;; Add Python Library Documentation to Emacs Info
(add-to-list 'Info-default-directory-list "~/Dev/py/info-docs")

(provide 'init-python)
;;;------------------------------------------------------------------------------
;;; init-python.el ends here
