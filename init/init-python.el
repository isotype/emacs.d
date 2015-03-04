;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author: Anton Strilchuk <anton@ilyfa.cc>                         ;;
;; URL: http://ype.env.sh                                           ;;
;; Created: 23-07-2014                                              ;;
;;; Last-Updated: 27-01-2015                                       ;;;
;;;  Update #: 105                                                 ;;;
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
(require-package 'pyenv-mode)
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
;;(setq elpy-modules (delq 'elpy-module-highlight-indentation elpy-modules))

;; set default virtualenv
;; (add-hook 'python-mode-hook (lambda () (setq pyvenv-workon "default")))
(when (require 'python-environment)
  (setq python-environment-directory "~/.virtualenvs/"
        python-environment-default-root-name "~/.virtualenvs/default"))
;; PyEnvMode
(pyenv-mode)

;; Jedi
(setq jedi:complete t
      jedi:complete-on-dot t
      jedi:environment-virtualenv "~/.virtualenvs/default/bin/"
      jedi:environment-virtualenv (append python-environment-virtualenv
                                          '("--python" "~/.virtualenvs/default/bin/python3")))
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
;; (add-to-list 'Info-default-directory-list "~/Dev/py/info-docs")

;; use pylint for checking
(setq python-check-command "pylint")

;;+===========+;;
;;| AUTO VENV |;;
;;+===========+;;
(require-package 'virtualenvwrapper)
(when (require 'virtualenvwrapper)
  (setq venv-location "~/.virtualenvs/")
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell))

(setq-default mode-line-format (cons '(:exec venv-current-name) mode-line-format))

(add-hook 'venv-postmkvirtualenv-hook
          (lambda () (shell-command "pip install nose pylint jedi")))

(add-hook 'python-mode-hook
          (lambda () (hack-local-variables)
            (when (boundp 'project-venv-name)
              (venv-workon project-venv-name))))

(provide 'init-python)
;;;------------------------------------------------------------------------------
;;; init-python.el ends here
