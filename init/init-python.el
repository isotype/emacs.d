;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author: Anton Strilchuk <anton@ilyfa.cc>                         ;;
;; URL: http://ype.env.sh                                           ;;
;; Created: 23-07-2014                                              ;;
;; Last-Updated: 12-11-2014                                         ;;
;;  Update #: 42                                                    ;;
;;   By: Anton Strilchuk <anton@env.sh>                             ;;
;;                                                                  ;;
;; Filename: init-python                                            ;;
;; Version:                                                         ;;
;; Description:                                                     ;;
;;                                                                  ;;
;; Package Requires: ()                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
;;,-------------------------------------------
;;| Elpy: Emacs Python Development Environment
;;`-------------------------------------------
;;(require-package 'yasnippet)
(require 'python)

(require-package 'virtualenvwrapper)
(setq venv-location "~/.virtualenvs/")
(setq-default mode-line-format (cons '(:exec venv-current-name) mode-line-format))

(require-package 'jedi)
(setq jedi:complete t)
(remove-hook 'python-mode-hook 'wisent-python-default-setup)
(add-hook 'python-mode-hook 'jedi:setup)

(require-package 'pip-requirements)

(require-package 'nose)
(require 'nose)
(add-hook 'python-mode-hook (lambda () (nose-mode t)))




;;,---------------------------------------------------------------------------
;;|  Anaconda: Code navigation, documentation lookup and completion for Python
;;|  Link: [[https://github.com/proofit404/anaconda-mode][proofit404/anaconda-mode]]
;;`---------------------------------------------------------------------------
;; (require-package 'anaconda-mode)

;; (add-hook 'python-mode-hook 'anaconda-mode)
;; (add-hook 'python-mode-hook 'eldoc-mode)

;; ;; [[https://github.com/proofit404/pyenv-mode][pyenv-mode]]
;; (require-package 'pyenv-mode)
;; (defun projectile-pyenv-mode-set ()
;;   "Set pyenv version matching project name.
;; Version must be already installed."
;;   (pyenv-mode-set (projectile-project-name)))

;; (add-hook 'projectile-switch-project-hook 'projectile-pyenv-mode-set)

;; PyFlakes
;;(require-package 'flycheck-pyflakes)
;;(add-hook 'python-mode-hook 'flycheck-mode)
;;(add-to-list 'flycheck-disabled-checkers 'python-flake8)
;;(add-to-list 'flycheck-disabled-checker 'python-pylint)


;;,-----------------------
;;| Emacs iPython Notebook
;;`-----------------------
(require-package 'ein)
(setq ein:use-auto-complete t)
(setq ein:use-smartrep nil)

;; Indentation
;; Ignoring electric indentation
;; (defun electric-indent-ignore-python (char)
;;   "Ignore electric indentation for python-mode"
;;   (if (equal major-mode 'python-mode)
;;       `no-indent'
;;     nil))
;; (add-hook 'electric-indent-functions 'electric-indent-ignore-python)


(provide 'init-python)
;;; init-python.el ends here
