;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author: Anton Strilchuk <anton@ilyfa.cc>                         ;;
;; URL: http://ype.env.sh                                           ;;
;; Created: 23-07-2014                                              ;;
;; Last-Updated: 12-08-2014                                         ;;
;;  Update #: 5                                                     ;;
;;   By: Anton Strilchuk <ype@env.sh>                               ;;
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
(require-package 'elpy)
(elpy-enable)



;;,---------------------------------------------------------------------------
;;|  Anaconda: Code navigation, documentation lookup and completion for Python
;;|  Link: [[https://github.com/proofit404/anaconda-mode][proofit404/anaconda-mode]]
;;`---------------------------------------------------------------------------

(require-package 'anaconda-mode)

(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'eldoc-mode)

(provide 'init-python)
;;; init-python.el ends here
