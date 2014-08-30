;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author: Anton Strilchuk <anton@ilyfa.cc>                         ;;
;; URL: http://ype.env.sh                                           ;;
;; Created: 30-08-2014                                              ;;
;; Last-Updated: 30-08-2014                                         ;;
;;  Update #: 11                                                    ;;
;;   By: Anton Strilchuk <anton@ilyfa.cc>                           ;;
;;                                                                  ;;
;; Filename: init-ess                                               ;;
;; Version:                                                         ;;
;; Description:                                                     ;;
;;                                                                  ;;
;; Package Requires: ()                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require-package 'ess)
(require-package 'ess-R-data-view)
(require-package 'ess-smart-underscore)

(add-to-list 'auto-mode-alist '("\\.R$" . R-mode))
(add-to-list 'auto-mode-alist '("\\.r$" . R-mode))

(setq inferior-R-program-name "/usr/local/bin/R"
      ess-ask-for-ess-directory nil
      ess-directory "~/Development/r/workspace/"
      ess-ask-about-transfile nil
      ess-use-auto-complete t
      ess-r-args-electric-paren t)


(provide 'init-ess)
