;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author: Anton Strilchuk <ype@env.sh>                             ;;
;; URL: http://ype.env.sh                                           ;;
;; Created: 08-08-2014                                              ;;
;; Last-Updated: 08-08-2014                                         ;;
;;  Update #: 6                                                     ;;
;;   By: Anton Strilchuk <ype@env.sh>                               ;;
;;                                                                  ;;
;; Filename: init-git-submodules                                    ;;
;; Version:                                                         ;;
;; Description:                                                      ;;
;;                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq git-submodules-directory "/Users/anton/.emacs.d/submodules/" )
(defun require-git-submodule (submodule)
  (add-to-list 'load-path (concat git-submodules-directory (symbol-name submodule) "/")))

(provide 'init-git-submodules)
