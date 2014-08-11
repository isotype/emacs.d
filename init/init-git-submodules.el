;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author: Anton Strilchuk <ype@env.sh>                             ;;
;; URL: http://ype.env.sh                                           ;;
;; Created: 08-08-2014                                              ;;
;; Last-Updated: 11-08-2014                                         ;;
;;  Update #: 11                                                    ;;
;;   By: Anton Strilchuk <ype@env.sh>                               ;;
;;                                                                  ;;
;; Filename: init-git-submodules                                    ;;
;; Version:                                                         ;;
;; Description:                                                      ;;
;;                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq git-submodules-directory "/Users/anton/.emacs.d/submodules/" )
(defun require-git-submodule (submodule &optional add-a-require)
  "Add Package from Git-Submodules-Directory and optional require package"
  (interactive)
  (add-to-list 'load-path (concat git-submodules-directory (symbol-name submodule) "/"))
  (if add-a-require
      (progn
        (let ((symodule submodule))
          (require symodule)))
    (progn
      nil)))

(provide 'init-git-submodules)
