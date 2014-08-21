;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author: Anton Strilchuk <ype@env.sh>                             ;;
;; URL: http://ype.env.sh                                           ;;
;; Created: 18-08-2014                                              ;;
;; Last-Updated: 18-08-2014                                         ;;
;;  Update #: 5                                                     ;;
;;   By: Anton Strilchuk <ype@env.sh>                               ;;
;;                                                                  ;;
;; Filename: init-oascript                                          ;;
;; Version:                                                         ;;
;; Description:                                                     ;;
;;                                                                  ;;
;; Package Requires: ()                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when *is-a-mac*
  (require-package 'applescript-mode)
  (autoload 'applescript-mode "applescript-mode" "major mode for editing AppleScript source." t)
  (setq auto-mode-alist
      (cons '("\\.applescript\\.scpt\\'" . applescript-mode) auto-mode-alist)))



(provide 'init-oascript)
