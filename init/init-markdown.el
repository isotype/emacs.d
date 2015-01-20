;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author: Anton Strilchuk <ype@env.sh>                             ;;
;; URL: http://ype.env.sh                                           ;;
;; Created: 23-09-2014                                              ;;
;;; Last-Updated: 15-01-2015                                       ;;;
;;;  Update #: 16                                                  ;;;
;;;   By: Anton Strilchuk <anton@env.sh>                           ;;;
;;                                                                  ;;
;; Filename: init-markdown                                          ;;
;; Version:                                                         ;;
;; Description:                                                     ;;
;;                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require-package 'markdown-mode)
(require-package 'pandoc-mode)

(setq auto-mode-alist (cons '("\\.\\(md\\|markdown\\)\\'" . markdown-mode) auto-mode-alist))
(setq markdown-open-command "/usr/local/bin/mark")

(add-hook 'markdown-mode-hook 'turn-on-pandoc)
(add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)

;;(el-get-bundle syohex/emacs-realtime-markdown-viewer)
;;(setq rtmv:lang 'ruby)


(provide 'init-markdown)
