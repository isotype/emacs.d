;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author: Anton Strilchuk <ype@env.sh>                             ;;
;; URL: http://ype.env.sh                                           ;;
;; Created: 23-09-2014                                              ;;
;; Last-Updated: 24-09-2014                                         ;;
;;  Update #: 13                                                    ;;
;;   By: Anton Strilchuk <antonstrilchuk@gmail.com>                 ;;
;;                                                                  ;;
;; Filename: init-markdown                                          ;;
;; Version:                                                         ;;
;; Description:                                                     ;;
;;                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require-package 'markdown-mode)
(require-package 'pandoc-mode)

(setq auto-mode-alist
      (cons '("\\.\\(md\\|markdown\\)\\'" . markdown-mode) auto-mode-alist))

(add-hook 'markdown-mode-hook 'turn-on-pandoc)
(add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)

;;(require-git-submodule 'realtime-markdown-viewer t)
;;(setq rtmv:lang 'ruby)


(provide 'init-markdown)
