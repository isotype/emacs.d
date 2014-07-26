;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author: Anton Strilchuk <ype@env.sh>                             ;;
;; URL: http://ype.env.sh                                           ;;
;; Created: 16-06-2014                                              ;;


;;                                                                  ;;
;; Filename: init-feeds                                             ;;
;; Version:                                                         ;;
;; Description:                                                     ;;
;;                                                                  ;;
;; Package Requires: ()                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require-package 'elfeed)
(require-package 'elfeed-web)

(setq elfeed-feeds
      '(
        ;; Emacs Blogs
        ("http://emacsredux.com/atom.xml" emacs)
        ("http://whattheemacsd.com/atom.xml" emacs)
        ("http://www.masteringemacs.org/feed/" emacs)))

(global-set-key (kbd "C-x w") 'elfeed)

(provide 'init-feeds)
;;; init-feeds.el ends here
