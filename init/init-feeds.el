;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author: Anton Strilchuk <anton@env.sh>                           ;;
;; URL: http://ype.env.sh                                           ;;
;; Created: 23-10-2014                                              ;;
;;; Last-Updated: 25-11-2014                                       ;;;
;;;  Update #: 3                                                   ;;;
;;;   By: Anton Strilchuk <anton@env.sh>                           ;;;
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
        ("http://www.masteringemacs.org/feed/" emacs)
        ;; Personal List
        ("http://getpocket.com/users/ilyfa/feed/all" self)
        ("http://git.io/asUhsA" self)
        ;; Blogs
        ("http://feeds.hanselman.com/ScottHanselman" blogs)
        ("http://feeds.feedburner.com/codinghorror" blogs)
        ("http://semanticweb.com/feed" blogs)
        ;; Data Mining/Machine Learning
        ("http://feeds.feedburner.com/kdnuggets-data-mining-analytics?format=xml" dm)
        ("http://machinelearningmastery.com/blog/feed/" dm)
        ))

;; (add-hook 'elfeed-new-entry-hook
;;           (elfeed-make-tagger :before "2 weeks ago"
;;                               :remove 'unread))

(global-set-key (kbd "C-x w") 'elfeed)


(provide 'init-feeds)
;;; init-feeds.el ends here
