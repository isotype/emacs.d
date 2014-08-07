;;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Author: Anton Strilchuk <anton@isoty.pe>                       ;;;
;;; URL: http://isoty.pe                                           ;;;
;;; Created: 15-04-2014                                            ;;;
;; Last-Updated: 07-08-2014                                         ;;
;;   By: Anton Strilchuk <ype@env.sh>                               ;;
;;;                                                                ;;;
;;; Filename: init-irc                                             ;;;
;;; Version:                                                       ;;;
;;; Description:                                                   ;;;
;;;                                                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require-package 'erc-terminal-notifier)

;; modules..
(setq erc-modules '(autojoin
                    button
                    completion
                    fill
                    irccontrols
                    keep-place
                    list
                    match
                    menu
                    move-to-prompt
                    netsplit
                    networks
                    noncommands
                    ;; notifications
                    readonly
                    ring
                    scrolltobottom
                    smiley
                    stamp
                    track))


;; add some color to nicks
(require-package 'erc-hl-nicks)
(add-hook 'erc-mode-hook 'erc-hl-nicks-mode)

;; wrap lines to buffer when typing
(add-hook 'erc-mode-hook 'visual-line-mode)

(setq erc-timestamp-only-if-changed-flag nil ;; always timestamp
      erc-timestamp-format "%H:%M:%S "
      erc-fill-prefix nil ;; don't force indentation
      erc-insert-timestamp-function 'erc-insert-timestamp-left ;; put the timestamp left
      erc-hide-list '("JOIN" "PART" "QUIT") ;; hide pesky stuff
      erc-input-line-position -1
      erc-prompt "[ype]: "
      erc-current-nick-highlight-type (quote all) ;; highlight full message to me
      erc-fill-column 85
      ;; matches
      erc-text-matched-hook '(erc-log-matches
                              erc-terminal-notifier-text-matched)
      erc-match-exclude-server-buffer t ;; don't bother matching the server buffer
      ;; log matches
      erc-log-matches-flag t ;; log mentions and keywords in their own buffer
      erc-log-matches-types-alist '((keyword . "#ERC Keywords")
                                    (current-nick . "#ERC Mentions")))

(make-variable-buffer-local 'erc-fill-column)
(add-hook 'window-configuration-change-hook
     '(lambda ()
        (save-excursion
          (walk-windows
     (lambda (w)
       (let ((buffer (window-buffer w)))
         (set-buffer buffer)
         (when (eq major-mode 'erc-mode)
           (setq erc-fill-column (- (window-width w) 2)))))))))

;; From: Emacs Wiki
;; Link: http://bit.ly/1qZL2Wp
(defmacro asf-erc-bouncer-connect (command server port nick ssl pass)
   "Create interactive command `command', for connecting to an IRC server. The
   command uses interactive mode if passed an argument."
   (fset command
         `(lambda (arg)
           (interactive "p")
     (if (not (= 1 arg))
         (call-interactively 'erc)
       (let ((erc-connect-function ',(if ssl
                 'erc-open-ssl-stream
               'open-network-stream)))
         (erc :server ,server :port ,port :nick ,nick :password ,pass))))))

(provide 'init-irc)
