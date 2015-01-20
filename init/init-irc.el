;;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Author: Anton Strilchuk <anton@isoty.pe>                       ;;;
;;; URL: http://ype.env.sh                                         ;;;
;;; Created: 15-04-2014                                            ;;;
;;; Last-Updated: 15-01-2015                                       ;;;
;;;   By: Anton Strilchuk <anton@env.sh>                           ;;;
;;;                                                                ;;;
;;; Filename: init-irc                                             ;;;
;;; Version:                                                       ;;;
;;; Description:                                                   ;;;
;;;                                                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require-package 'sr-speedbar)
;;(require-package 'znc)
(require 'erc)

;;ERC Terminal Notifier
;;(el-get-bundle emacsmirror/erc-terminal-notifier)

;;ERC Tabs
(add-to-list 'load-path (expand-file-name "init-tools/erc-tab" user-emacs-directory))

(defvar erc-header-line-uses-tabbar-p t
  "Use tab instead to display channels")

;; wrap lines to buffer when typing
(add-hook 'erc-mode-hook 'visual-line-mode)
(add-hook 'erc-mode-hook (lambda () (auto-fill-mode 0)))

(after-load 'erc
;;  (require 'znc)
;;  (require 'erc-terminal-notifier)
  (require 'erc-tab)
  (setq erc-timestamp-only-if-changed-flag nil ;; always timestamp
        erc-timestamp-format "[%H:%M] "
        erc-insert-timestamp-function 'erc-insert-timestamp-left ;; put the timestamp left
        erc-hide-list '("JOIN" "PART" "QUIT" "NICK") ;; hide pesky stuff
        erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE" "333" "353")
        erc-prompt "[ype]: "
        erc-fill-column 75
        erc-server-send-ping-interval 45
        erc-server-send-ping-timeout 180
        erc-server-reconnect-timeout 60
        erc-lurker-threshold-time 3600
        erc-join-buffer 'bury
        erc-flood-protect nil
        erc-track-use-faces t
        erc-track-faces-priority-list '(erc-current-nick-face erc-keyword-face)
        erc-current-nick-highlight-type 'all
        erc-complete-functions '(erc-pcomplete erc-button-next)))


;;,-------------------------------
;;|  From: [[http://www.emacswiki.org/emacs/ErcBar][EmacsWiki: Erc Bar]]
;;|  Read and Unread bar delimiter
;;`-------------------------------
(eval-after-load 'erc-track
  '(progn
     (defun erc-bar-move-back (n)
       "Moves back n message lines. Ignores wrapping, and server messages."
       (interactive "nHow many lines ? ")
       (re-search-backward "^.*<.*>" nil t n))

     (defun erc-bar-update-overlay ()
       "Update the overlay for current buffer, based on the content of
erc-modified-channels-alist. Should be executed on window change."
       (interactive)
       (let* ((info (assq (current-buffer) erc-modified-channels-alist))
              (count (cadr info)))
         (if (and info (> count erc-bar-threshold))
             (save-excursion
               (goto-char (point-max))
               (when (erc-bar-move-back count)
                 (let ((inhibit-field-text-motion t))
                   (move-overlay erc-bar-overlay
                                 (line-beginning-position)
                                 (line-end-position)
                                 (current-buffer)))))
           (delete-overlay erc-bar-overlay))))

     (defvar erc-bar-threshold 1
       "Display bar when there are more than erc-bar-threshold unread messages.")
     (defvar erc-bar-overlay nil
       "Overlay used to set bar")
     (setq erc-bar-overlay (make-overlay 0 0))
     (overlay-put erc-bar-overlay 'face '(:underline "black"))
     ;;put the hook before erc-modified-channels-update
     (defadvice erc-track-mode (after erc-bar-setup-hook
                                      (&rest args) activate)
       ;;remove and add, so we know it's in the first place
       (remove-hook 'window-configuration-change-hook 'erc-bar-update-overlay)
       (add-hook 'window-configuration-change-hook 'erc-bar-update-overlay))
     (add-hook 'erc-send-completed-hook (lambda (str)
                                          (erc-bar-update-overlay)))))

;;,--------------------------------------
;;| From: [[http://www.emacswiki.org/emacs/ErcChannelTracking][EmacsWiki: Erc Channel Tracking]]
;;| ERC Track and Untrack Channels
;;`--------------------------------------
(defun erc-cmd-UNTRACK (&optional target)
  "Add TARGET to the list of target to be tracked."
  (if target
      (erc-with-server-buffer
        (let ((untracked (car (erc-member-ignore-case target erc-track-exclude))))
          (if untracked
              (erc-display-line
               (erc-make-notice (format "%s is not currently tracked!" target))
               'active)
            (add-to-list 'erc-track-exclude target)
            (erc-display-line
             (erc-make-notice (format "Now not tracking %s" target))
             'active))))

    (if (null erc-track-exclude)
        (erc-display-line (erc-make-notice "Untracked targets list is empty") 'active)

      (erc-display-line (erc-make-notice "Untracked targets list:") 'active)
      (mapc (lambda (item)
                (erc-display-line (erc-make-notice item) 'active))
            (erc-with-server-buffer erc-track-exclude))))
  t)


(defun erc-cmd-TRACK (target)
  "Remove TARGET of the list of targets which they should not be tracked.
   If no TARGET argument is specified, list the contents of `erc-track-exclude'."
  (when target
    (erc-with-server-buffer
      (let ((tracked (not (car (erc-member-ignore-case target erc-track-exclude)))))
        (if tracked
            (erc-display-line
             (erc-make-notice (format "%s is currently tracked!" target))
             'active)
          (setq erc-track-exclude (remove target erc-track-exclude))
          (erc-display-line
           (erc-make-notice (format "Now tracking %s" target))
           'active)))))
  t)



;; From: Emacs Wiki
                                        ; Auto-reconnect
(add-hook 'erc-server-376-hook
          (lambda (&rest args)
             (keep-alive)))
(setq erc-auto-reconnect nil)

(defun keep-alive-kick-once ()
  (interactive)
  ;; this seems necessary else ERC won't send the ping after a while.
  (let ((erc-flood-protect nil))
    (save-window-excursion
      (when (buffer-name-p-my "#emacs")
        (switch-to-buffer "#emacs")
        ;; don't send pings unless we are actually alive, to protect flood-quits.
        (when (erc-process-alive) (erc-send-command "PING"))))))

(defvar keep-active-active-p nil)
(defun keep-alive ()
  (interactive)
                                        ; prevent timer duplication
  (run-with-timer 15 5 'keep-alive-kick-once)
  (setq keep-alive-active-p t)
  (message "Started erbtrain-keep-alive. "))

;; From: Emacs Wiki
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

(defadvice erc-display-prompt (after conversation-erc-display-prompt activate)
  "Insert last recipient after prompt."
  (let ((previous
         (save-excursion
           (if (and (search-backward-regexp (concat "^[^<]*<" erc-nick ">") nil t)
                  (search-forward-regexp (concat "^[^<]*<" erc-nick ">"
                                                 " *\\([^:]*: ?\\)") nil t))
               (match-string 1)))))
    ;; when we got something, and it was in the last 3 mins, put it in
    (when (and previous
             (> 180 (time-to-seconds
                     (time-since (get-text-property 0 'timestamp previous)))))
      (set-text-properties 0 (length previous) nil previous)
      (insert previous))))


(provide 'init-irc)
