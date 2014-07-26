;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author: Anton Strilchuk <ype@env.sh>                             ;;
;; URL: http://ype.env.sh                                           ;;
;; Created: 11-06-2014                                              ;;
;; Last-Updated: 25-07-2014                                         ;;
;;   By: Anton Strilchuk <anton@ilyfa.cc>                           ;;
;;                                                                  ;;
;; Filename: init-mu4e                                              ;;
;; Version:                                                         ;;
;; Description:                                                     ;;
;;                                                                  ;;
;; Package Requires: ()                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "/usr/local/Cellar/mu/HEAD/share/emacs/site-lisp/mu4e")
(require-package 'offlineimap)
(require 'mu4e)
(require 'org-mu4e)
;; Needs Emacs Async Package from Github
;; for smtpmail-async
(require-git-package 'jwiegley/emacs-async)

;; Not Working Yet
;; (require 'ype-network-manager)
(require-package 'w3m)

;;(require-package 'mu4e-maildirs-extension)
;;(mu4e-maildirs-extension)


;;,--------------------------
;;| DEFAULTS
;;| General settings for MU4E
;;`--------------------------

(setq mail-user-agent 'mu4e-user-agent                           ; mu4e as default mail agent
      mu4e-maildir "~/.mail/anton-ilyfa.cc"                      ; set mu4e mail directory
      mu4e-drafts-folder "/drafts"                               ; set drafts folder
      mu4e-sent-folder   "/sent"                                 ; set sent folder
      mu4e-trash-folder  "/trash"                                ; set trash folder
      mu4e-attachment-dir "~/Documents/email-attachments"        ; put attachements in download dir
      mu4e-confirm-quit nil                                        ; don't ask me to quit
      message-kill-buffer-on-exit t                              ; don't keep message buffers around
      mu4e-headers-skip-duplicates t                             ; skip duplicate email, great for gmail
      mu4e-headers-date-format "%A at %H:%M"                     ; date format
      mu4e-headers-leave-behavior 'apply                         ; apply all marks at quit
      mu4e-html2text-command "w3m -dump -T text/html -cols 72"   ; html to text
      mu4e-compose-dont-reply-to-self t                          ; don't reply to myself
      mail-signature nil                                           ; kill default signature
      mu4e-compose-signature nil                                   ; signature
      org-mu4e-convert-to-html t                                 ; automatic convert org-mode => html
      mu4e-sent-messages-behavior 'delete                        ; don't delete messages
      mu4e-compose-complete-only-personal t                      ; only personal messages get in the address book
      mu4e-use-fancy-chars t                                     ; use fancy characters
      mu4e-get-mail-command "offlineimap -q"                     ; fetch email with offlineimap
      mu4e-html-renderer 'w3m                                    ; use w3m to render html in mail
      mu4e-view-show-images t                                    ; auto show images
      mu4e-view-image-max-width 450                              ; set max image width
      )

;; my email addresses
(setq mu4e-user-mail-address-list
      (list "antonstrilchuk@gmail.com" "anton@ilyfa.cc" "anton@ilyfa.com"
            "anton@isoty.pe" "anton@env.sh" "anton@homeroom.org.uk" "ype@env.sh"))

;; 1) messages to me@foo.com should be replied with From:me@foo.com
;; 2) messages to me@bar.com should be replied with From:me@bar.com
;; 3) all other mail should use From: ype@env.sh
(add-hook 'mu4e-compose-pre-hook
          (defun my-set-from-address ()
            "Set the From address based on the To address of the original."
            (let ((msg mu4e-compose-parent-message))
              (if msg
                  (setq user-mail-address
                        (cond
                         ((mu4e-message-contact-field-matches msg :to "antonstrilchuk@gmail.com")
                          "antonstrilchuk@gmail.com")
                         ((mu4e-message-contact-field-matches msg :to "anton@ilyfa.cc")
                          "anton@ilyfa.cc")
                         ((mu4e-message-contact-field-matches msg :to "anton@ilyfa.com")
                          "anton@ilyfa.com")
                         ((mu4e-message-contact-field-matches msg :to "anton@isoty.pe")
                          "anton@isoty.pe")
                         ((mu4e-message-contact-field-matches msg :to "anton@env.sh")
                          "anton@env.sh")
                         ((mu4e-message-contact-field-matches msg :to "anton@homeroom.org.uk")
                          "anton@homeroom.org.uk")
                         ((mu4e-message-contact-field-matches msg :to "ype@env.sh")
                          "ype@env.sh")
                         (t "ype@env.sh")))))))

;; reply attribution line
(setq message-citation-line-format "On %A \[%d/%m/%y\]\n%N wrote:")
(setq message-citation-line-function 'message-insert-formatted-citation-line)

;;,--------------------------------------------------------
;;| SHORTCUTS
;;| you can quickly switch to your Inbox -- press ``ji''
;;| then, when you want archive some messages, move them to
;;| the 'All Mail' folder by pressing ``ma''.
;;`--------------------------------------------------------
(setq mu4e-maildir-shortcuts
      '( ("/INBOX"    . ?i)
         ("/starred"  . ?!)
         ("/drafts"   . ?d)
         ("/sent"     . ?s)
         ("/trash"    . ?t)
         ("/archive"  . ?a)))

;; See http://www.emacswiki.org/emacs/GnusGmail for more details
(require 'smtpmail-async)
(setq message-send-mail-function 'async-smtpmail-send-it
      ;; message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.mandrillapp.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.mandrillapp.com" 587 "anton@ilyfa.cc" nil))
      smtpmail-default-smtp-server "smtp.mandrillapp.com"
      smtpmail-smtp-server "smtp.mandrillapp.com"
      smtpmail-smtp-service 587
      smtpmail-debug-info nil
      smtpmail-local-domain "ilyfa.cc"
      sendmail-coding-system 'UTF-8
      smtpmail-queue-mail t
      smtpmail-queue-dir "/Users/anton/.mail/queue/cur")

(global-set-key (kbd "H-x H-s") 'smtpmail-send-queued-mail)

;; headers in the overview
(setq mu4e-headers-fields
      '((:flags         .   6)
        (:from          .  16)
        (:date          .  24)
        (:subject       .  nil)
        ))

;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;; message view action
(defun mu4e-msgv-action-view-in-browser (msg)
  "View the body of the message in a web browser."
  (interactive)
  (let ((html (mu4e-msg-field (mu4e-message-at-point t) :body-html))
        (tmpfile (format "%s/%d.html" temporary-file-directory (random))))
    (unless html (error "No html part for this message"))
    (with-temp-file tmpfile
      (insert
       "<html>"
       "<head><meta http-equiv=\"content-type\""
       "content=\"text/html;charset=UTF-8\">"
       html))
    (browse-url (concat "file://" tmpfile))))

(add-to-list 'mu4e-view-actions
             '("View in browser" . mu4e-msgv-action-view-in-browser) t)

(require 'gnus-dired)
;; make the `gnus-dired-mail-buffers' function also work on
;; message-mode derived modes, such as mu4e-compose-mode
(defun gnus-dired-mail-buffers ()
  "Return a list of active message buffers."
  (let (buffers)
    (save-current-buffer
      (dolist (buffer (buffer-list t))
        (set-buffer buffer)
        (when (and (derived-mode-p 'message-mode)
                   (null message-sent-message-via))
          (push (buffer-name buffer) buffers))))
    (nreverse buffers)))

(setq gnus-dired-mail-mode 'mu4e-user-agent)
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)
(add-hook 'mu4e-index-updated-hook
          '(lambda ()
             (tn-notify "Mail has been updated, next update in 3 minutes" "MU4E" "New/Updated Mail")))

;;MU4E Maildirs Extentions
;; (setq mu4e-maildirs-extension-insert-before-str "\n  Bookmarks")
;; (setq mu4e-maildirs-extension-maildir-separator "∑ ")
;; (setq mu4e-maildirs-extension-submaildir-separator "\t ∫ ")

(defalias 'org-mail 'org-mu4e-compose-org-mode)
(global-set-key (kbd "H-c") 'mu4e)

(add-hook 'mu4e-compose-mode-hook
          (defun ype/do-compose-stuff ()
            "My settings for message composition."
            (set-fill-column 72)
            (flyspell-mode)))

(add-hook 'mu4e-compose-mode-hook
          (defun ype/add-mandrill-tags ()
            "Add Mandrill tags to header."
            (save-excursion (message-add-header
                             (concat "X-MC-BccAddress: antonstrilchuk@gmail.com\n"
                                     "X-MC-Tags: mu4e\n"
                                     "X-MC-SendAt: \n")))))


;;,------------------------------
;;| External MAILTO Links Handler
;;`------------------------------
(defun ype/mu4e-mailto (to subject &optional body)
  "Handler for external @mailto: links in mu4e"
  (progn
    (mu4e-compose-new)
    (message-goto-to)
    (insert
     (url-unhex-string (format "%s" to)))
    (message-goto-subject)
    (insert
     (url-unhex-string (format "%s" subject)))
    (message-goto-body)
    (insert
     (url-unhex-string (format "\n%s\n" body)))))


;;,------------------------------
;;| Queue Connection Check & Send
;;`------------------------------

;; TODO: Need alternative connection check to dbus

(setq mu4e-update-interval (m2s 3))

;; (defun ype:on-connect()
;;   (message "Connected: Flushing Mail Queue")
;;   (offlineimap)
;;   (smtpmail-send-queued-mail)
;;   (setq smtpmail-queue-mail nil))

;; (defun ype:on-disconnect()
;;   (message "Disconnected: Queuing Mail")
;;   (condition-case ex
;;       (offlineimap-kill 9)
;;     ('error (message (format "Ignoring exception: %s" ex))))
;;   (setq smtpmail-queue-mail t))

;; (add-to-list 'nm-connected-hook 'ype:on-connect)
;; (add-to-list 'nm-disconnected-hook 'ype:on-disconnect)

;; (nm-enable)


;;,---------
;;| Contacts
;;`---------
;; Google Contacts
(require 'org-contacts)

(setq mu4e-org-contacts-file "~/.org-contacts.org")
(add-to-list 'mu4e-headers-actions
  '("org-contact-add" . mu4e-action-add-org-contact) t)
(add-to-list 'mu4e-view-actions
             '("org-contact-add" . mu4e-action-add-org-contact) t)


;;,---------------------------
;;| Send Mail At Specific Time
;;`---------------------------

(defun ype:send-it-later (date time)
  "
/=========================================================================`
| Insert SMTP-Header for Mandrill X-MC-SendAt, scheduled message delivery |
|                                                                         |
| Example: X-MC-SendAt: 2014-07-24 10:34:00+01:00                         |
| Usage: M-x ype:send-it-later                                            |
| Requires: insert-manual-day-and-time from ype:init-helpers.el           |
`=========================================================================/
"
  (interactive "sDeliver on [Date]: \nsDeliver at [24hr]: ")
  (insert "X-MC-SendAt: ")
  (insert-manual-day-and-time date time))



(provide 'init-mu4e)
