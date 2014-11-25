;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author: Anton Strilchuk <ype@env.sh>                             ;;
;; URL: http://ype.env.sh                                           ;;
;; Created: 11-06-2014                                              ;;
;;; Last-Updated: 25-11-2014                                       ;;;
;;;   By: Anton Strilchuk <anton@env.sh>                           ;;;
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
(require 'mu4e-contrib)

;; Needs Emacs Async Package from Github
;; for smtpmail-async
;;(require-git-submodule 'emacs-async)

;; Not Working Yet
;; (require 'ype-network-manager)
(require-package 'w3m)

;;,--------------------------
;;| DEFAULTS
;;| General settings for MU4E
;;`--------------------------

(setq mail-user-agent 'mu4e-user-agent                           ; mu4e as default mail agent
      read-mail-command 'mu4e
      mu4e-maildir "~/.mail/ilyfa.cc"                            ; set mu4e mail directory
      mu4e-drafts-folder "/drafts"                               ; set drafts folder
      mu4e-sent-folder   "/sent"                                 ; set sent folder
      mu4e-trash-folder  "/trash"                                ; set trash folder
      mu4e-attachment-dir "~/Documents/email-attachments"        ; put attachements in download dir
      mu4e-confirm-quit nil                                        ; don't ask me to quit
      message-kill-buffer-on-exit t                              ; don't keep message buffers around
      mu4e-headers-skip-duplicates t                             ; skip duplicate email, great for gmail
      mu4e-headers-date-format "%A at %H:%M"                     ; date format
      mu4e-headers-leave-behavior 'apply                         ; apply all marks at quit
      mu4e-html2text-command 'mu4e-shr2text                      ; html to text
      mu4e-view-prefer-html nil                                    ; if text version available prefer it
      mu4e-compose-dont-reply-to-self t                          ; don't reply to myself
      mail-signature nil                                           ; kill default signature
      mu4e-compose-signature nil
      mu4e-compose-signature-auto-include nil
      org-mu4e-convert-to-html t                                 ; automatic convert org-mode => html
      mu4e-sent-messages-behavior 'delete                        ; don't delete messages
      mu4e-compose-complete-only-personal t                      ; only personal messages get in the address book
      mu4e-use-fancy-chars t                                     ; use fancy characters
      mu4e-get-mail-command "true"
      mu4e-html-renderer 'w3m                                    ; use w3m to render html in mail
      mu4e-view-show-images nil                                   ; auto show images
      mu4e-view-image-max-width 450                              ; set max image width
      mu4e-user-mail-address-list '("anton@env.sh"
                                    "ype@env.sh"
                                    "anton@ilyfa.cc"
                                    "anton@ilyfa.com"
                                    "antonstrilchuk@gmail.com"
                                    "anton@homeroom.org.uk"))

(add-hook 'mu4e-compose-mode-hook 'turn-off-auto-fill)
(remove-hook 'text-mode-hook 'turn-on-auto-fill)

;; 1) messages to me@foo.com should be replied with From:me@foo.com
;; 2) messages to me@bar.com should be replied with From:me@bar.com
;; 3) all other mail should use From: anton@env.sh
(add-hook 'mu4e-compose-pre-hook
          (defun my-set-from-address ()
            "Set the From address based on the To address of the original."
            (let ((msg mu4e-compose-parent-message))
              (if msg
                  (setq user-mail-address
                        (cond
                         ((mu4e-message-contact-field-matches msg :to "antonstrilchuk@gmail.com")
                          "anton@env.sh")
                         ((mu4e-message-contact-field-matches msg :to "anton@ilyfa.cc")
                          "anton@env.sh")
                         ((mu4e-message-contact-field-matches msg :to "anton@ilyfa.com")
                          "anton@env.sh")
                         ((mu4e-message-contact-field-matches msg :to "anton@env.sh")
                          "anton@env.sh")
                         ((mu4e-message-contact-field-matches msg :to "anton@homeroom.org.uk")
                          "anton@homeroom.org.uk")
                         ((mu4e-message-contact-field-matches msg :to "ype@env.sh")
                          "ype@env.sh")
                         (t "anton@env.sh")))))))

;; reply attribution line
(setq message-citation-line-format "On %A \[%d/%m/%y\]\n%N wrote:"
      message-citation-line-function 'message-insert-formatted-citation-line)

;; headers in the overview
(setq mu4e-headers-fields
      '((:flags         .   6)
        (:from-or-to    .  16)
        (:date          .  24)
        (:subject       .  nil)
        ))

;;,--------------------------------------------------------
;;| SHORTCUTS
;;| you can quickly switch to your Inbox -- press ``ji''
;;| then, when you want archive some messages, move them to
;;| the 'All Mail' folder by pressing ``ma''.
;;`--------------------------------------------------------
(setq mu4e-maildir-shortcuts
      '( ("/INBOX"        . ?i)
         ("/A_important"  . ?a)
         ("/B_work"       . ?b)
         ("/C_misc"       . ?c)
         ("/starred"      . ?s)
         ("/drafts"       . ?d)
         ("/sent"         . ?S)
         ("/trash"        . ?t)
         ("/archive"      . ?r)))
(add-to-list 'mu4e-bookmarks '("flag:attach"    "Messages with attachment"   ?a) t)
(add-to-list 'mu4e-bookmarks '("size:5M..500M"  "Big messages"               ?b) t)
(add-to-list 'mu4e-bookmarks '("flag:flagged"   "Flagged messages"           ?f) t)

;; See http://www.emacswiki.org/emacs/GnusGmail for more details
(require-git-submodule 'emacs-async)
(require 'async)
(require 'smtpmail-async)
(setq message-send-mail-function 'async-smtpmail-send-it
      send-mail-function 'async-smtpmail-send-it
      smtpmail-stream-type 'starttls
      ;;smtpmail-starttls-credentials '(("smtp.mandrillapp.com" 587 nil nil))
      ;;smtpmail-auth-credentials '(("smtp.mandrillapp.com" 587 "anton@ilyfa.cc" nil))
      smtpmail-default-smtp-server "smtp.mandrillapp.com"
      smtpmail-smtp-server "smtp.mandrillapp.com"
      smtpmail-smtp-service 587
      smtpmail-debug-info t
      smtpmail-local-domain "ilyfa.cc"
      sendmail-coding-system 'utf-8
      smtpmail-queue-mail t
      smtpmail-queue-dir "/Users/anton/.mail/queue/cur")

(global-set-key (kbd "H-x s") 'smtpmail-send-queued-mail)

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


;;,-------------------------
;;| MU4E Maildirs Extentions
;;`-------------------------
(require-package 'mu4e-maildirs-extension)
(require 'mu4e-maildirs-extension)
(defun ype:mu4e-maildirs-extension-propertize-unread-only (item)
  "Propertize only the maildir unread count using ITEM plist."
  (format "%s\t%s%s %s (%s/%s)\n"
          (if (equal (plist-get item :level) 0) "\n" "")
          (plist-get item :indent)
          (plist-get item :separator)
          (plist-get item :name)
          (propertize
           (number-to-string (plist-get item :unread))
           'face (cond
                  ((> (plist-get item :unread) 0) 'mu4e-maildirs-extension-maildir-unread-face)
                  (t 'mu4e-maildirs-extension-maildir-face)))
          (plist-get item :total)))

(mu4e-maildirs-extension)
(setq mu4e-maildirs-extension-propertize-func 'ype:mu4e-maildirs-extension-propertize-unread-only
      mu4e-maildirs-extension-insert-before-str "  Misc"
      mu4e-maildirs-extension-title nil
      mu4e-maildirs-extension-maildir-separator " » "
      mu4e-maildirs-extension-submaildir-separator " | "
      mu4e-maildirs-extension-custom-list (quote ("/INBOX" "/A_important" "/B_work"
                                                  "/C_misc" "/flagged" "/drafts"
                                                  "/sent" "/archive" "/trash")))


;;,---------
;;| Org MU4E
;;`---------
(require 'org-mu4e)
(require 'ox)

(defalias 'org-mail 'org-mu4e-compose-org-mode)
(defun org-export-string (data &rest rest)
  (let ((org-html-with-latex 'imagemagick))
    (org-export-string-as
     data 'html t)))



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
                             (concat "X-MC-Tags: mu4e\n")))))


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
;; TODO: Need Less Annoying Notification
(defvar monitor-attributes nil
  "Cached file attributes to be monitored.")

(defun mail-monitor (file secs)
  (run-with-timer
   0 secs
   (lambda (f p)
     (let ((att (file-attributes f)))
       (unless (or (null monitor-attributes) (equalp monitor-attributes att))
         (progn
           (mu4e-update-mail-and-index 1)
           ;; Uncomment to get Notifications when INBOX is changed
           ;; (tn-notify "INBOX: checked and updated")
           ))
       (setq monitor-attributes att))) file secs))

(defvar monitor-timer (mail-monitor "/Users/anton/.offlineimap/Account-anton-ilyfa/LocalStatus-sqlite/inbox" 60))


;;,---------
;;| Contacts
;;`---------
;; Google Contacts
(require 'org-contacts)
(require-package 'google-contacts)
(require 'google-org-contacts)

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


;;,------------------------------------------
;;| FIX: Use MU4E as default OS X Mail Client
;;`------------------------------------------

;;,---------------------------------------
;;| ,---------------
;;| | Smart Refiling
;;| `---------------
;;|  This file is not included with config
;;|  Not Required by Config
;;`---------------------------------------
(require 'init-mu4e-sr)


;; Import OSX Addressbook
(add-to-list 'load-path (expand-file-name "init-tools" user-emacs-directory))
(require 'external-abook)
(setq external-abook-command "contacts -lSf '%%e\t\"%%n\"' '%s'")
(eval-after-load "message"
  '(progn
     (add-to-list 'message-mode-hook
                  '(lambda ()
                     (define-key message-mode-map "\C-c\t" 'external-abook-try-expand)))))


;;HELM-MU, git fork:ype/helm-mu
;; (require-git-submodule 'helm-mu t)
;; (after-load 'helm-mu
;;   (setq helm-mu-default-search-string "maildir:/INBOX")
;;   (define-key ctrl-apos (kbd "m") 'helm-mu))


;; Send HTML and Plain Text
(defun mimedown ()
  (interactive)
  (save-excursion
    (message-goto-body)
    (shell-command-on-region (point) (point-max) "mimedown" nil t)))


(provide 'init-mu4e)
