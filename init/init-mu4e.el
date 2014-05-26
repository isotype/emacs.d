;;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Author: Anton Strilchuk <anton@isoty.pe>                       ;;;
;;; URL: http://isoty.pe                                           ;;;
;;; Created: 25-05-2014                                            ;;;
;;; Last-Updated: 25-05-2014                                       ;;;
;;;   By: Anton Strilchuk <anton@isoty.pe>                         ;;;
;;;                                                                ;;;
;;; Filename: init-mu4e                                            ;;;
;;; Version:                                                       ;;;
;;; Description:                                                   ;;;
;;;                                                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'mu4e)
(require 'org-mu4e)

;;(require-package 'mu4e-maildirs-extension)
;;(mu4e-maildirs-extension)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; defaults
(setq mu4e-maildir "~/.mail/anton-ilyfa.cc")
(setq mu4e-drafts-folder "/drafts")
(setq mu4e-sent-folder   "/sent")
(setq mu4e-trash-folder  "/trash")

;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

;; setup some handy shortcuts
;; you can quickly switch to your Inbox -- press ``ji''
;; then, when you want archive some messages, move them to
;; the 'All Mail' folder by pressing ``ma''.

(setq mu4e-maildir-shortcuts
      '( ("/INBOX"               . ?i)
         ("/starred"   . ?!)
         ("/drafts" . ?d)
         ("/sent"   . ?s)
         ("/trash"       . ?t)
         ("/archive"    . ?a)))
;; allow for updating mail using 'U' in the main view:
;; I have this running in the background anyway
(setq mu4e-get-mail-command "offlineimap -q") ;;offlineimap -q
;; Use fancy chars
(setq mu4e-use-fancy-chars t)

;; See http://www.emacswiki.org/emacs/GnusGmail for more details
(require 'smtpmail-async)
(setq message-send-mail-function 'async-smtpmail-send-it
      ;; message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "anton@ilyfa.cc" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-debug-info t
      smtpmail-local-domain "ilyfa.cc"
      sendmail-coding-system 'UTF-8
      smtpmail-queue-mail nil
      smtpmail-queue-dir "/Users/anton/.mail/queue/cur")

;; set encoding system
;; (defun async-smtpmail-send-it ()
;;   (let ((to (message-field-value "To")))
;;     (message "Delivering message to %s..." to)
;;     (async-start
;;      `(lambda ()
;;         (require-package 'smtpmail)
;;         (with-temp-buffer
;;           (let ((coding-system-for-read 'utf-8)
;;                 (coding-system-for-write 'utf-8))
;;             (insert ,(buffer-substring-no-properties (point-min) (point-max)))
;;             ;; Pass in the variable environment for smtpmail
;;             ,(async-inject-variables "\\`\\(smtpmail\\|\\(user-\\)?mail\\)-")
;;             (smtpmail-send-it))))
;;      `(lambda (&optional ignore)
;;         (message "Delivering message to %s...done" ,to)))))

;; sending mail
;; (setq message-send-mail-function 'message-send-mail-with-sendmail
;;       sendmail-program "/usr/local/bin/msmtp")

(setq message-signature nil)

(setq org-mu4e-convert-to-html t)

(require-package 'w3m)
(setq mu4e-confirm-quit nil
      mu4e-headers-date-format "%d/%b/%Y %H:%M" ; date format
      mu4e-html2text-command "w3m -dump -T text/html")
(setq mu4e-html-renderer 'w3m)
;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

;; show images
;; Try to display images in mu4e
(setq
 mu4e-view-show-images t
 mu4e-view-image-max-width 800)

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
(setq mu4e-update-interval 300) ;; update every 5 minutes
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
          (defun y_pe/do-compose-stuff ()
            "My settings for message composition."
            (set-fill-column 72)
            (flyspell-mode)))

(add-hook 'mu4e-compose-mode-hook
          (defun y_pe/add-bcc ()
            "Add a Bcc: header."
            (save-excursion (message-add-header "Bcc: anton@ilyfa.cc\n"))))

(provide 'init-mu4e)
