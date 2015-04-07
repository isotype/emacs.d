;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Filename: init-sauron                                          ;;;
;;; Created: 10-02-2015                                            ;;;
;;; Author: Anton Strilchuk <anton@env.sh>                         ;;;
;;; URL: http://ype.env.sh                                         ;;;
;;; Version:                                                       ;;;
;;; Last-Updated: 26-03-2015                                       ;;;
;;;  Update #: 15                                                  ;;;
;;;   By: Anton Strilchuk <anton@env.sh>                           ;;;
;;;                                                                ;;;
;;; Description:                                                   ;;;
;;;                                                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init-sauron)

;;(require 'alert)
;;(defun alert-notifier-notify (info)
;;  (if alert-notifier-command
;;      (let ((args
;;             (list "-title"   (alert-encode-string (plist-get info :title))
;;                   "-activate" "org.gnu.Emacs"
;;                   "-message" (alert-encode-string (plist-get info :message))
;;                   "-execute" (format "\"%s\"" (switch-to-buffer-command ;;(plist-get info :buffer))))))
;;        (apply #'call-process alert-notifier-command nil nil nil args))
;;    (alert-message-notify info))
;;  (alert-message-notify info))
;;(setq alert-default-style 'growl)
;;(setq alert-log-messages nil)

(require-package 'sauron)
(require 'sauron)

(setq sauron-modules '(sauron-erc sauron-org sauron-notifications
                                  sauron-twittering sauron-jabber sauron-identica))

;;(defun sauron-dbus-start () nil)
;;(makunbound 'dbus-path-emacs)

(global-set-key (kbd "C-c s") 'sauron-toggle-hide-show)
(global-set-key (kbd "C-c t") 'sauron-clear)

(defun sauron:jabber-notify (origin priority message &optional properties)
  (funcall notify-function "hipchat" message))

(setq sauron-timestamp-format "%H:%M:%S")
(setq sauron-column-alist
      '((timestamp . 10)
        (origin . 7)
        (priority . 4)
        (message . nil)))

(setq sauron-max-line-length 150
      sauron-hide-mode-line t
      sauron-separate-frame nil
      sauron-watch-patterns '("astrilchuk")
      sauron-watch-nicks '("astrilchuk"))

(defun sauron:dispatch-notify (origin priority message &optional properties)
  (let ((handler (cond ((string= origin "erc") 'sauron:erc-notify)
                       ((string= origin "jabber") 'sauron:jabber-notify)
                       ((string= origin "mu4e") 'sauron:mu4e-notify)
                       ((string= origin "dbus") 'sauron:dbus-notify)
                       (t (lambda (&rest r) nil)))))
    (funcall handler origin priority message properties)))

;;(add-hook 'sauron-event-added-functions 'sauron-alert-el-adapter)

;; (setq display-buffer-alist              ; Treat sauron specially.
;;       '("*Sauron*" . ""))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;init-sauron.el ends here
