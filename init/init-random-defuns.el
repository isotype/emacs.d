;;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Author: Anton Strilchuk <anton@isoty.pe>                       ;;;
;;; URL: http://isoty.pe                                           ;;;
;;; Created: 25-05-2014                                            ;;;
;; Last-Updated: 28-07-2014                                         ;;
;;   By: Anton Strilchuk <ype@env.sh>                               ;;
;;;                                                                ;;;
;;; Filename: defuns                                               ;;;
;;; Version:                                                       ;;;
;;; Description:                                                   ;;;
;;;                                                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ype/get-to-work ()
  "Depends on: sudo gem install work 
this function blocks internet traffic to specific 
time wasting sites"
  (interactive)
  (shell-command
   (concat "echo "
	   (shell-quote-argument
	    (read-passwd "Password: ")) " | sudo -S work start")))

(defun ype/fuck-work ()
  "Depends on: sudo gem install work
this function unblocks internet traffic to specific
time wasting sites"
  (interactive)
  (shell-command
   (concat "echo "
	   (shell-quote-argument
	    (read-passwd "Password: ")) " | sudo -S work stop")))

;;Reload browser
(defun ype/reload-browser ()
  (interactive)
  (shell-command
   (concat
    "chromix with http://localhost:4000/ reload")))

;;Reload Browser, Requires Chromix
(global-set-key "\C-crr" 'ype/reload-browser)

(defun ype/kill-global-wrap ()
  "Depends on: nothing
This function quickly switches visual-line-mode and auto-fill-mode off"
  (interactive)
  (visual-line-mode 0)
  (auto-fill-mode 0))

;;Quick switch text wrap on
(defun ype/global-wrap ()
    "Depends on: nothing
This function quickly switches visual-line-mode and auto-fill-mode on"
  (interactive)
  (visual-line-mode 0)
  (auto-fill-mode 0))

(defun ype/save-buffers-kill-emacs (&optional arg)
  "Offer to save each buffer(once only), then kill this Emacs process.
With prefix ARG, silently save all file-visiting buffers, then kill.
source: http://stackoverflow.com/questions/6762686/prevent-emacs-from-asking-modified-buffers-exist-exit-anyway"
 (interactive "P")
  (save-some-buffers t)
  (and (or (not (fboundp 'process-list))
	   ;; process-list is not defined on MSDOS.
	   (let ((processes (process-list))
		 active)
	     (while processes
	       (and (memq (process-status (car processes)) '(run stop open listen))
		    (process-query-on-exit-flag (car processes))
		    (setq active t))
	       (setq processes (cdr processes)))
	     (or (not active)
		 (progn (list-processes t)
			(yes-or-no-p "Active processes exist; kill them and exit anyway? ")))))
       ;; Query the user for other things, perhaps.
       (run-hook-with-args-until-failure 'kill-emacs-query-functions)
       ;;(or (null confirm-kill-emacs)
	   ;;(funcall confirm-kill-emacs "Really exit Emacs? "))
       (kill-emacs)))

(defun camelCase-to_underscores (start end)
  "Convert any string matching something like aBc to a_bc"
  (interactive "r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char 1)
    (let ((case-fold-search nil))
      (while (search-forward-regexp "\\([a-z]\\)\\([A-Z]\\)\\([a-z]\\)" nil t)
        (replace-match (concat (match-string 1)
                               "_"
                               (downcase (match-string 2))
                               (match-string 3))
                       t nil)))))

;; ;;ERC TERMINAL NOTIFIER
;; (defvar erc-terminal-notifier-command nil "The path to terminal-notifier.")
;; (setq erc-terminal-notifier-command (executable-find "terminal-notifier"))

;; (defun erc-terminal-notifier-notify (title message)
;;   "Show a message with `terminal-notifier-command`."
;;   (start-process "terminal-notifier"
;;                  "*terminal-notifier*"
;;                  erc-terminal-notifier-command
;;                  "-title" title
;;                  "-message" message
;;                  "-activate" "org.gnu.Emacs"
;;                  "-sender" "org.gnu.Emacs"))

;; (defun erc-terminal-notifier-text-matched (match-type nick message)
;;   "Show a notification, when user's nick is mentioned."
;;   (when (eq match-type 'current-nick)
;;     (unless (posix-string-match "^\\** *Users on #" message)
;;       (erc-terminal-notifier-notify
;;        (concat "ERC " (buffer-name (current-buffer)))
;;        (concat "\\<" (nth 0 (erc-parse-user nick)) "> " message)))))

;; (if (eq system-type 'darwin)
;;     (add-hook 'erc-text-matched-hook 'erc-terminal-notifier-text-matched))

(provide 'init-random-defuns)
