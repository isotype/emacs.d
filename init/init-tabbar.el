;;Tabs
(require-package 'tabbar)
(eval-after-load 'init-appearance
  (require-package 'tabbar-ruler))
;; (defun tabbar-buffer-groups-by-dir ()
;;         "Put all files in the same directory into the same tab bar"
;;         (with-current-buffer (current-buffer)
;;           (let ((dir (expand-file-name default-directory)))
;;             (cond ;; assign group name until one clause succeeds, so the order is important
;;              ((eq major-mode 'dired-mode)
;;               (list "Dired"))
;;              ((memq major-mode
;;                     '(help-mode apropos-mode Info-mode Man-mode))
;;               (list "Help"))
;;              ((string-match-p "\*.*\*" (buffer-name))
;;               (list "Misc"))
;;              (t (list dir))))))

(defun autohide-tabbar ()
  "Make tabbar briefly show itself while you are switching
 buffers with shortcuts.  Tested with GNU Emacs 25.0.50.1."
  (interactive)
  (defvar *tabbar-autohide-delay* 3)
  (tabbar-mode -1)
  (defvar *tabbar-autohide-timer* nil)
  (defun renew-tabbar-autohide-timer (seconds-to-show)
    (if (timerp *tabbar-autohide-timer*)
        (cancel-timer *tabbar-autohide-timer*))
    (setf *tabbar-autohide-timer*
          (run-with-timer
           seconds-to-show nil
           (lambda () (tabbar-mode -1)
             (setf *tabbar-autohide-timer* nil)))))
  (global-set-key
   (kbd "H-]")
   (lambda ()
     (interactive)
     (if (null tabbar-mode)
         (progn
           (tabbar-mode 1)
           (tabbar-forward-tab))
       (tabbar-forward-tab))
     (renew-tabbar-autohide-timer 4)))
  (global-set-key
   (kbd "H-[")
   (lambda ()
     (interactive)
     (if (null tabbar-mode)
         (progn
           (tabbar-mode 1)
           (tabbar-backward-tab))
       (tabbar-backward-tab))
     (renew-tabbar-autohide-timer 4)))
  (global-set-key
   (kbd "H-}")
   (lambda ()
     (interactive)
     (if (null tabbar-mode)
         (progn
           (tabbar-mode 1)
           (tabbar-forward-group))
       (tabbar-forward-group))
     (renew-tabbar-autohide-timer 5)))
  (global-set-key
   (kbd "H-{")
   (lambda ()
     (interactive)
     (if (null tabbar-mode)
         (progn
           (tabbar-mode 1)
           (tabbar-backward-group))
       (tabbar-backward-group))
     (renew-tabbar-autohide-timer 5)))
  (global-set-key
   (kbd "H-1")
   (lambda ()
     (interactive)
     (if (null tabbar-mode)
         (progn
           (tabbar-mode 1)
           (tabbar-ruler-group-user-buffers))
       (tabbar-ruler-group-user-buffers))
     (renew-tabbar-autohide-timer 3)))
  (global-set-key
   (kbd "H-2")
   (lambda ()
     (interactive)
     (if (null tabbar-mode)
         (progn
           (tabbar-mode 1)
           (tabbar-ruler-group-by-projectile-project))
       (tabbar-ruler-group-by-projectile-project))
     (renew-tabbar-autohide-timer 3)))
  (global-set-key
   (kbd "H-\`")
   (lambda ()
     (interactive)
     (if (null tabbar-mode)
         (progn
           (tabbar-mode 1)
           (tabbar-ruler-move))
       (tabbar-mode -1)))))

(autohide-tabbar)

(defun tabbar-switch-grouping-method (&optional arg)
  "Changes grouping method of tabbar to grouping by dir.
With a prefix arg, changes to grouping by major mode."
  (interactive "P")
  (ignore-errors
    (if arg
        (setq tabbar-buffer-groups-function 'tabbar-buffer-groups) ;; the default setting
      (setq tabbar-buffer-groups-function 'tabbar-buffer-groups-by-dir))))

(setq tabbar-add-tab-function 'tabbar-add-tab)
(add-hook 'after-change-major-mode-hook 'tabbar-install-faces)

;; Add a buffer modification state indicator in the tab label, and place a
;; space around the label to make it looks less crowd.
(defadvice tabbar-buffer-tab-label (after fixup_tab_label_space_and_flag activate)
  (setq ad-return-value
        (if (and (buffer-modified-p (tabbar-tab-value tab))
               (buffer-file-name (tabbar-tab-value tab)))
            (concat " [" (concat ad-return-value "] "))
          (concat " " (concat ad-return-value " ")))))

;;(tabbar-ruler-group-user-buffers)


(provide 'init-tabbar)
