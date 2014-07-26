;;Tabs
(require-package 'tabbar)
(require-package 'tabbar-ruler)
(tabbar-mode t) ;; turn on the tabbar
(tabbar-install-faces t)

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

;; (defun tabbar-switch-grouping-method (&optional arg)
;;   "Changes grouping method of tabbar to grouping by dir.
;; With a prefix arg, changes to grouping by major mode."
;;   (interactive "P")
;;   (ignore-errors
;;     (if arg
;;       (setq tabbar-buffer-groups-function 'tabbar-buffer-groups) ;; the default setting
;;         (setq tabbar-buffer-groups-function 'tabbar-buffer-groups-by-dir))))

(global-set-key (kbd "C-c C-\;") 'tabbar-ruler-group-buffer-groups)
(global-set-key (kbd "C-c C-\'") 'tabbar-ruler-group-by-projectile-project)


(setq tabbar-add-tab-function 'tabbar-add-tab)
(add-hook 'after-change-major-mode-hook 'tabbar-install-faces)

;;,----------------
;;| TABBAR AUTOHIDE
;;`----------------
;; Modified from: http://www.emacswiki.org/emacs/TabBarMode#toc15
;;-----------------------------------------------------------------------------

;; (defun autohide-tabbar ()
;;   "Make tabbar briefly show itself while you are switching
;;  buffers with shortcuts.  Tested with GNU Emacs 23."
;;   (interactive)
;;   (defvar *tabbar-autohide-delay* 3)
;;   (tabbar-mode nil)
;;   (defvar *tabbar-autohide-timer* nil)
;;   (defun renew-tabbar-autohide-timer ()
;;     (if (timerp *tabbar-autohide-timer*)
;;         (cancel-timer *tabbar-autohide-timer*))
;;     (setf *tabbar-autohide-timer*
;;           (run-with-timer 3 nil (lambda ()
;;                                 (tabbar-mode nil)
;;                                 (setf *tabbar-autohide-timer* nil)))))
;;   (global-set-key
;;    [C-next]
;;    (lambda () (interactive)
;;      (if tabbar-mode
;;          (tabbar-forward)
;;        (tabbar-mode t))
;;      (renew-tabbar-autohide-timer)))
;;   (global-set-key
;;    [C-prior]
;;    (lambda () (interactive)
;;      (if tabbar-mode
;;          (tabbar-backward)
;;        (tabbar-mode t))
;;      (renew-tabbar-autohide-timer))))

;; (autohide-tabbar)


(provide 'init-tabbar)
