;;Tabs
(require 'tabbar)
(tabbar-mode t) ;; turn on the tabbar
(tabbar-install-faces t)

(defun tabbar-buffer-groups-by-dir ()
        "Put all files in the same directory into the same tab bar"
        (with-current-buffer (current-buffer)
          (let ((dir (expand-file-name default-directory)))
            (cond ;; assign group name until one clause succeeds, so the order is important
             ((eq major-mode 'dired-mode)
              (list "Dired"))
             ((memq major-mode
                    '(help-mode apropos-mode Info-mode Man-mode))
              (list "Help"))
             ((string-match-p "\*.*\*" (buffer-name))
              (list "Misc"))
             (t (list dir))))))

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
