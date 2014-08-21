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

(defun tabbar-switch-grouping-method (&optional arg)
  "Changes grouping method of tabbar to grouping by dir.
With a prefix arg, changes to grouping by major mode."
  (interactive "P")
  (ignore-errors
    (if arg
      (setq tabbar-buffer-groups-function 'tabbar-buffer-groups) ;; the default setting
        (setq tabbar-buffer-groups-function 'tabbar-buffer-groups-by-dir))))

(global-set-key (kbd "C-c C-\;") 'tabbar-ruler-group-buffer-groups)
(global-set-key (kbd "C-c C-\'") 'tabbar-ruler-group-by-projectile-project)


(setq tabbar-add-tab-function 'tabbar-add-tab)
(add-hook 'after-change-major-mode-hook 'tabbar-install-faces)

;;Switch tabbar to show by major-mode
(defun ype/tabbar-group-by-mode ()
  (interactive)
  (tabbar-switch-grouping-method '1))

;;Buffer Switch Remap
(global-set-key (kbd "H-[") 'tabbar-backward-tab)
(global-set-key (kbd "H-]") 'tabbar-forward-tab)
(global-set-key (kbd "H-{") 'tabbar-backward-group)
(global-set-key (kbd "H-}") 'tabbar-forward-group)

 ;; Add a buffer modification state indicator in the tab label, and place a
 ;; space around the label to make it looks less crowd.
 (defadvice tabbar-buffer-tab-label (after fixup_tab_label_space_and_flag activate)
   (setq ad-return-value
         (if (and (buffer-modified-p (tabbar-tab-value tab))
                  (buffer-file-name (tabbar-tab-value tab)))
             (concat " [" (concat ad-return-value "] "))
           (concat " " (concat ad-return-value " ")))))


(provide 'init-tabbar)
