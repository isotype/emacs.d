;; Block Use Of Arrow Keys
;; (Global-unset-key (kbd "<left>"))
;; (global-unset-key (kbd "<right>"))
;; (global-unset-key (kbd "<up>"))
;; (global-unset-key (kbd "<down>"))
;; (global-unset-key (kbd "<C-left>"))
;; (global-unset-key (kbd "<C-right>"))
;; (global-unset-key (kbd "<C-up>"))
;; (global-unset-key (kbd "<C-down>"))
;; (global-unset-key (kbd "<M-left>"))
;; (global-unset-key (kbd "<M-right>"))
;; (global-unset-key (kbd "<M-up>"))
;; (global-unset-key (kbd "<M-down>"))

;;Key modifiers
(unless *is-x-toolkit*
  (setq ns-command-modifier 'meta)
  (setq ns-option-modifier 'super)
  (setq ns-right-command-modifier 'hyper)
  (setq ns-right-option-modifier 'alt)
  (setq ns-right-control-modifier 'nil)
  (setq default-input-method "MacOSX")
  ;; Make mouse wheel / trackpad scrolling less jerky
  (setq mouse-wheel-scroll-amount '(1
                                    ((shift) . 5)
                                    ((control))))
  (global-set-key (kbd "S-h") 'ns-do-hide-emacs)
  (after-load 'nxml-mode
    (define-key nxml-mode-map (kbd "M-h") nil))
  (global-set-key (kbd "M-ˍ") 'ns-do-hide-others))

(global-set-key (kbd "A-a") 'backward-sentence)
(global-set-key (kbd "A-e") 'forward-sentence)

(global-set-key (kbd "C-\=") 'set-mark-command)

;;Switch between two (or more) emacs windows
;;(global-set-key (kbd "C-s-<right>") 'other-frame)
;;(global-set-key (kbd "C-s-<left>") 'other-frame)

;;Recent file open
(global-set-key (kbd "A-x /") 'recentf-open-files)

;;Comment out
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)

;;SMEX Keys
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-x x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;;Multiple Cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;;Orgmode
(define-key global-map (kbd "H-a") 'org-agenda)
(define-key global-map (kbd "H-i") 'org-clock-in)
(define-key global-map (kbd "H-o") 'org-clock-out)
(define-key global-map (kbd "H-x o l") 'org-store-link)
(define-key global-map (kbd "H-x o i") 'org-insert-link)
(define-key global-map (kbd "H-x o a") 'org-archive-set-tag)
(define-key global-map (kbd "H-f") 'org-refile)
(define-key global-map (kbd "H-x o o") 'org-refile-goto-last-stored)
(define-key global-map (kbd "H-x o r") 'org-capture)
(define-key global-map (kbd "H-x o p") 'ype/phone-call)
(define-key global-map (kbd "H-x o d") 'ype/clock-in-default-task-as-default)
(define-key global-map (kbd "H-x o e") 'ype/clock-in-default-email)
(define-key global-map (kbd "H-x o c") 'ype/clock-in-default-elisp)
(define-key global-map (kbd "H-x o s") 'ype/clock-in-default-school)
(define-key global-map (kbd "H-x o t") 'org-clock-select-task)
(define-key global-map (kbd "H-x e") 'org-set-effort)

;;MAP SET/JUMP Bookmarks fast nav
(global-set-key (kbd "A-q") '(lambda () (interactive) (bookmark-set "SAVED")))
(global-set-key (kbd "A-w") '(lambda () (interactive) (bookmark-jump "SAVED")))

;;Helm
(global-set-key (kbd "C-c c") 'ac-complete-with-helm)
;;Helm projectile
(global-set-key (kbd "C-c h") 'helm-projectile)

;;Helm gtags lookup
(global-set-key (kbd "A-x l") 'helm-gtags-select)
;;Helm jump to definition
(global-set-key (kbd "A-x j") 'helm-gtags-find-tag)

;; Helm Kill Ring
(global-set-key (kbd "C-c C-SPC") 'helm-show-kill-ring)

;;Flyspell
(global-set-key (kbd "M-s p") 'ispell-region)
(global-set-key (kbd "M-s M-s") 'ispell-word)

;;Run shit in iterm tmux
;; (global-set-key (kbd "H-x d") 'ype/lldb-tmux-send)
;; (global-set-key (kbd "H-x r") 'ype/lldb-tmux-run)
;; (global-set-key (kbd "H-x t") 'ype/tmux-any-send)
;; (global-set-key (kbd "H-x x") 'ype/xcodebuild)

;;Sudo Search
(defun ype/sudo-find()
  (interactive)
  (find-file "/sudo::/etc/passwd"))
(global-set-key (kbd "H-x f") 'ype/sudo-find)

(provide 'init-keybindings)
