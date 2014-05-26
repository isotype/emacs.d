;;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Author: Anton Strilchuk <anton@isoty.pe>                       ;;;
;;; URL: http://isoty.pe                                           ;;;
;;; Created: 07-04-2014                                            ;;;
;;; Last-Updated: 25-05-2014                                       ;;;
;;;   By: Anton Strilchuk <anton@isoty.pe>                         ;;;
;;;                                                                ;;;
;;; Filename: init-git                                             ;;;
;;; Description: Git Setup                                         ;;;
;;;                                                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require-package 'magit)
(require-package 'git-gutter-fringe)
(require-package 'git-commit-mode)
(require-package 'git-rebase-mode)
(require-package 'gitignore-mode)
(require-package 'gitconfig-mode)
(require-package 'git-messenger)
(require-package 'gh)

(setq-default
 magit-save-some-buffers nil
 magit-process-popup-time 10
 magit-diff-refine-hunk t
 magit-completing-read-function 'magit-ido-completing-read)

;; Hint: customize `magit-repo-dirs' so that you can use H-s to
;; quickly open magit on any one of your projects.
;;Magit
(global-set-key (kbd "H-s") 'magit-status)
(global-set-key (kbd "H-l") 'magit-log)

(after-load 'magit
  (define-key magit-status-mode-map (kbd "C-M-<up>") 'magit-goto-parent-section))

(global-set-key (kbd "H-b") 'magit-blame-mode)

(require-package 'fullframe)
(after-load 'magit
  (fullframe magit-status magit-mode-quit-window))

;; When we start working on git-backed files, use git-wip if available
(after-load 'magit
  (global-magit-wip-save-mode 1)
  (diminish 'magit-wip-save-mode))

(after-load 'magit
  (diminish 'magit-auto-revert-mode))

;; Use the fringe version of git-gutter
(after-load 'git-gutter
  (require-package 'git-gutter-fringe)
  (setq git-gutter:lighter " GG")
  (global-git-gutter-mode +1)
  (set-face-foreground 'git-gutter-fr:added "#B4C342")
  (set-face-foreground 'git-gutter-fr:modified "#F2804F")
  (set-face-foreground 'git-gutter-fr:deleted "#990A1B"))

(when *is-a-mac*
  (after-load 'magit
    (add-hook 'magit-mode-hook (lambda () (local-unset-key [(meta h)])))))

;; git-svn support
(require-package 'magit-svn)
(after-load 'magit-key-mode
  (require-package 'magit-svn))

(after-load 'compile
  (dolist (defn (list '(git-svn-updated "^\t[A-Z]\t\\(.*\\)$" 1 nil nil 0 1)
                      '(git-svn-needs-update "^\\(.*\\): needs update$" 1 nil nil 2 1)))
    (add-to-list 'compilation-error-regexp-alist-alist defn)
    (add-to-list 'compilation-error-regexp-alist (car defn))))

(defvar git-svn--available-commands nil "Cached list of git svn subcommands")

(defun git-svn (dir)
  "Run a git svn subcommand in DIR."
  (interactive "DSelect directory: ")
  (unless git-svn--available-commands
    (setq git-svn--available-commands
          (sanityinc/string-all-matches
           "^  \\([a-z\\-]+\\) +"
           (shell-command-to-string "git svn help") 1)))
  (let* ((default-directory (vc-git-root dir))
         (compilation-buffer-name-function (lambda (major-mode-name) "*git-svn*")))
    (compile (concat "git svn "
                     (ido-completing-read "git-svn command: " git-svn--available-commands nil t)))))

(require-package 'git-messenger)
(global-set-key (kbd "H-p") #'git-messenger:popup-message)

;; github
(require-package 'gist) ; for gist-list
(require-package 'yagist) ; for yagist-region-or-buffer...
(global-set-key (kbd "H-1") 'yagist-region-or-buffer-private)
(global-set-key (kbd "H-2") 'yagist-region-or-buffer)
(global-set-key (kbd "H-\`") 'gist-list)

(require-package 'github-browse-file)
(require-package 'bug-reference-github)
(add-hook 'prog-mode-hook 'bug-reference-prog-mode)

;; Org-Sync Github
(mapc 'load
      '("os" "os-github"))

(provide 'init-git)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-git.el ends here
