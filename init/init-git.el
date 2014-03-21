;;; init-git.el --- 
;; 
;; Filename: init-git.el
;; Description: 
;; Author: y_pe
;; Maintainer: 
;; Created: Sun Feb 16 19:18:34 2014 (+0000)
;; Version: 
;; Package-Requires: ()
;; Last-Updated: Wed Mar 19 09:27:59 2014 (+0000)
;;           By: anton
;;     Update #: 15
;; URL: isoty.pe
;; Doc URL: https://github.com/purcell/emacs.d/tree/master/lisp
;; Keywords: git, git-gutter, magit
;; Compatibility: Emacs 24.3.50.1+
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;;  This is a modified version of Steve Purcell's init-git.el
;;  which can be found at the Doc URL. I have made slight changes
;;  one of which is the addition of Peter Seibel's use of
;;  git-gutter-fringe in his emacs.d configuration.
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;;  - Added git-gutter-fringe
;;  - Set color preferences for git-gutter
;;  - Removed Git-wip due to onslaught of file load errors
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:
(require 'magit)
(require 'git-gutter-fringe)
(require 'magit-blame)
(require 'git-commit-mode)
(require 'git-rebase-mode)
(require 'gitignore-mode)
(require 'gitconfig-mode)
(require 'git-messenger)

(setq-default
 magit-save-some-buffers nil
 magit-process-popup-time 10
 magit-diff-refine-hunk t
 magit-completing-read-function 'magit-ido-completing-read)

;; Hint: customize `magit-repo-dirs' so that you can use H-s to
;; quickly open magit on any one of your projects.
;;Magit
(after-load 'magit
  (global-set-key (kbd "H-s") 'magit-status))
(after-load 'magit
  (global-set-key (kbd "H-l") 'magit-log))

(after-load 'magit
  (define-key magit-status-mode-map (kbd "C-M-<up>") 'magit-goto-parent-section))

(after-load 'magit
  (global-set-key (kbd "H-b") 'magit-blame-mode))

(require 'fullframe)
(after-load 'magit
  (fullframe magit-status magit-mode-quit-window))

;; When we start working on git-backed files, use git-wip if available
;; (after-load 'vc-git
;; 	    (global-magit-wip-save-mode 1)
;; 	    (diminish 'magit-wip-save-mode))

;; Use the fringe version of git-gutter
(after-load 'git-gutter
  (require 'git-gutter-fringe)
  (setq git-gutter:lighter " GG")
  (global-git-gutter-mode +1)
  (set-face-foreground 'git-gutter-fr:added "#B4C342")
  (set-face-foreground 'git-gutter-fr:modified "#F2804F")
  (set-face-foreground 'git-gutter-fr:deleted "#990A1B"))

(when *is-a-mac*
  (after-load 'magit
    (add-hook 'magit-mode-hook (lambda () (local-unset-key [(meta h)])))))

;; Convenient binding for vc-git-grep
(global-set-key (kbd "C-x v f") 'vc-git-grep)

;; git-svn support
(require 'magit-svn)
(after-load 'magit-key-mode
  (require 'magit-svn))

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

(require 'git-messenger)
(global-set-key (kbd "C-x v p") #'git-messenger:popup-message)

;; github
(require 'yagist)
(require 'github-browse-file)
(require 'bug-reference-github)
(add-hook 'prog-mode-hook 'bug-reference-prog-mode)

(provide 'init-git)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-git.el ends here
