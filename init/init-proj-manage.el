;;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Author: Anton Strilchuk <anton@isoty.pe>                       ;;;
;;; URL: http://isoty.pe                                           ;;;
;;; Created: 23-04-2014                                            ;;;
;;; Last-Updated: 27-12-2014                                       ;;;
;;;   By: Anton Strilchuk <anton@env.sh>                           ;;;
;;;                                                                ;;;
;;; Filename: init-proj-manage                                     ;;;
;;; Version:                                                       ;;;
;;; Description:                                                   ;;;
;;;                                                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require-package 'projectile)
(require 'projectile)
(defun http-server-in-project (port)
  (interactive "nPort: ")
  (require 'projectile)
  (let ((default-directory (projectile-project-root)))
    (start-process "http.server" "*http.server*"
                   "python3" "-m" "http.server" (number-to-string port))))

(require-package 'neotree)
(global-set-key (kbd "A-g") 'neotree-toggle)

(projectile-global-mode)
(setq projectile-indexing-method 'native
      projectile-enable-caching t
      projectile-file-exists-remote-cache-expire nil)

;; OrgMode Projectile
(require-package 'org-projectile)
(require 'org-projectile)
(setq org-projectile:projects-file
      "~/Dropbox/org-project-todos/projects.org")
(add-to-list 'org-capture-templates (org-projectile:project-todo-entry))
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c n p") 'org-projectile:project-todo-completing-read)

(provide 'init-proj-manage)
