;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author: Anton Strilchuk <ype@env.sh>                             ;;
;; URL: http://ype.env.sh                                           ;;
;; Created: 10-06-2014                                              ;;
;; Last-Updated: 18-06-2014                                         ;;
;;   By: Anton Strilchuk <ype@env.sh>                               ;;
;;                                                                  ;;
;; Filename: init-git-packages                                      ;;
;; Version:                                                         ;;
;; Description: Check/Install Emacs packages from github at init    ;;
;;                                                                  ;;
;; Package Requires: ()                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(require 'request-deferred)
;;(defvar connected nil)
;; (defun ype/check-connection ()
;;   (deferred:$
;;     (request-deferred "http://github.com/")
;;     (deferred:nextc it
;;       (lambda (response)
;;         (let ((temp (request-response-data response))
;;               (status (request-response-status-code response))
;;               (error-msg (request-response-error-thrown response)))
;;           (cond
;;            ((eq status nil)
;;             (setq connected nil)
;;             (message "Connection Unavailable"))
;;            ((eq 401 status)
;;             (message "401"))
;;            ((eq 403 status)
;;             (message "403"))
;;            ((eq 200 status)
;;             (setq connected t)
;;             (print "Everything looks fine"))
;;            ((and (> 299 status) (eq temp nil))
;;             (message (concat "Received HTTP"
;;                              (number-to-string status)
;;                              " Error occured, but no message body.")))
;;            ((not (eq error-msg nil))
;;             (progn (print (format "%s %s"
;;                                   (concat "Status code: "
;;                                           (number-to-string status)) error-msg))))
;;            (t (progn (message "True")))))))))



;;,------------------------------------------
;;| TODO: Add check for internet connectivity
;;`------------------------------------------
;; (defun test-con ()
;;   (interactive)
;;   (setq connected nil)
;;   (when (eq (ype/check-connection) "Everything looks fine")
;;     (message "Success"))
;;   (when (eq connected t)
;;     (message "gogogo")))

(let ((git-el-packages
       (expand-file-name "from-git" user-emacs-directory)))
  (when (and (file-directory-p git-el-packages))
    (message "Removing local package.el from load-path to avoid shadowing bundled version")
    (setq load-path (remove git-el-packages load-path))))

(defun require-git-package (userandrepo)
  "Automagically Download el packages from github"
  (let ((user
         (car (split-string (symbol-name userandrepo) "\/")))
        (repo
         (car (cdr (split-string (symbol-name userandrepo) "\/"))))
        (git-el-packages
         (expand-file-name "from-git" user-emacs-directory))
        (pkg-status
         (quote(default))))
    (with-temp-buffer
      (shell-command
       (concat "git clone git@github.com:" user "/" repo ".git " git-el-packages "/" repo) "*CumGitSum Errors*")
      (add-to-list 'load-path (concat git-el-packages "/" repo)))
    ;; (with-temp-buffer
    ;;   (setq buffstr (with-temp-buffer
    ;;                   (insert-buffer "*CumGitSum Errors*")
    ;;                   (buffer-string)))
    ;;   (goto-char (point-min))
    ;;   (let ((str buffstr))
    ;;     (when (string-match ".*destination path.*already exists.*" str)
    ;;       (message "Package was already installed, checking if update needed")
    ;;       ;;          (shell-command (concat "cd " git-el-packages "/" repo "; " "git pull origin master --dry-run") "*CumGitSum Update*")
    ;;       (setq update-needed (with-temp-buffer
    ;;                             (insert-buffer "*CumGitSum Update*")
    ;;                             (buffer-string)))
    ;;       (let ((update-str update-needed))
    ;;         (when (string-match ".*Current branch master is up to date.*" update-str))))
    ;;     (when (string-match ".*ERROR" str)
    ;;       )
    ;;     (when (string-match "\\bCloning into\\b" str)
    ;;       )
    ;;     ))
    (message "Finished Checking Package, %s" pkg-status)))

(require-git-package 'emacsmirror/org-sync)


(provide 'init-git-packages)
