;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Filename: init-atlassian                                       ;;;
;;; Created: 06-03-2015                                            ;;;
;;; Author: Anton Strilchuk <anton@env.sh>                         ;;;
;;; URL: http://ype.env.sh                                         ;;;
;;; Version: 0.0.0.0.0.0.1                                         ;;;
;;; Last-Updated: 17-03-2015                                       ;;;
;;;  Update #: 18                                                  ;;;
;;;   By: Anton Strilchuk <anton@env.sh>                           ;;;
;;;                                                                ;;;
;;; Description: Do Atlassian Apps the right way :)                ;;;
;;;                                                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init-atlassian)


;;+===============================+;;
;;| Confluence Major-Mode         |;;
;;| Source: [[https://github.com/lispnik/confluence-el][lispnik/confluence-el]] |;;
;;+===============================+;;

;; assuming confluence.el and xml-rpc.el are in your load path
(require-package 'confluence)
(require 'confluence)

;; confluence customization
(setq confluence-url "https://i.adaptavist.com/rpc/xmlrpc"
      confluence-default-space-alist (list (cons confluence-url "~astrilchuk"))
      confluence-xml-convert-to-wiki-on-load t)

(after-load "confluence"
  (progn (add-hook 'confluence-mode-hook '(lambda () (local-set-key "\C-j" 'confluence-newline-and-indent)))))

;; setup confluence mode
(add-hook 'confluence-mode-hook
          '(lambda () (local-set-key "\C-xw" confluence-prefix-map)))


;;+==================================+;;
;;| OrgMode Export Confluence MarkUp |;;
;;+==================================+;;
;; Source: [[https://github.com/hgschmie/org-confluence][hgschmie/org-confluence]]

;; (el-get-bundle 'hgschmie/org-confluence)
;; (add-to-list 'load-path (expand-file-name "el-get/org-confluence" user-emacs-directory))
;; (require 'org-confluence)

;;+=================+;;
;;| JIRA in OrgMode |;;
;;+=================+;;
(require-package 'org-jira)
(require 'jiralib)

(defun sort-jira (file)
  (with-temp-buffer
    (when (file-readable-p file)
      (find-file file)
      (org-mode)
      (goto-char (point-min))
      (org-sort-entries nil ?R nil nil "ID")
      (org-shifttab)
      (when (file-writable-p file)
        (write-file file nil)))))

(defun clean-jira ()
  "Cleans JIRA Issues from retrieved via JQL Query"
  (defconst jira-dir (directory-file-name "~/.org-jira/")
    "Default JIRA Directory")
  (let ((files (directory-files jira-dir nil nil t)))
    (dolist (file files)
      (unless (member file '("." ".." ".DS_Store" "projects-list.org" "my-ticks.org"))
        (sort-jira (concat jira-dir "/" file))))))

(setq jira-host "adaptavist.com"
      jira-jql "SUPPORT: Task List"
      jiralib-host "Adaptavist"
      jiralib-url "https://tracker.adaptavist.com"
      jiralib-wsdl-descriptor-url (concat jiralib-url "/rpc/soap/jirasoapservice-v2?wsdl"))

(defun jira-grab-adaptavist ()
  (org-jira-get-issues-from-filter jira-jql)
  (clean-jira))

;; (defun jira-get-issues (host)
;;   "Interactive function to get JIRA issues"
;;   (interactive "s [a]daptavist or [d]ell?: ")
;;   (let ((jira-hostname host))
;;     (when (equal jira-hostname "a")
;;       (setq jira-host "tracker.adaptavist.com")
;;       (setq jira-jql "SUPPORT: bigquery"))
;;     (when (equal jira-hostname "d")
;;       (setq jira-host "jira.labs.dell.com")
;;       (setq jira-jql "Adaptavist Support JQL Filter"))
;;     (unless (equal jira-host nil)
;;       (setq jiralib-host jira-host)
;;       (setq jiralib-url (concat "https://" jira-host))
;;       (setq jiralib-wsdl-descriptor-url (concat jiralib-url "/rpc/soap/jirasoapservice-v2?wsdl"))
;;       (unless (equal jira-jql nil)
;;         (org-jira-get-issues-from-filter jira-jql)
;;         (clean-jira)))))

;;---------------------------------------------------------------------
;; Optional Timer
;;---------------------------------------------------------------------
;; Run a timer every 30 minutes and update all my jira issues
;;
;;(run-with-timer 0 (* 60 30) 'jira-get-issues)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;init-atlassian.el ends here
