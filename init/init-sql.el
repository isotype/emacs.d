;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Filename: init-sql                                             ;;;
;;; Created: 09-03-2015                                            ;;;
;;; Author: Anton Strilchuk <anton@env.sh>                         ;;;
;;; URL: http://ype.env.sh                                         ;;;
;;; Version:                                                       ;;;
;;; Last-Updated: 09-03-2015                                       ;;;
;;;  Update #: 6                                                   ;;;
;;;   By: Anton Strilchuk <anton@env.sh>                           ;;;
;;;                                                                ;;;
;;; Description:                                                   ;;;
;;;                                                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'sql)

(setq sql-connection-alist
      '((adapt.mysql (sql-product 'mysql)
                     (sql-port 3306)
                     (sql-server "localhost")
                     (sql-user "debian-sys-maint")
                     (sql-database ""))))

(setq sql-mysql-login-params
      '((user :default "debian-sys-maint")
        (database :default "")
        (server :default "localhost")
        (port :default 3306)))

(defun helm-sql:connect-server (connection)
  "Connect to the input server using my-sql-servers-list"
  (interactive
   (helm-comp-read "Select server: " (mapcar (lambda (item) (list (symbol-name (nth 0 item)) (nth 0 item))) sql-connection-alist)))

  ;; password
  (require 'passwords "~/.emacs.d/passwd/passwords.el.gpg")
  ;; get the sql connection info and product from the sql-connection-alist
  (let* ((connection-info (assoc connection sql-connection-alist))
         (connection-product (nth 1 (nth 1 (assoc 'sql-product connection-info))))
         (sql-password (nth 1 (assoc connection pass-adapt-sql))))
    ;; delete the connection info from the sql-connection-alist
    (setq sql-connection-alist (assq-delete-all connection sql-connection-alist))
    ;; delete the old password from the connection-info
    (setq connection-info (assq-delete-all 'sql-password connection-info))
    ;; add the password to the connection-info
    (nconc connection-info `((sql-password ,sql-password)))
    ;; add back the connection info to the beginning of sql-connection-alist
    ;; (last used server will appear first for the next prompt)
    (add-to-list 'sql-connection-alist connection-info)
    ;; override the sql-product by the product of this connection
    (setq sql-product connection-product)
    ;; connect
    (if current-prefix-arg
        (sql-connect connection connection)
      (sql-connect connection))))

(defun sql:save-history-hook ()
  (let ((lval 'sql-input-ring-file-name)
        (rval 'sql-product))
    (if (symbol-value rval)
        (let ((filename (concat "~/.emacs.d/sql/" (symbol-name (symbol-value rval)) "-history.sql")))
          (set (make-local-variable lval) filename))
      (error (format "SQL history will not be saved because %s is nil" (symbol-name rval))))))

(add-hook 'sql-interactive-mode-hook 'sql:save-history-hook)
(add-hook 'sql-interactive-mode-hook (lambda () (toggle-truncate-lines t)))

;; (defadvice sql-mysql (around sql-mysql-around activate)
;;   "SSH to linux, then connect"
;;   (let ((default-directory "/ssh:host.myhost.com:"))
;;     ad-do-it))

(provide 'init-sql)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;init-sql.el ends here
