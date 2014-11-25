;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Author: Anton Strilchuk <anton@isoty.pe>                       ;;;
;;; URL: http://isoty.pe                                           ;;;
;;; Created: 28-04-2014                                            ;;;
;;; Last-Updated: 16-11-2014                                       ;;;
;;;   By: Anton Strilchuk <anton@env.sh>                           ;;;
;;;                                                                ;;;
;;; Filename: init-flycheck                                        ;;;
;;; Version:                                                       ;;;
;;; Description:                                                   ;;;
;;;                                                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require-package 'flycheck)
(require-package 'flycheck-pos-tip)
(add-hook 'after-init-hook 'global-flycheck-mode)

;; Override default flycheck triggers
(setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
      flycheck-idle-change-delay 0.8)

(defun sanityinc/flycheck-errors-visible-p ()
  "Returns t if a window in the current frame is displaying \"*Flycheck errors*\"."
  (let (found-flycheck-errors-buf)
    (walk-window-tree (lambda (w)
                        (when (string-equal "*Flycheck errors*"
                                            (buffer-name (window-buffer w)))
                          (setq found-flycheck-errors-buf t))))
    (selected-frame)
    found-flycheck-errors-buf))

(defun sanityinc/flycheck-maybe-display-errors (errors)
  (unless (sanityinc/flycheck-errors-visible-p)
    (flycheck-display-error-messages errors)))

(setq flycheck-display-errors-function 'flycheck-pos-tip-error-messages)

(global-set-key (kbd "M-2") 'flycheck-next-error)
(global-set-key (kbd "M-1") 'flycheck-previous-error)

;; Add virtualenv support for checkers
(defadvice flycheck-check-executable
    (around python-flycheck-check-executable (checker)
            activate compile)
  "`flycheck-check-executable' with virtualenv support."
  (if (eq major-mode 'python-mode)
      (let* ((process-environment (python-shell-calculate-process-environment))
             (exec-path (python-shell-calculate-exec-path)))
        ad-do-it)
    ad-do-it))

(defadvice flycheck-start-checker
    (around python-flycheck-start-checker (checker)
            activate compile)
  "`flycheck-start-checker' with virtualenv support."
  (if (eq major-mode 'python-mode)
      (let* ((process-environment (python-shell-calculate-process-environment))
             (exec-path (python-shell-calculate-exec-path)))
        ad-do-it)
    ad-do-it))

(add-hook 'python-mode-hook 'flycheck-mode)
;;(add-to-list 'flycheck-disabled-checkers 'python-flake8)
(add-hook 'python-mode-hook #'(lambda () (setq flycheck-checker 'python-pylint)))


;;,------------------
;;| DISABLED WARNINGS
;;`------------------
(after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))



(provide 'init-flycheck)
