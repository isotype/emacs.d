;;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Author: Anton Strilchuk <anton@isoty.pe>                       ;;;
;;; URL: http://isoty.pe                                           ;;;
;;; Created: 20-05-2014                                            ;;;
;;; Last-Updated: 17-11-2014                                       ;;;
;;;   By: Anton Strilchuk <anton@env.sh>                           ;;;
;;;                                                                ;;;
;;; Filename: init-dash                                            ;;;
;;; Version:                                                       ;;;
;;; Description:                                                   ;;;
;;;                                                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require-package 'helm-dash)

(defun elisp-doc()
  (interactive)
  (setq-local helm-dash-docset '("Emacs_Lisp")))
(add-hook 'emacs-lisp-mode 'elisp-doc)

(defun clisp-doc ()
  (interactive)
  (setq-local helm-dash-docset '("Common_Lisp")))
(add-hook 'common-lisp-mode 'clisp-doc)

(defun ype/helm-pydocs ()
  (interactive)
  (setq-local helm-dash-docsets '("Python_3")))
(add-hook 'python-mode-hook 'ype/helm-pydocs)

(after-load 'helm-dash
  (define-key ctrl-apos (kbd "h d") 'helm-dash)
  (define-key ctrl-apos (kbd "d") 'helm-dash-at-point))

(defun sanityinc/dash-installed-p ()
  "Return t if Dash is installed on this machine, or nil otherwise."
  (let ((lsregister "/System/Library/Frameworks/CoreServices.framework/Versions/A/Frameworks/LaunchServices.framework/Versions/A/Support/lsregister"))
    (and (file-executable-p lsregister)
         (not (string-equal
               ""
               (shell-command-to-string
                (concat lsregister " -dump|grep com.kapeli.dash")))))))

(when (and *is-a-mac* (not (package-installed-p 'dash-at-point)))
  (message "Checking whether Dash is installed")
  (when (sanityinc/dash-installed-p)
    (require-package 'dash-at-point)))

(when (package-installed-p 'dash-at-point)
  (global-set-key (kbd "C-c d") 'dash-at-point))

(provide 'init-dash)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dash.el ends here
