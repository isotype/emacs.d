;;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Author: Anton Strilchuk <anton@isoty.pe>                       ;;;
;;; URL: http://isoty.pe                                           ;;;
;;; Created: 12-04-2014                                            ;;;
;;; Last-Updated: 17-12-2014                                       ;;;
;;;   By: Anton Strilchuk <anton@env.sh>                           ;;;
;;;                                                                ;;;
;;; Filename: init-auto-complete                                   ;;;
;;; Version:                                                       ;;;
;;; Description:                                                   ;;;
;;;                                                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'popup)
(require-package 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)
(diminish 'auto-complete-mode " ⌦")
(setq-default ac-quick-help-prefer-pos-tip nil)
(setq-default ac-expand-on-auto-complete nil)
(setq-default ac-auto-start t)
(setq-default ac-dwim nil) ; To get pop-ups with docs even if a word is uniquely completed
;;(define-key ac-complete-mode-map "\r" nil)
;;(define-key ac-complete-mode-map [return] nil)

;;----------------------------------------------------------------------------
;; Use Emacs' built-in TAB completion hooks to trigger AC (Emacs >= 23.2)
;;----------------------------------------------------------------------------
(setq tab-always-indent 'complete)  ;; use 't when auto-complete is disabled
(add-to-list 'completion-styles 'initials t)
;; Stop completion-at-point from popping up completion buffers so eagerly
(setq completion-cycle-threshold 3)

;; TODO: find solution for php, haskell and other modes where TAB always does something

(setq c-tab-always-indent nil
      c-insert-tab-function 'indent-for-tab-command)

;; hook AC into completion-at-point
(defun sanityinc/auto-complete-at-point ()
  (when (and (not (minibufferp))
             (fboundp 'auto-complete-mode)
             auto-complete-mode)
    (auto-complete)))

(defun sanityinc/never-indent ()
  (set (make-local-variable 'indent-line-function) (lambda () 'noindent)))

(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions
        (cons 'sanityinc/auto-complete-at-point
              (remove 'sanityinc/auto-complete-at-point completion-at-point-functions))))

(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

(set-default 'ac-sources
             '(ac-source-imenu
               ac-source-dictionary
               ac-source-yasnippet
               ac-source-words-in-buffer
               ac-source-words-in-same-mode-buffers
               ac-source-semantic))

(dolist (mode '(magit-log-edit-mode
                log-edit-mode
                text-mode
                haml-mode
                git-commit-mode
                sass-mode
                yaml-mode
                csv-mode
                espresso-mode
                haskell-mode
                html-mode
                nxml-mode
                sh-mode
                smarty-mode
                clojure-mode
                lisp-mode
                textile-mode
                markdown-mode
                tuareg-mode
                js3-mode
                css-mode
                less-css-mode
                sql-mode
                sql-interactive-mode
                inferior-emacs-lisp-mode))
  (add-to-list 'ac-modes mode))

;; Exclude very large buffers from dabbrev
(defun sanityinc/dabbrev-friend-buffer (other-buffer)
  (< (buffer-size other-buffer) (* 1 1024 1024)))

(setq dabbrev-friend-buffer-function 'sanityinc/dabbrev-friend-buffer)


(define-key ac-completing-map (kbd "M-h") 'ac-quick-help)

;;; Docstring popup anywhere
;;From
(require-package 'popup)
(defun describe-thing-in-popup ()
  (interactive)
  (let* ((thing (symbol-at-point))
         (help-xref-following t)
         (description (with-temp-buffer
                        (help-mode)
                        (help-xref-interned thing)
                        (buffer-string))))
    (popup-tip description
               :point (point)
               :around t
               :height 30
               :scroll-bar t
               :margin t)))

(global-set-key (kbd "H-x h") 'describe-thing-in-popup)

;;,----
;;| YAS
;;`----
(require-package 'yasnippet)

(defun yas-popup-isearch-prompt (prompt choices &optional display-fn)
  (when (featurep 'popup)
    (popup-menu*
     (mapcar
      (lambda (choice)
        (popup-make-item
         (or (and display-fn (funcall display-fn choice)) choice)
         :value choice))
      choices)
     :prompt prompt
     ;; start isearch mode immediately
     :isearch t)))

(add-hook 'prog-mode-hook (lambda () (yas-minor-mode)))
(setq yas-prompt-functions '(yas-popup-isearch-prompt yas-ido-prompt))

;; -----------------------------------------------------------------------------
;;| YCMD
;; -----------------------------------------------------------------------------
;; (add-to-list 'load-path
;;              (expand-file-name "submodules/ycmd" user-emacs-directory))
;; (require 'ycmd)
;; (require 'company-ycmd)
;; (ycmd-setup)
;; (set-variable 'ycmd-server-command '("python" "/Users/anton/.vim/bundle/ycmd"))

;; -----------------------------------------------------------------------------

(provide 'init-auto-complete)
