;;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Author: Anton Strilchuk <anton@isoty.pe>                       ;;;
;;; URL: http://isoty.pe                                           ;;;
;;; Created: 03-04-2014                                            ;;;
;; Last-Updated: 16-11-2014                                         ;;
;;   By: Anton Strilchuk <anton@env.sh>                             ;;
;;;                                                                ;;;
;;; Filename: init-paredit                                         ;;;
;;; Description: Config for Paredit (From: github:purcell)         ;;;
;;;                                                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require-package 'paredit)
(autoload 'enable-paredit-mode "paredit")

(defun maybe-map-paredit-newline ()
  (unless (or (memq major-mode '(inferior-emacs-lisp-mode cider-repl-mode))
             (minibufferp))
    (local-set-key (kbd "RET") 'paredit-newline)))

(add-hook 'paredit-mode-hook 'maybe-map-paredit-newline)

(after-load 'paredit
  (diminish 'paredit-mode " ⑊")
  (dolist (binding (list (kbd "C-<left>") (kbd "C-<right>")
                         (kbd "C-M-<left>") (kbd "C-M-<right>")))
    (define-key paredit-mode-map binding nil))

  ;; Disable kill-sentence, which is easily confused with the kill-sexp
  ;; binding, but doesn't preserve sexp structure
  (define-key paredit-mode-map [remap kill-sentence] nil)
  (define-key paredit-mode-map [remap backward-kill-sentence] nil)

  ;; Change Paredit Splice Sexp to Super instead of Meta
  ;; Meta used for window movement
  (define-key paredit-mode-map (kbd "s-<up>") 'paredit-splice-sexp-killing-backward)
  (define-key paredit-mode-map (kbd "s-<down>") 'paredit-splice-sexp-killing-forward)

  ;; Disable
  (define-key paredit-mode-map (kbd "M-<up>") nil)
  (define-key paredit-mode-map (kbd "M-<down>") nil)

  ;; Allow my global binding of M-? to work when paredit is active
  (define-key paredit-mode-map (kbd "M-?") nil)

  ;; Compatibility with other modes
  (suspend-mode-during-cua-rect-selection 'paredit-mode)

  ;; Use paredit in the minibuffer
  ;; TODO: break out into separate package
  ;; http://emacsredux.com/blog/2013/04/18/evaluate-emacs-lisp-in-the-minibuffer/
  (add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode))

(defvar paredit-minibuffer-commands '(eval-expression
                                      pp-eval-expression
                                      eval-expression-with-eldoc
                                      ibuffer-do-eval
                                      ibuffer-do-view-and-eval)
  "Interactive commands for which paredit should be enabled in the minibuffer.")

(defun conditionally-enable-paredit-mode ()
  "Enable paredit during lisp-related minibuffer commands."
  (if (memq this-command paredit-minibuffer-commands)
      (enable-paredit-mode)))

;; Use paredit with autopair
;; (require-package 'autopair)
;; (add-hook 'python-mode-hook
;;           #'(lambda ()
;;               (setq autopair-handle-action-fns
;;                     (list #'autopair-default-handle-action
;;                           #'autopair-python-triple-quote-action))))

;; (defvar autopair-modes '(r-mode ruby-mode python-mode))
;; (defun turn-on-autopair-mode () (autopair-mode 1))
;; (dolist (mode autopair-modes) (add-hook (intern (concat (symbol-name mode) "-hook")) 'turn-on-autopair-mode))

;; (defadvice paredit-mode (around disable-autopairs-around (arg))
;;   "Disable autopairs mode if paredit-mode is turned on"
;;   ad-do-it
;;   (if (null ad-return-value)
;;       (autopair-mode 1)
;;     (autopair-mode 0)
;;     ))

;; (ad-activate 'paredit-mode)

;;,-----------
;;| TOGGLE KEY
;;`-----------
;; Toggle Between Paredit-Mode and autopair-mode using single key
;; TODO: turn into function to use elsewhere
;; (global-set-key (kbd "C-\,")
;;                 (lambda ()
;;                   (interactive)
;;                   (if (or (null paredit-mode)
;;                          (not (null autopair-mode)))
;;                       (progn
;;                         (paredit-mode 1)
;;                         (autopair-mode -1))
;;                     (progn
;;                       (paredit-mode -1)
;;                       (autopair-mode 1)))))

(global-set-key (kbd "C-\, q")
                (lambda ()
                  (interactive)
                  (if (null paredit-mode)
                      (paredit-mode 1)
                    (paredit-mode -1))))

;; ----------------------------------------------------------------------------
;; Enable some handy paredit functions in all prog modes
;; ----------------------------------------------------------------------------
(require-package 'paredit-everywhere)
(add-hook 'prog-mode-hook 'paredit-everywhere-mode)
(add-hook 'css-mode-hook 'paredit-everywhere-mode)

;;,-==================-
;;| Custom Keybindings
;;`-==================-
(global-set-key (kbd "M-\]") 'paredit-forward-slurp-sexp)
(global-set-key (kbd "M-\[") 'paredit-backward-slurp-sexp)
(global-set-key (kbd "s-\]") 'paredit-forward-barf-sexp)
(global-set-key (kbd "s-\[") 'paredit-backward-barf-sexp)



(provide 'init-paredit)
