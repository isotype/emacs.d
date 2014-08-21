;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author: Anton Strilchuk <ype@env.sh>                             ;;
;; URL: http://ype.env.sh                                           ;;
;; Created: 30-06-2014                                              ;;
;; Last-Updated: 07-08-2014                                         ;;
;;  Update #: 18                                                    ;;
;;   By: Anton Strilchuk <ype@env.sh>                               ;;
;;                                                                  ;;
;; Filename: init-ledger                                            ;;
;; Description: Conf for ledger-mode                                ;;
;;                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require-package 'ledger-mode)
(require-package 'flycheck-ledger)

(add-to-list 'auto-mode-alist '("\\.ledger\\.journal\\.hledger" . ledger-mode))

;; (eval-after-load 'flycheck
;;   '(require 'flycheck-ledger))

(setq flycheck-ledger-executable "/usr/local/bin/ledger")

(after-load 'ledger-mode
  (define-key ledger-mode-map (kbd "RET") 'newline)
  (define-key ledger-mode-map (kbd "C-o") 'open-line))

(setq ledger-highlight-xact-under-point nil
      ledger-use-iso-dates nil)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-copy-env "LEDGER_FILE"))

(add-hook 'ledger-mode-hook 'goto-address-prog-mode)

(provide 'init-ledger)
