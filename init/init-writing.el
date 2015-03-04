;;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Author: Anton Strilchuk <anton@isoty.pe>                       ;;;
;;; URL: http://isoty.pe                                           ;;;
;;; Created: 03-05-2014                                            ;;;
;;; Last-Updated: 10-02-2015                                       ;;;
;;;   By: Anton Strilchuk <anton@env.sh>                           ;;;
;;;                                                                ;;;
;;; Filename: init-writing                                         ;;;
;;; Version:                                                       ;;;
;;; Description:                                                   ;;;
;;;                                                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Speelin Chek
(require-package 'ispell)
(after-load "ispell"
  (progn
    (setq ispell-dictionary "english"
          ispell-silently-savep t)))

;; Synonms
(require-package 'synosaurus)
(require 'synosaurus)
(setq synosaurus-choose-method 'ido
      synosaurus-backend-wordnet)

;;,---------------
;;| Writegood mode
;;`---------------
(el-get-bundle bnbeckwith/writegood-mode)
(global-set-key "\C-cg" 'writegood-mode)
(global-set-key (kbd "C-c q") 'refill-mode)

;;,---------
;;| TextLint
;;`---------
(el-get-bundle DamienCassou/textlint)
(require 'textlint)
(setq textlint-location-textlint "~/.emacs.d/submodules/textlint/")
(define-key ctrl-apos [?t ?l] 'textlint-run)

;;,--------------------------------------------------
;;| Requires: LanguageTool from http://bit.ly/1yihDih
;;`--------------------------------------------------
(add-to-list 'load-path (expand-file-name "writing-tools" user-emacs-directory))
(require 'langtool)
(setq langtool-language-tool-jar "~/.emacs.d/writing-tools/LanguageTool/LanguageTool.jar")
(setq langtool-mother-tongue "en-US")
(setq langtool-java-bin "/usr/bin/java")

(global-set-key "\C-c11" 'langtool-check)
(global-set-key "\C-c12" 'langtool-check-done)
(global-set-key "\C-c13" 'langtool-switch-default-language)
(global-set-key "\C-c21" 'langtool-show-message-at-point)
(global-set-key "\C-c22" 'langtool-correct-buffer)
(global-set-key "\C-c33" 'langtool-goto-next-error)
(global-set-key "\C-c44" 'langtool-goto-previous-error)

;;Syntax analysis - Emacs minor mode
(require-package 'wordsmith-mode)
;;(add-hook 'mu4e-compose-mode-hook 'wordsmith-mode)

;; Predictive Text
(add-to-list 'load-path (expand-file-name "submodules/predictive" user-emacs-directory))
(require 'predictive)
(set-default 'predictive-auto-add-to-dict t)
(setq predictive-main-dict 'rpg-dictionary
      predictive-auto-learn t
      predictive-add-to-dict-ask nil
      predictive-use-auto-learn-cache nil
      predictive-which-dict t)

(when 'predictive-mode
  (define-key ctrl-apos [?t ?p] 'complete-predictive-ispell))

(provide 'init-writing)
