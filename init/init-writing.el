;;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Author: Anton Strilchuk <anton@isoty.pe>                       ;;;
;;; URL: http://isoty.pe                                           ;;;
;;; Created: 03-05-2014                                            ;;;
;; Last-Updated: 18-09-2014                                         ;;
;;   By: Anton Strilchuk <ype@env.sh>                               ;;
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
    (setq ispell-dictionary "british"
          ispell-silently-savep t)))

;;,---------------
;;| Writegood mode
;;`---------------
(require-git-submodule 'writegood-mode)
(global-set-key "\C-cg" 'writegood-mode)
(global-set-key (kbd "C-c q") 'refill-mode)

;;,---------
;;| TextLint
;;`---------
(require-git-submodule 'textlint t)
(setq textlint-location-textlint "~/.emacs.d/submodules/textlint/")
(global-set-key (kbd "<f2>") 'textlint-run)

;;,--------------------------------------------------
;;| Requires: LanguageTool from http://bit.ly/1yihDih
;;`--------------------------------------------------
(add-to-list 'load-path (expand-file-name "writing-tools" user-emacs-directory))
(require 'langtool)
(setq langtool-language-tool-jar "~/.emacs.d/writing-tools/LanguageTool/LanguageTool.jar")
(setq langtool-mother-tongue "en-GB")
(setq langtool-java-bin "/usr/bin/java")

(global-set-key "\C-x4w" 'langtool-check)
(global-set-key "\C-x4e" 'langtool-check-done)
(global-set-key "\C-x4l" 'langtool-switch-default-language)
(global-set-key "\C-x44" 'langtool-show-message-at-point)
(global-set-key "\C-x4c" 'langtool-correct-buffer)


(provide 'init-writing)
