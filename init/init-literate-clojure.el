;;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Author: Anton Strilchuk <anton@isoty.pe>                       ;;;
;;; URL: http://isoty.pe                                           ;;;
;;; Created: 23-04-2014                                            ;;;
;; Last-Updated: 03-11-2014                                         ;;
;;   By: Anton Strilchuk <anton@env.sh>                             ;;
;;;                                                                ;;;
;;; Filename: init-literate-clojure                                ;;;
;;; Version:                                                       ;;;
;;; Description:                                                   ;;;
;;;                                                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;This file must be placed after initial clojure setup
(require-package 'org)
(require 'ob)
(require 'ob-tangle)
(require 'ob-clojure)
(add-to-list 'org-babel-tangle-lang-exts '("clojure" . "clj"))

(defvar org-babel-default-header-args:clojure
  '((:results . "silent")))

;;Use cider as clojure execution backend
(setq org-babel-clojure-backend 'cider)

;; Pretty source code blocks
(setq org-edit-src-content-indentation 0
      org-src-tab-acts-natively t
      org-src-fontify-natively t
      org-confirm-babel-evaluate nil)

;;Cider config
(setq nrepl-hide-special-buffers t
      cider-repl-pop-to-buffer-on-connect nil
      cider-popup-stacktraces nil
      cider-repl-popup-stacktraces t)

;;Helpful keybindings
(org-defkey org-mode-map "\C-x\C-e" 'cider-eval-last-sexp)
(org-defkey org-mode-map "\C-c\C-d" 'cider-doc)

;;Clojure quick structure template
(defvar clj-name "placeholder-filename")

(defun set-clj-name (filename)
  (interactive "sEnter Org Template CLJ export name: ")
  (message "CLJ: %s" filename)
  (after-load 'org
    '(progn
       (add-to-list 'org-structure-template-alist
                    `("m"
                      ,(concat "#+BEGIN_SRC clojure :exports none :tangle ../src/" filename " :padline no :results silent :noweb yes\n?\n#+END_SRC")
                      "")))))

(after-load 'org
  '(progn
     (add-to-list 'org-structure-template-alist
                  `("m"
                    ,(concat "#+BEGIN_SRC clojure :exports none :tangle ../src/" clj-name " :padline no :results silent :noweb yes\n?\n#+END_SRC")
                    ""))))

(provide 'init-literate-clojure)
