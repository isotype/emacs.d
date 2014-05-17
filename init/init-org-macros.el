;;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Author: Anton Strilchuk <anton@isoty.pe>                       ;;;
;;; URL: http://isoty.pe                                           ;;;
;;; Created: 17-05-2014                                            ;;;
;;; Last-Updated: 17-05-2014                                       ;;;
;;;   By: Anton Strilchuk <anton@isoty.pe>                         ;;;
;;;                                                                ;;;
;;; Filename: init-org-macros                                      ;;;
;;; Version:                                                       ;;;
;;; Description:                                                   ;;;
;;;                                                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'kanban)
;; Macro to add a kanban table to file
(fset 'kanban
      "
| TODO | DOING | DONE |
|------+-------+------|
|      |       |      |
|      |       |      |
|      |       |      |
#+TBLFM: @1='(kanban-headers $#)::@2$1..@>$>='(kanban-zero @# $# nil (list \"~/path/to/file.org\"))
")

(provide 'init-org-macros)
