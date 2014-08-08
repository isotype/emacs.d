;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author: Anton Strilchuk <ype@env.sh>                             ;;
;; URL: http://ype.env.sh                                           ;;
;; Created: 17-06-2014                                              ;;
;; Last-Updated: 08-08-2014                                         ;;
;;  Update #: 8                                                     ;;
;;   By: Anton Strilchuk <ype@env.sh>                               ;;
;;                                                                  ;;
;; Filename: init-spritz                                            ;;
;; Version:                                                         ;;
;; Description:                                                     ;;
;;                                                                  ;;
;; Package Requires: ()                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;,---------------------------------------------------------
;;| SPRAY: Spritz in Emacs
;;| [[https://github.com/zk-phi/spray][Source]]
;;| --------------------------------------------------------
;;| Commands
;;| In spray-mode buffers, following commands are available.
;;|
;;| spray-start/stop (SPC)
;;|    pause or resume spraying
;;|
;;| spray-backward-word (h, )
;;|    pause and back to the last word
;;|
;;| spray-forward-word (l, )
;;|    inverse of spray-backward-word
;;|
;;| Press any key else to quit spray-mode.
;;`---------------------------------------------------------

(require-git-submodule 'spray)
(require 'spray)
(setq spray-wpm 350)

(global-set-key (kbd "<f6>") 'spray-mode)


(provide 'init-spritz)
