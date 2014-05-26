;;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Author: Anton Strilchuk <anton@isoty.pe>                       ;;;
;;; URL: http://isoty.pe                                           ;;;
;;; Created: 17-04-2014                                            ;;;
;;; Last-Updated: 25-05-2014                                       ;;;
;;;   By: Anton Strilchuk <anton@isoty.pe>                         ;;;
;;;                                                                ;;;
;;; Filename: init-docker                                          ;;;
;;; Description: Config for handling dockerfiles                   ;;;
;;;                                                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ------------------------------------------------------- ;;
;; A Dockerfile mode for emacs                             ;;
;; Adds syntax highlighting as well as the ability to      ;;
;; build the image directly (C-c C-b) from the buffer.     ;;
;; You can specify the image name in the file itself by    ;;
;; adding a line like this at the top of your Dockerfile.  ;;
;; ## -*-- docker-image-name: your-image-name-here -*-     ;;
;; If you don't, you'll be prompted for an image name each ;;
;; time you build.                                         ;;
;; ------------------------------------------------------- ;;
(require-package 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
