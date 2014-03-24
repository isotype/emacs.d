;;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Author: Anton Strilchuk <anton@isoty.pe>                       ;;;
;;; URL: http://isoty.pe                                           ;;;
;;; Created: 24-03-2014                                            ;;;
;;; Last-Updated: 24-03-2014                                       ;;;
;;;   By: Anton Strilchuk <anton@isoty.pe>                         ;;;
;;;                                                                ;;;
;;; Filename: init-theme                                           ;;;
;;; Description: Setup for: Color-theme, Powerline, and tabbars    ;;;
;;;                                                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Powerline
(require 'powerline)
(powerline-vim-theme)

;;; Color-theme
(defun light ()
  (interactive)
  (load-theme 'solarized-light t)
  (powerline-solarized-light))

(defun dark ()
  (interactive)
  (load-theme 'solarized-dark t)
  (powerline-solarized-dark))

(defun powerline-solarized-light ()
  "Function to change powerline to look nice with solarized-light"
  (setq powerline-color1 "#eee8d5")
  (setq powerline-color2 "#fdf6e3")
  (set-face-attribute 'mode-line nil
                      :foreground "#93a1a1"
                      :background "#eee8d5"
                      :box nil
                      :inverse-video nil)
  (set-face-attribute 'mode-line-inactive nil
                      :foreground "#eee8d5"
                      :background "#fdf6e3"
                      :box nil))

(defun powerline-solarized-dark ()
  "Function to change powerline to look nice with solarized-dark"
  (setq powerline-color1 "#073642")
  (setq powerline-color2 "#002B36")
  (set-face-attribute 'mode-line nil
                      :foreground "#fdf6e3"
                      :background "#073642"
                      :box nil
                      :inverse-video nil)
  (set-face-attribute 'mode-line-inactive nil
                      :foreground "#93a1a1"
                      :background "#586e75"
                      :box nil))

(provide 'init-theme)
