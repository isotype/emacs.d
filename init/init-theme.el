;;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Author: Anton Strilchuk <anton@isoty.pe>                       ;;;
;;; URL: http://isoty.pe                                           ;;;
;;; Created: 24-03-2014                                            ;;;
;;; Last-Updated: 25-02-2015                                       ;;;
;;;   By: Anton Strilchuk <anton@env.sh>                           ;;;
;;;                                                                ;;;
;;; Filename: init-theme                                           ;;;
;;; Description: Setup for: Color-theme, Powerline, and tabbars    ;;;
;;;                                                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init-theme)
;;;Powerline
(require-git-submodule 'powerline t)
(powerline-vim-theme)

(require-package 'sublime-themes)
(require-package 'leuven-theme)
(add-to-list 'load-path (expand-file-name "ype-emacs-packages/36-symbols-theme/" user-emacs-directory))

;; Change Light Based on OSX Ambient Light Sensor Values
(defvar direct-sun 20000000)
(defvar ambient-light 1)
(defvar dark-theme '36-symbols)
(defvar light-theme 'leuven)

(defun light-level ()
  "Access the level of light detected by the LMU sensor on Macbook Pros"
  (string-to-number (shell-command-to-string "~/.emacs.d/light/LMU-sensor")))

(defun adjust-theme-to-light ()
  "Picks a theme according to the level of ambient light in the room"
  (cond ((= ambient-light 1)
         (if (> (light-level) direct-sun)
             (progn
               (require 'leuven-theme nil t)
               (enable-theme light-theme)
               (disable-theme dark-theme))
           (progn
             (require '36-symbols-theme nil t)
             (enable-theme dark-theme)
             (disable-theme light-theme))))
        ((= ambient-light 0)
         (message "LMU Theme Switcher Disabled"))))

(defun ype:toggle-lmu-theme-switch-on ()
  (interactive)
  (setq ambient-light 1)
  (adjust-theme-to-light))

(defun ype:toggle-lmu-theme-switch-off ()
  (interactive)
  (setq ambient-light 0))

;;,-------------------------------------------------
;;| Change theme based on OSX ambient light sensor
;;| BUG: Occasionally Crashes Emacs
;;|
;;| eg. check light sensor every hour
;;`-------------------------------------------------
(after-load 'init-theme
  (adjust-theme-to-light)
  (run-at-time 180 (* 60 3) 'adjust-theme-to-light))
