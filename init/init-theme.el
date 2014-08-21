;;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Author: Anton Strilchuk <anton@isoty.pe>                       ;;;
;;; URL: http://isoty.pe                                           ;;;
;;; Created: 24-03-2014                                            ;;;
;; Last-Updated: 21-08-2014                                         ;;
;;   By: Anton Strilchuk <ype@env.sh>                               ;;
;;;                                                                ;;;
;;; Filename: init-theme                                           ;;;
;;; Description: Setup for: Color-theme, Powerline, and tabbars    ;;;
;;;                                                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Powerline
(require-package 'powerline)
(powerline-vim-theme)

(require-package 'ample-theme)

(defun ype:light ()
 (interactive)
 (ample-light-theme))

(defun ype:dark ()
 (interactive)
 (ample-theme))

;; Change Light Based on OSX Ambient Light Sensor Values
(setq direct-sun 44000000)

(setq light-theme 'solarized-light)
(setq dark-theme 'ample)

(defun light-level ()
  "Access the level of light detected by the LMU sensor on Macbook Pros"
  (string-to-number (shell-command-to-string "~/.emacs.d/light/LMU-sensor")))

(defun adjust-theme-to-light ()
  "Picks a theme according to the level of ambient light in the room"
  (cond ((= ambient-light 1)
         (if (> (light-level) direct-sun)
             (progn
               (load-theme light-theme t)
               (disable-theme dark-theme))
           (progn
             (load-theme dark-theme t)
             (disable-theme light-theme))))
        ((= ambient-light 0)
         (message "LMU Theme Switcher Disabled"))))

(setq ambient-light 1)
(if (= ambient-light 1)
    't
  'nil)

(defun ype:toggle-lmu-theme-switch-on ()
  (interactive)
  (setq ambient-light 1))

(defun ype:toggle-lmu-theme-switch-off ()
  (interactive)
  (setq ambient-light 0))

(run-at-time 0 (* 60 60) 'adjust-theme-to-light)

;;(adjust-theme-to-light)


(provide 'init-theme)
