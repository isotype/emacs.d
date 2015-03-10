';; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author: Anton Strilchuk <ype@env.sh>                             ;;
;; URL: http://ype.env.sh                                           ;;
;; Created: 30-06-2014                                              ;;
;;; Last-Updated: 10-03-2015                                       ;;;
;;;  Update #: 27                                                  ;;;
;;;   By: Anton Strilchuk <anton@env.sh>                           ;;;
;;                                                                  ;;
;; Filename: init-windows                                           ;;
;; Description: From @purcell with minor modifications              ;;
;;                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;,------------------------------------------------------------
;;| Navigate window layouts with "C-c <left>" and "C-c <right>"
;;`------------------------------------------------------------
(winner-mode 1)

;; Make "M-`" prompt for a target window when there are more than 2
(require-package 'switch-window)
(require 'switch-window)
(global-set-key (kbd "M-\`") 'switch-window)

;; Ace Window
;;(require-package 'ace-window)
;;(global-set-key (kbd "M-\`") 'ace-select-window)
;;(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

;;,-------------------------------------------------------------
;;| When splitting window, show (other-buffer) in the new window
;;`-------------------------------------------------------------
(defun split-window-func-with-other-buffer (split-function)
  (lexical-let ((s-f split-function))
    (lambda ()
      (interactive)
      (funcall s-f)
      (set-window-buffer (next-window) (other-buffer)))))

(global-set-key "\C-x2" (split-window-func-with-other-buffer 'split-window-vertically))
(global-set-key "\C-x3" (split-window-func-with-other-buffer 'split-window-horizontally))

(defun winner-mode/toggle-delete-other-windows ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and winner-mode
         (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

(global-set-key "\C-x1" 'winner-mode/toggle-delete-other-windows)

;;,------------------------
;;| Rearrange split windows
;;`------------------------
(defun split-window-horizontally-instead ()
  (interactive)
  (save-excursion
    (delete-other-windows)
    (funcall (split-window-func-with-other-buffer 'split-window-horizontally))))

(defun split-window-vertically-instead ()
  (interactive)
  (save-excursion
    (delete-other-windows)
    (funcall (split-window-func-with-other-buffer 'split-window-vertically))))

(global-set-key "\C-x|" 'split-window-horizontally-instead)
(global-set-key "\C-x_" 'split-window-vertically-instead)

;;,---------------------------------------------------------------
;;| Borrowed from http://postmomentum.ch/blog/201304/blog-on-emacs
;;`---------------------------------------------------------------
(defun sanityinc/split-window()
  "Split the window to see the most recent buffer in the other window.
Call a second time to restore the original window configuration."
  (interactive)
  (if (eq last-command 'sanityinc/split-window)
      (progn
        (jump-to-register :sanityinc/split-window)
        (setq this-command 'sanityinc/unsplit-window))
    (window-configuration-to-register :sanityinc/split-window)
    (switch-to-buffer-other-window nil)))

(global-set-key (kbd "<f7>") 'sanityinc/split-window)
(global-set-key (kbd "<f8>")
                (lambda ()
                  (interactive)
                  (switch-to-buffer nil)))

(require-package 'ace-jump-buffer)
(global-set-key (kbd "M-\'") 'ace-jump-buffer)
;; (print (let ((this-frame-buffer nil)
;;              (other-frame-buffer nil))
;;          (setq this-frame-buffer (car (frame-parameter nil 'buffer-list)))
;;          (other-frame 1)

;;          (mouse-autoselect-window)
;;          (setq other-frame-buffer (car (frame-parameter nil 'buffer-list)))
;;          (other-frame 1)
;;          (switch-to-buffer other-frame-buffer))
;;        )
(setq focus-follows-mouse nil)
(setq mouse-autoselect-window nil)
(defun switch-buffers-between-frames ()
  "switch-buffers-between-frames switches the buffers between the two last frames"
  (interactive)
  (let ((this-frame-buffer nil)
  (other-frame-buffer nil))
    (setq this-frame-buffer (car (frame-parameter nil 'buffer-list)))
    (other-frame 1)
    (setq other-frame-buffer (car (frame-parameter nil 'buffer-list)))
    (switch-to-buffer this-frame-buffer)
    (other-frame 1)
    (switch-to-buffer other-frame-buffer)))

;; Sticky Windows
(defadvice pop-to-buffer (before cancel-other-window first)
  (ad-set-arg 1 nil))

(ad-activate 'pop-to-buffer)

;; Toggle window dedication
(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not"
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))

;; Press [pause] key in each window you want to "freeze"
(global-set-key (kbd "H-x w l") 'toggle-window-dedicated)

;; Golden Ratio Frames
(require-package 'golden-ratio)
(when (require 'golden-ratio)
  (setq golden-ratio-exclude-buffer-names '("*Sauron*"))
  (setq golden-ratio-extra-commands '("ace-select-window" "switch-window"))
  (golden-ratio-mode 1))

(provide 'init-windows)
