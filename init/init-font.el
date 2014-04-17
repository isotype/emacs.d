;;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Author: Anton Strilchuk <anton@isoty.pe>                       ;;;
;;; URL: http://isoty.pe                                           ;;;
;;; Created: 08-04-2014                                            ;;;
;;; Last-Updated: 17-04-2014                                       ;;;
;;;   By: Anton Strilchuk <anton@isoty.pe>                         ;;;
;;;                                                                ;;;
;;; Filename: init-font                                            ;;;
;;; Version:                                                       ;;;
;;; Description:                                                   ;;;
;;;                                                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-frame-font "Fira Mono-18")

;;; Character sets
(defcustom ype/force-default-font-for-symbols nil
  "When non-nil, force Emacs to use your default font for symbols."
  :type 'boolean)

(defun ype/maybe-use-default-font-for-symbols ()
  "Force Emacs to render symbols using the default font, if so configured."
  (when ype/force-default-font-for-symbols
    (set-fontset-font "fontset-default" 'symbol (face-attribute 'default :family))))

(add-hook 'after-init-hook 'ype/maybe-use-default-font-for-symbols)

;;; Changing font sizes
(require 'cl)
(defun ype/font-name-replace-size (font-name new-size)
  (let ((parts (split-string font-name "-")))
    (setcar (nthcdr 7 parts) (format "%d" new-size))
    (mapconcat 'identity parts "-")))

(defun ype/increment-default-font-height (delta)
  "Adjust the default font height by DELTA on every frame.
Emacs will keep the pixel size of the frame approximately the
same.  DELTA should be a multiple of 10, to match the units used
by the :height face attribute."
  (let* ((new-height (+ (face-attribute 'default :height) delta))
         (new-point-height (/ new-height 10)))
    (dolist (f (frame-list))
      (with-selected-frame f
        ;; Latest 'set-frame-font supports a "frames" arg, but
        ;; we cater to Emacs 23 by looping instead.
        (set-frame-font (ype/font-name-replace-size
                         (face-font 'default)
                         new-point-height)
                        t)))
    (set-face-attribute 'default nil :height new-height)
    (message "default font size is now %d" new-point-height)))

(defun ype/increase-default-font-height ()
  (interactive)
  (ype/increment-default-font-height 10))

(defun ype/decrease-default-font-height ()
  (interactive)
  (ype/increment-default-font-height -10))

(global-set-key (kbd "H-\=") 'ype/increase-default-font-height)
(global-set-key (kbd "H-\-") 'ype/decrease-default-font-height)

(provide 'init-font)
