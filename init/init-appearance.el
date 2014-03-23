;;; init-appearance.el --- 
;;
;; Filename: init-appearance.el
;; Description: Emacs Look-n-feel yo! 
;; Author: y_pe
;; Maintainer: y_pe
;; Created: Mon Mar  3 09:48:41 2014 (+0000)
;; Version: 0.1
;; Package-Requires: ()
;; Last-Updated: Sat Mar 22 16:53:22 2014 (+0000)
;;           By: y_pe
;;     Update #: 14
;; URL: http://isoty.pe
;; Doc URL: https://github.com/isotype/emacs.d
;; Keywords: Emacs, init, appearence
;; Compatibility: Emacs 24++
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary
;;  This is where all the pretty glitz and glam of emacs happens
;;  with customizations to make everything look a little nicer.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log
;;  - Added pretty mode and symbols
;;  - Added OSX mouse scroll stuff
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; License
;;  This program is free software; you can redistribute it and/or
;;  modify it under the terms of the GNU General Public License as
;;  published by the Free Software Foundation; either version 3, or
;;  (at your option) any later version.
;; 
;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.
;;  
;;  You should have received a copy of the GNU General Public License
;;  along with this program; see the file COPYING.  If not, write to
;;  the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;;  Floor, Boston, MA 02110-1301, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code

;;; Color-theme
(defun light ()
  (interactive)
  (load-theme 'solarized-light t))

(defun dark ()
  (interactive)
  (load-theme 'solarized-dark t))

;;Default theme
(dark)

;;;Powerline
(require 'powerline)
(powerline-default-theme)
(setq powerline-color1 "#073642")
(setq powerline-color2 "#002B36")

(set-face-attribute 'mode-line nil
                    :foreground "#fdf6e3"
                    :background "#2aa198"
                    :box nil
		    :inverse-video nil)
(set-face-attribute 'mode-line-inactive nil
		    :foreground "#586e75"
                    :background "#002b36"
                    :box nil)

;;; Wrap Text
(global-visual-line-mode 1)

;;; Highlight Cursor Line
(global-hl-line-mode 1)

;;; Column Numbers
(column-number-mode 1)

;;; Remove Toolbar
(tool-bar-mode -1)

;;; Remove scrollbar
(scroll-bar-mode -1)

(require 'pretty-mode)
(global-pretty-mode t)

;;Make things look pretty
(require 'pretty-symbols)

;;Less Flickery Display
(setq redisplay-dont-pause t)

;;Don't display battery life
(display-battery-mode -1)
(size-indication-mode -1)

;; Nicer scrolling with mouse wheel/trackpad.
(unless (and (boundp 'mac-mouse-wheel-smooth-scroll) mac-mouse-wheel-smooth-scroll)
  (global-set-key [wheel-down] (lambda () (interactive) (scroll-up-command 1)))
  (global-set-key [wheel-up] (lambda () (interactive) (scroll-down-command 1)))
  (global-set-key [double-wheel-down] (lambda () (interactive) (scroll-up-command 2)))
  (global-set-key [double-wheel-up] (lambda () (interactive) (scroll-down-command 2)))
  (global-set-key [triple-wheel-down] (lambda () (interactive) (scroll-up-command 4)))
  (global-set-key [triple-wheel-up] (lambda () (interactive) (scroll-down-command 4))))

(setq-default scroll-up-aggressively 0.01 scroll-down-aggressively 0.01)

(defun disable-all-pretty-highlighting ()
  "Quick function to turn off pretty-mode when it gets annoying"
  (pretty-mode -1)
  (pretty-symbols-mode -1))

;;Page break line mode
(require 'page-break-lines)
(global-page-break-lines-mode)
"Examples Line below ^L (C-q C-l)"


;;Highlight
;;FIXME
;;TODO
(require 'fic-ext-mode)
(defun fic-ext-mode-modes ()
  (interactive)
  (add-hook 'emacs-lisp-mode-hook 'fic-ext-mode)
  (add-hook 'lisp-mode-hook 'fic-ext-mode)
  (add-hook 'web-mode-hook 'fic-ext-mode)
  (add-hook 'c-mode-common-hook 'fic-ext-mode)
  (add-hook 'python-mode-hook 'fic-ext-mode))

(fic-ext-mode-modes)

;;(require 'wiki-nav)
;;(global-wiki-nav-mode)

(provide 'init-appearance)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-appearance.el ends here
