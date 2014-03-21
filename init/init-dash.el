;;; init-dash.el --- 
;; 
;; Filename: init-dash.el
;; Description: Helm Dash Configuration
;; Author: y_pe
;; Maintainer: y_pe
;; Created: Fri Mar 21 17:45:26 2014 (+0000)
;; Package-Requires: (Dash.app, helm-dash)
;; Last-Updated: Fri Mar 21 18:08:20 2014 (+0000)
;;           By: y_pe
;;     Update #: 6
;; URL: github/isotype/emacs.d
;; Doc URL: http://isoty.pe
;; Keywords: Dashapp, helm, docsets
;; Compatibility: Emacs 24++, OSX
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;;  - Initialization of modes to use with helm-dash docsets
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;;  - Added elisp-doc, and clisp-doc
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:
(require 'helm-dash)

(defun elisp-doc()
  (interactive)
  (setq-local helm-dash-docset '("Emacs Lisp")))
(add-hook 'emacs-lisp-mode 'elisp-doc)

(defun clisp-doc ()
  (interactive)
  (setq-local helm-dash-docset '("Common Lisp")))
(add-hook 'common-lisp-mode 'clisp-doc)

(provide 'init-dash)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dash.el ends here
