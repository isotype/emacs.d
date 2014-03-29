;;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Author: Anton Strilchuk <anton@isoty.pe>                       ;;;
;;; URL: http://isoty.pe                                           ;;;
;;; Created: 23-03-2014                                            ;;;
;;; Last-Updated: 28-03-2014                                       ;;;
;;;   By: Anton Strilchuk <anton@isoty.pe>                         ;;;
;;;                                                                ;;;
;;; Filename: header_setup                                         ;;;
;;; Description: Auto Make File Header                             ;;;
;;;                                                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'header2)

(setq
 header-date-format	"%d-%m-%Y"
 header-max 5000
 header-copyright-notice nil
 header-url-str "http://isoty.pe"
 make-header-hook '(
                    custom/header-prefix-string
                    header-mode-line
                    header-end-line
                    header-author
                    header-url
                    header-copyright
                    header-creation-date
                    header-modification-date
                    header-modification-author
                    header-blank
                    header-file-name
                    header-version
                    header-description
                    header-lib-requires
                    header-end-line
                    ;;TODO header-license
                    header-end-line
                    ))
;;; From: https://github.com/ahilsend/dotfiles/blob/master/.emacs.d/rc/rc-header2.el
(defun custom/header-prefix-string ()
  (setq header-prefix-string
        (pcase  major-mode
          (`c-mode           "///")
          (`arduino-mode		 "///")
          (`c++-mode         "///")
          (`conf-mode        "##")
          (`conf-colon-mode  "##")
          (`conf-space-mode  "##")
          (`conf-unix-mode   "##")
          (`emacs-lisp-mode  ";;;")
          (`erlang-mode      "%%")
          (`gitconfig-mode   "###")
          (`gitignore-mode   "##")
          (`haskell-mode     "---")
          (`python-mode      "#")
          (`js2-mode         "//")
          (`lua-mode         "---")
          (`sh-mode          "##")
          (_                 header-prefix-string))))

(defun custom/insert-aligned (&rest str)
  (insert (apply 'custom/aligned-str header-prefix-string str) "\n"))

(defun header-mode-line ()
  (custom/insert-aligned "-*- mode: " (true-mode-name)
                         "; tab-width: 2; indent-tabs-mode:nil; -*-"))

(defun custom/user-str ()
  (concat user-full-name " <" user-mail-address ">"))

(defun header-author ()
  (custom/insert-aligned "Author: " (custom/user-str)))

(defun header-creation-date ()
  (custom/insert-aligned "Created: " (header-date-string)))

(defun header-modification-date ()
  (custom/insert-aligned "Last-Updated:"))

(defsubst header-modification-author ()
  (custom/insert-aligned "  By:" " "))
(defsubst header-update-count ()
  (custom/insert-aligned "  Update #:" " 0"))

(defun header-file-name ()
  (custom/insert-aligned "Filename: " (custom/file-name)))

(defun header-description ()
  (custom/insert-aligned "Description:")
  (setq return-to (+ 2 (point)))
  (custom/insert-aligned))

(defun y_pe/url-str ()
  (concat "URL: " header-url-str))
(defun header-url ()
  (custom/insert-aligned (y_pe/url-str)))

(defun header-version ()
  (custom/insert-aligned "Version: "))

(defun header-blank ()
  (custom/insert-aligned))

(defun header-end-line ()
  (insert (custom/fill-str (nth 0 (string-to-list header-prefix-string))) "\n"))

;;;FIXME custom header license
(setq header-license
      "Distributed under an [MIT-style][license] license.

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files \(the \"Software\"), to deal with
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

- Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimers.
- Redistributions in binary form must reproduce the above copyright notice, this
  list of conditions and the following disclaimers in the documentation and/or
  other materials provided with the distribution.
- Neither the names of the copyright holders, nor the names of the authors, nor
  the names of other contributors may be used to endorse or promote products
  derived from this Software without specific prior written permission.

THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE CONTRIBUTORS
OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE SOFTWARE.

\[license]: http://www.opensource.org/licenses/ncsa")

(defun header-license ()
  "Insert license"
   (custom/insert-aligned (split-string header-license)))

;;; Update header
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun update-last-modified-date ()
  (fold-dwim-show)
  (move-beginning-of-line nil)
  (delete-and-forget-line)
  (custom/header-prefix-string)
  (insert (custom/aligned-str header-prefix-string " Last-Updated:" " "
                              (header-date-string))))

(defun update-last-modifier ()
  (move-beginning-of-line nil)
  (delete-and-forget-line)
  (custom/header-prefix-string)
  (insert (custom/aligned-str header-prefix-string "   By:" " " (custom/user-str))))

(defun update-write-count ()
  (let* ((str  (delete-and-forget-line))
         (rem  (read-from-string str))
         (num  (car rem)))
    (when (numberp num)
      (move-beginning-of-line nil)
      (delete-and-forget-line)
      (custom/header-prefix-string)
      (insert (custom/aligned-str header-prefix-string
                                  "  Update #:" " " (format "%s" (1+ num)))))))

(register-file-header-action "Last-Updated[ \t]*: " 'update-last-modified-date)
(register-file-header-action "  By[ \t]*: " 'update-last-modifier)
(register-file-header-action "  Update #[ \t]*: " 'update-write-count)

;;; Add hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'write-file-hooks 'auto-update-file-header)

;;; Modes
(add-hook 'emacs-lisp-mode-hook 'auto-make-header)
(add-hook 'c-mode-common-hook   'auto-make-header)
