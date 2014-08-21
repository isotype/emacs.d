;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author: Anton Strilchuk <ype@env.sh>                             ;;
;; URL: http://ype.env.sh                                           ;;
;; Created: 16-06-2014                                              ;;
;; Last-Updated: 15-08-2014                                         ;;
;;  Update #: 2                                                     ;;
;;   By: Anton Strilchuk <ype@env.sh>                               ;;
;;                                                                  ;;
;; Filename: init-headers                                           ;;
;; Version:                                                         ;;
;; Description: Do not byte-compile this file                       ;;
;;                                                                  ;;
;; Package Requires: ()                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require-package 'header2)
(require 'fold-dwim)
(require 'header2)

(setq ype/fill-length 70)

(defun ype/length (&rest rest)
  "Summed length of all the strings/lists in rest."
  (apply '+ (mapcar 'length rest)))

(defun ype/fill-str (delim &optional total &rest str)
  (let* ((total-len (or total ype/fill-length))
         (str-len (or (apply 'ype/length str) 0))
         (rem-length (- total-len str-len)))
    (if (> rem-length 0)
        (make-string rem-length delim)
      "")))

(defun ype/aligned-str (pre &rest str &optional total)
  (let* ((l (car (last str)))
         (total (unless (stringp l) l))
         (str
          (replace-regexp-in-string "\\` ?\\| ?$" " "
                                    (apply 'concat (remove-if-not 'stringp str)))))
    (concat pre str (ype/fill-str ?\s total pre str pre) pre)))

(defun ype/file-name ()
  (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))

(setq header-date-format "%d-%m-%Y"
      header-max 5000
      header-copyright-notice nil
      header-url-str "http://ype.env.sh"
      make-header-hook '(
                         ype/header-prefix-string
                         ype/header-mode-line
                         ype/header-end-line
                         ype/header-author
                         ype/header-url
                         header-copyright
                         ype/header-creation-date
                         ype/header-modification-date
                         ype/header-update-count
                         ype/header-modification-author
                         ype/header-blank
                         ype/header-file-name
                         ype/header-version
                         ype/header-description
                         header-lib-requires
                         ype/header-pkg-requires
                         ype/header-end-line
                         ;;TODO header-license
                         ype/header-end-line
                         ))
;;; From: https://github.com/ahilsend/dotfiles/blob/master/.emacs.d/rc/rc-header2.el
(defun ype/header-prefix-string ()
  (setq header-prefix-string
        (pcase  major-mode
          (`c-mode           "///")
          (`arduino-mode     "///")
          (`c++-mode         "///")
          (`conf-mode        "##")
          (`conf-colon-mode  "##")
          (`conf-space-mode  "##")
          (`conf-unix-mode   "##")
          (`emacs-lisp-mode  ";;")
          (`erlang-mode      "%%")
          (`gitconfig-mode   "###")
          (`gitignore-mode   "##")
          (`haskell-mode     "---")
          (`python-mode      "#")
          (`js2-mode         "//")
          (`lua-mode         "---")
          (`sh-mode          "##")
          (_                 header-prefix-string))))

(defun ype/insert-aligned (&rest str)
  (insert (apply 'ype/aligned-str header-prefix-string str) "\n"))

(defun ype/header-mode-line ()
  (ype/insert-aligned "-*- mode: " (true-mode-name)
                      "; tab-width: 2; indent-tabs-mode:nil; -*-"))

(defun ype/user-str ()
  (concat user-full-name " <" user-mail-address ">"))

(defun ype/header-author ()
  (ype/insert-aligned "Author: " (ype/user-str)))

(defun ype/header-creation-date ()
  (ype/insert-aligned "Created: " (header-date-string)))

(defun ype/header-modification-date ()
  (ype/insert-aligned "Last-Updated:"))

(defsubst ype/header-modification-author ()
  (ype/insert-aligned "  By:" " "))
(defsubst ype/header-update-count ()
  (ype/insert-aligned "  Update #:" " 0"))

(defun ype/header-file-name ()
  (ype/insert-aligned "Filename: " (ype/file-name)))

(defun ype/find-req-pkg ()
  (with-temp-buffer
    (goto-char (point-min))
    (while
        (re-search-forward "\\(\\(\\Srequire.+'\\)\\(\\b.+\\b\\)\\)" nil t)
      (match-string-no-properties 0)
      )))

(defun ype/header-pkg-requires ()
  (ype/insert-aligned "Package Requires: (" (ype/find-req-pkg) ")"))

(defun ype/header-description ()
  (ype/insert-aligned "Description:")
  (setq return-to (+ 2 (point)))
  (ype/insert-aligned))

(defun ype/url-str ()
  (concat "URL: " header-url-str))
(defun ype/header-url ()
  (ype/insert-aligned (ype/url-str)))

(defun ype/header-version ()
  (ype/insert-aligned "Version: "))

(defun ype/header-blank ()
  (ype/insert-aligned))

(defun ype/header-end-line ()
  (insert (ype/fill-str (nth 0 (string-to-list header-prefix-string))) "\n"))

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

(defun ype/header-license ()
  "Insert license"
  (ype/insert-aligned (split-string header-license)))

;;; Update header
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun update-last-modified-date ()
  (fold-dwim-show)
  (move-beginning-of-line nil)
  (delete-and-forget-line)
  (ype/header-prefix-string)
  (insert (ype/aligned-str header-prefix-string " Last-Updated:" " "
                           (header-date-string))))

(defun update-last-modifier ()
  (move-beginning-of-line nil)
  (delete-and-forget-line)
  (ype/header-prefix-string)
  (insert (ype/aligned-str header-prefix-string "   By:" " " (ype/user-str))))

(defun update-write-count ()
  (let* ((str  (delete-and-forget-line))
         (rem  (read-from-string str))
         (num  (car rem)))
    (when (numberp num)
      (move-beginning-of-line nil)
      (delete-and-forget-line)
      (ype/header-prefix-string)
      (insert (ype/aligned-str header-prefix-string
                               "  Update #:" " " (format "%s" (1+ num)))))))

(register-file-header-action "Last-Updated[ \t]*: " 'update-last-modified-date)
(register-file-header-action "  By[ \t]*: " 'update-last-modifier)
(register-file-header-action "  Update #[ \t]*: " 'update-write-count)
(defun kill-header-auto-write ()
  (interactive)
  (remove-hook 'write-file-hooks 'auto-update-file-header))
(defun header-auto-write ()
  (interactive)
  (add-hook 'write-file-hooks 'auto-update-file-header))
(add-hook 'write-file-hooks 'auto-update-file-header)
(add-hook 'emacs-lisp-mode-hook 'auto-make-header)
(add-hook 'c-mode-common-hook   'auto-make-header)

(provide 'init-headers)
