;;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Author: y_pe <anton@isoty.pe>                                  ;;;
;;; URL: http://isoty.pe                                           ;;;
;;; Created: 23-03-2014                                            ;;;
;;; Last-Updated: 23-03-2014                                       ;;;
;;;   By: y_pe <anton@isoty.pe>                                    ;;;
;;;                                                                ;;;
;;; Filename: header_setup                                         ;;;
;;; Version: 0.0.1                                                 ;;;
;;; Description:                                                   ;;;
;;;                                                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'header2)

(setq
 custom-user-name		"y_pe"
 header-date-format           	"%d-%m-%Y"
 header-max 			5000
 header-copyright-notice	nil
 header-url-str			"http://isoty.pe"
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
		    ))
;;; From: https://github.com/ahilsend/dotfiles/blob/master/.emacs.d/rc/rc-header2.el
(defun custom/header-prefix-string ()
  (setq header-prefix-string
        (pcase  major-mode
          (`c-mode           "///")
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
  (concat custom-user-name " <" user-mail-address ">"))

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
(add-hook 'lisp-mode-hook 'auto-make-header)
(add-hook 'c-mode-common-hook   'auto-make-header)
