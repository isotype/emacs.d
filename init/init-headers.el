;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Filename: init-headers                                         ;;;
;;; Created: 16-11-2014                                            ;;;
;;; Author: Anton Strilchuk <anton@env.sh>                         ;;;
;;; URL: http://ype.env.sh                                         ;;;
;;; Version:                                                       ;;;
;;; Last-Updated: 16-11-2014                                       ;;;
;;;  Update #: 5                                                   ;;;
;;;   By: Anton Strilchuk <anton@env.sh>                           ;;;
;;;                                                                ;;;
;;; Description:                                                   ;;;
;;;                                                                ;;;
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
                         ype/header-end-line
                         ype/header-file-name
                         ype/header-creation-date
                         ype/header-author
                         ype/header-url
                         ype/header-version
                         ype/header-modification-date
                         ype/header-update-count
                         ype/header-modification-author
                         ype/header-blank
                         ype/header-description
                         ype/header-end-line
                         ;;TODO header-license
                         header-eof
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
          (`emacs-lisp-mode  ";;;")
          (`erlang-mode      "%%")
          (`gitconfig-mode   "###")
          (`gitignore-mode   "##")
          (`haskell-mode     "---")
          (`python-mode      "###")
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
  (ype/insert-aligned " Description:")
  (setq return-to (+ 2 (point)))
  (ype/insert-aligned))

(defun ype/vc-root-dir ()
  (ype/insert-aligned " package: --- git root: " (vc-root-dir)))

(defun ype/url-str ()
  (concat "URL: " header-url-str))
(defun ype/header-url ()
  (ype/insert-aligned (ype/url-str)))

(defun ype/header-version ()
  (ype/insert-aligned "Version: "))

(defun ype/header-license ()
  (ype/insert-aligned "License: See included LICENSE file for details")
  (with-temp-buffer
    (async-shell-command (concat "/usr/bin/licgen mit \'" user-full-name "\' \'<" user-mail-address ">\'") nil nil)))

(defun ype/header-code ()
  (ype/insert-aligned " Code:\n"))

(defun ype/header-blank ()
  (ype/insert-aligned))

(defun ype/header-end-line ()
  (insert (ype/fill-str (nth 0 (string-to-list header-prefix-string))) "\n"))

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
