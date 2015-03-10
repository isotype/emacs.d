;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author: Anton Strilchuk <anton@ilyfa.cc>                         ;;
;; URL: http://ype.env.sh                                           ;;
;; Created: 23-07-2014                                              ;;
;;; Last-Updated: 09-03-2015                                       ;;;
;;;  Update #: 10                                                  ;;;
;;;   By: Anton Strilchuk <anton@env.sh>                           ;;;
;;                                                                  ;;
;; Filename: init-helpers                                           ;;
;; Description: Simple Helper Functions to make life easier         ;;
;;                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;,----------------
;;| It's About Time
;;`----------------
(defun insert-date-time ()
  "Insert current date-time string in full
ISO 8601 format.
Example: 2010-11-29T23:23:35-08:00
See: URL `http://en.wikipedia.org/wiki/ISO_8601'
From: Xah
"
  (interactive)
  (when (use-region-p)
    (delete-region (region-beginning) (region-end) )
    )
  (insert
   (concat
    (format-time-string "%Y-%m-%d %T")
    ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
     (format-time-string "%z")))))

;; Manual Day and Time
(defun insert-manual-day-and-time (day time)
  "Insert user selected day-and-time as string
Example: 2014-07-24 10:34:00+01:00
Usage: (insert-manual-day-and-time \"day\" \"time\")
"
  (interactive)
  (when (use-region-p)
    (delete-region (region-beginning) (region-end) )
    )
  (insert
   (concat
    (format-time-string "%Y-%m-") day " " time ":00"
    ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
     (format-time-string "%z")))))

;;,---------
;;| Comments
;;`---------

(defun paste-giant-comment-line ()
  (interactive)
  (comment-dwim nil)
  (insert "-----------------------------------------------------------------------------")
  (newline-and-indent))
;; Paste giant comment line
;; -----------------------------------------------------------------------------

(global-set-key (kbd "H-;") 'paste-giant-comment-line)

(defun ype:paste-defun-comment-pretty ()
"
/=========================================================================`
| Function to quickly create a box that looks pretty in popup             |
`=========================================================================/
"
  (interactive)
  (insert "\"")
  (newline-and-indent)
  (insert "/=========================================================================`")
  (newline-and-indent)
  (insert "|                                                                         |")
  (newline-and-indent)
  (insert "`=========================================================================/")
  (newline-and-indent)
  (insert "\""))

(global-set-key (kbd "H-\'") 'ype:paste-defun-comment-pretty)

(provide 'init-helpers)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;init-helpers.el ends here
