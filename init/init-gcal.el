;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author: Anton Strilchuk <anton@env.sh>                           ;;
;; URL: http://ype.env.sh                                           ;;
;; Created: 29-10-2014                                              ;;
;; Last-Updated: 29-10-2014                                         ;;
;;  Update #: 1                                                     ;;
;;   By: Anton Strilchuk <anton@env.sh>                             ;;
;;                                                                  ;;
;; Filename: init-gcal                                              ;;
;; Version:                                                         ;;
;; Description:                                                     ;;
;;                                                                  ;;
;; Package Requires: ()                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ype:gcal-set (type what date start finish &optional with where reminder)
  "Add an event to Google Calendar using google-cl"
  (with-temp-buffer
    (call-process-shell-command
     (concat "echo " type " " what
             (when (stringp with)
               (concat " with " with))
             " on " date " from " start " till " finish
             (when (stringp where)
               (concat " where " where))
             (when (stringp reminder)
               (concat " --reminder=" reminder))
             " | " "google calendar add ") nil 0)))

(defun ype:gcal-add (type what date start finish)
  "Ask a series of Questions about an entry you want to add to google calendar"
  (interactive "sEvent Type? [TODO,EVENT,INVOICE,REPLY]: \nsWhats Happening? \nsWhen is it? \nsWhat time does it start? \nsWhat time does it finish? ")
  (ype:gcal-set type what date start finish))



(provide 'init-gcal)
