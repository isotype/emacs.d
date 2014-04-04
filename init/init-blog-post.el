;;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Author: Anton Strilchuk <anton@isoty.pe>                       ;;;
;;; URL: http://isoty.pe                                           ;;;
;;; Created: 03-04-2014                                            ;;;
;;; Last-Updated: 04-04-2014                                       ;;;
;;;   By: Anton Strilchuk <anton@isoty.pe>                         ;;;
;;;                                                                ;;;
;;; Filename: init-blog-post                                       ;;;
;;; Description: Tool to generate new org blog post from template  ;;;
;;;                                                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Template insert options
;; Based on org-page function
;; https://github.com/kelvinh/org-page/blob/master/org-page.el
(defun ype/insert-options-template (&optional title subtitle tags)
  (interactive
   (let* ((i (read-string "title: "))
          (u (read-string "subtitle: "))
          (k (read-string "tags(separated by comma and space [, ]): ")))
     (list i u k)))
  (if (not (bolp)) (newline))
  (insert (format
           "---
title:         %s
subtitle:      %s
author:        %s
created_at:    %s
status:        %s
format:        %s
isonum:        %s
layout:        %s
kind:          %s
tags:          %s
keywords:
---"
    (if (string= title "") (buffer-name) title)
    (if (string= subtitle "") (buffer-name) subtitle)
    (user-full-name)
    (format-time-string (concat (format-time-string "%Y-%m-%d %T")
                                   ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
                                    (format-time-string "%z"))))
    (format "Completed") ; status
    (format "org") ; format
    (format "%d" (random* 606)) ; isonum
    (format "post") ; layout
    (format "article") ; kind
    (if (string= tags "") "<TODO: insert your tags here>" tags))))

(defun ype/new-post (&optional filename)
  (interactive
   (let* ((f (read-string "filename: " "new-post")))
     (list f)))
  (if (string= filename "")
      (setq filename "new-post"))
  (setq filename (concat (format-time-string "%Y-%m-%d-") filename ".org"))
  (let* ((dir (concat (file-name-as-directory ype/posts-directory)))
         (path (concat dir filename)))
    (if (file-exists-p path)
        (error "Post `%s' already exists." path))
    (unless (file-directory-p dir)
      (mkdir dir t))
    (switch-to-buffer (find-file path))
    (call-interactively 'ype/insert-options-template)
    (save-buffer)))

(provide 'init-blog-post)
