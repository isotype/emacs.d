;;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Author: Anton Strilchuk <anton@isoty.pe>                       ;;;
;;; URL: http://isoty.pe                                           ;;;
;;; Created: 03-04-2014                                            ;;;
;; Last-Updated: 01-09-2014                                         ;;
;;   By: Anton Strilchuk <ype@env.sh>                               ;;
;;;                                                                ;;;
;;; Filename: init-blog-post                                       ;;;
;;; Description: Quick 'n Dirty blog posts                         ;;;
;;;                                                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Template insert options

(defun ype/insert-options-template (&optional title subtitle tags image video filetype)
  "Ask a series of simple questions to quickly prep blog post front matter"
  (interactive
   (let* ((i (read-string "title: " (combine-and-quote-strings
                                     (cdr (split-string (file-name-sans-extension (buffer-name)) "-" t)))))
          (u (read-string "subtitle: "))
          (k (read-string "tags(separated by comma and space [, ]): "))
          (j (if (y-or-n-p "Would you like to add an image?")
                 (progn
                   (read-file-name "Image Location: "))
               (progn
                 "false")))
          (h (if (y-or-n-p "Would you like to add a video?")
                 (progn
                   (read-file-name "Video Location: "))
               (progn
                 "false"))))
     (setq image j)
     (setq video h)
     (list i u k j h)))
  (if (not (bolp)) (newline))
  (insert
   (format
    "---
title: \"%s\"
subtitle: \"%s\"
author: %s
created: %s
status: %s
filetype: %s
isonum: %s
layout: %s
kind: %s
comments: %s
tags: [%s]
keywords:
image: %s
video: %s
---"
    (if (string= title "") (buffer-name) title)
    (if (string= subtitle "") (buffer-name) subtitle)
    (user-full-name)
    (format-time-string "%Y-%m-%d")
    (format "Incomplete")               ; status
    (if (string= filetype "") "<TODO: insert a filetype>" filetype)
    (format "%d" (random* 606))         ; isonum
    (format "post")                     ; layout
    (format "article")                  ; kind
    (when
        (y-or-n-p "Comments? ")
      (format "true"))
    (if (string= tags "") "<TODO: insert tags>" tags)
    (if (string= image "false") "false"
      (progn
        (dired-create-directory (concat ype/blog-media-directory (file-name-sans-extension (buffer-name))))
        (dired-copy-file image (concat ype/blog-media-directory
                                       (file-name-sans-extension (buffer-name))
                                       "/" (combine-and-quote-strings (last (split-string image)))) t)
        (concat "/media/" (file-name-sans-extension (buffer-name)) "/" (combine-and-quote-strings (last (split-string image))))))
    (if (string= video "false") "false"
      (progn
        (dired-create-directory (concat ype/blog-media-directory (file-name-sans-extension (buffer-name))))
        (dired-copy-file video (concat ype/blog-media-directory
                                       (file-name-sans-extension (buffer-name))
                                       "/" (combine-and-quote-strings (last (split-string image)))) t)
        (concat "/media/" (file-name-sans-extension (buffer-name)) "/" (combine-and-quote-strings (last (split-string image)))))))))

(defun ype/new-post (&optional filename filetype)
  "Prompt for creating new blog post file"
  (interactive
   (let* ((f (read-string "filename: " ""))
          (p (read-string "filetype (default: md): " "")))
     (list f p)))
  (when (string= filename "")
    (setq filename "new-post"))
  (when (string= filetype "")
    (setq filetype "md"))
  (setq filename (concat (format-time-string "%Y-%m-%d-") filename "." filetype))
  (let* ((dir (concat (file-name-as-directory ype/blog-posts-directory)))
         (path (concat dir filename)))
    (when (file-exists-p path)
      (error "Post `%s' already exists." path))
    (unless (file-directory-p dir)
      (mkdir dir t))
    (switch-to-buffer (find-file path))
    (call-interactively 'ype/insert-options-template)
    (save-buffer)))


(provide 'init-blog-post)
