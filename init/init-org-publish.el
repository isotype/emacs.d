;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author: Anton Strilchuk <ype@env.sh>                             ;;
;; URL: http://ype.env.sh                                           ;;
;; Created: 01-10-2014                                              ;;
;; Last-Updated: 02-10-2014                                         ;;
;;  Update #: 21                                                    ;;
;;   By: Anton Strilchuk <anton@ilyfa.cc>                           ;;
;;                                                                  ;;
;; Filename: init-org-publish                                       ;;
;; Version:                                                         ;;
;; Description: Org Publishing                                      ;;
;;                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-publish-project-alist
      '(
        ("org"
         :components ("org-notes"
                      "org-static"))
        ("org-notes"
         :base-directory "~/Dropbox/ype/cournal/base/"
         :base-extension "org"
         :publishing-directory "~/Dropbox/ype/cournal/html/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :export-with-tags nil
         :headline-levels 4
         :auto-sitemap t
         :sitemap-filename "sitemap.org"
         :sitemap-title "Sitemap"
         :section-numbers nil
         :with-toc nil
         :with-author nil
         :with-creator nil
         :html-doctype "html5"
         :html-preamble org-mode-blog-preamble
         :html-postamble "<hr><div id='comments'></div>"
         :html-head "<script>document.write('<script src=\"http://' + (location.host || 'localhost').split(':')[0] + ':35729/livereload.js?snipver=1\"></' + 'script>')</script>\n"
         :html-head-extra"<link href=\"css/bootstrap.min.css\" rel=\"stylesheet\" media=\"screen\">
         <link href=\"css/bootstrap-theme.min.css\" rel=\"stylesheet\" media=\"screen\">
         <link href=\"css/org.css\" rel=\"stylesheet\" media=\"screen\">
         <script src=\"js/bootstrap.min.js\"></script>
         <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" />"
         :html-html5-fancy t
         :html-head-include-default-style nil
         )
        ("org-static"
         :base-directory "~/Dropbox/ype/cournal/base/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory "~/Dropbox/ype/cournal/html/"
         :recursive t
         :publishing-function org-publish-attachment
         )
        ))

(defun org-mode-blog-preamble (options)
  "The function that creates the preamble (sidebar) for the blog.
OPTIONS contains the property list from the org-mode export."
  (let ((base-directory (plist-get options :base-directory)))
    (org-babel-with-temp-filebuffer (expand-file-name "html/preamble.html" base-directory) (buffer-string))))

(defun org-mode-blog-prepare ()
  "`index.org' should always be exported so touch the file before publishing."
  (let* ((base-directory (plist-get project-plist :base-directory))
         (buffer (find-file-noselect (expand-file-name "index.org" base-directory) t)))
    (with-current-buffer buffer
      (set-buffer-modified-p t)
      (save-buffer 0))
    (kill-buffer buffer)))

(defun ype:quick-publish ()
  (interactive)
  (org-publish "org" t))

(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key "\C-p" 'ype:quick-publish)))

(provide 'init-org-publish)
