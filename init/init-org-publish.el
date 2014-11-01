;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author: Anton Strilchuk <ype@env.sh>                             ;;
;; URL: http://ype.env.sh                                           ;;
;; Created: 01-10-2014                                              ;;
;; Last-Updated: 23-10-2014                                         ;;
;;  Update #: 65                                                    ;;
;;   By: Anton Strilchuk <anton@env.sh>                             ;;
;;                                                                  ;;
;; Filename: init-org-publish                                       ;;
;; Version:                                                         ;;
;; Description: Org Publishing                                      ;;
;;                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-publish-project-alist
      '(
        ("quadriv"
         :components ("quadriv-notes"
                      "quadriv-static"))
        ("quadriv-notes"
         :base-directory "~/Dropbox/ype/quadriv/base/"
         :base-extension "org"
         :publishing-directory "~/Dropbox/ype/quadriv/html/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :export-with-tags nil
         :headline-levels 4
         :auto-sitemap t
         :sitemap-filename "archive.org"
         :sitemap-title "Archive"
         :sitemap-sans-extension nil
         :sitemap-file-entry-format "%d \[%t\]"
         :sitemap-sort-folders last
         :sitemap-date-format "%b %d -- "
         :sitemap-sort-files chronologically
         :section-numbers nil
         :with-toc nil
         :with-author t
         :with-creator nil
         :html-doctype "html5"
         :html-preamble org-mode-blog-preamble
         :html-postamble "
         <script src=\"http://code.jquery.com/jquery-1.10.2.min.js\"></script>
         <script src=\"/assets/js/bootstrap.min.js\"></script>
         <script src=\"/assets/material/js/ripples.min.js\"></script>
         <script src=\"/assets/material/js/material.min.js\"></script>
         <script type=\"text/javascript\">
            /* * * CONFIGURATION VARIABLES: EDIT BEFORE PASTING INTO YOUR WEBPAGE * * */
            var disqus_shortname = 'ypenv'; // required: replace example with your forum shortname

            /* * * DON'T EDIT BELOW THIS LINE * * */
            (function() {
                var dsq = document.createElement('script');
                dsq.type = 'text/javascript';
                dsq.async = true;
                dsq.src = '//' + disqus_shortname + '.disqus.com/embed.js';
            (document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(dsq);})();
        </script>
        <noscript>Please enable JavaScript to view the
            <a href=\"http://disqus.com/?ref_noscript\">comments powered by Disqus.</a>
        </noscript>"
         :html-head "<!-- Hello Internet -->"
         :html-head-extra"<link href=\"/assets/css/bootstrap.min.css\" rel=\"stylesheet\" media=\"screen\">
         <link href=\"/assets/material/css/ripples.min.css\" rel=\"stylesheet\">
         <link href=\"/assets/material/css/material-wfont.css\" rel=\"stylesheet\">
         <link href=\"/assets/css/org.css\" rel=\"stylesheet\" media=\"screen\">
         <link href=\"/assets/css/syntax.css\" rel=\"stylesheet\" media=\"screen\">
         <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" />"
         :html-html5-fancy t
         :html-head-include-default-style nil
         :html-link-use-abs-url t
         :html-extension "html"
         )
        ("quadriv-static"
         :base-directory "~/Dropbox/ype/quadriv/base/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|eot\\|ttf\\|woff\\|svg"
         :publishing-directory "~/Dropbox/ype/quadriv/html/"
         :recursive t
         :publishing-function org-publish-attachment
         )
        ))

(defun org-mode-blog-preamble (options)
  "The function that creates the preamble (sidebar) for the blog.
OPTIONS contains the property list from the org-mode export."
  (let ((base-directory (plist-get options :base-directory)))
    (org-babel-with-temp-filebuffer (expand-file-name "assets/html/.preamble.html" base-directory) (buffer-string))))

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
  (org-publish "quadriv" t))

(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key "\C-p" 'ype:quick-publish)))

(add-hook 'before-save-hook #'endless/update-includes)

(defun endless/update-includes (&rest ignore)
  "Update the line numbers of #+INCLUDE:s in current buffer.
Only looks at INCLUDEs that have either :range-begin or :range-end.
This function does nothing if not in org-mode, so you can safely
add it to `before-save-hook'."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp
              "^\\s-*#\\+INCLUDE: *\"\\([^\"]+\\)\".*:range-\\(begin\\|end\\)"
              nil 'noerror)
        (let* ((file (expand-file-name (match-string-no-properties 1)))
               lines begin end)
          (forward-line 0)
          (when (looking-at "^.*:range-begin *\"\\([^\"]+\\)\"")
            (setq begin (match-string-no-properties 1)))
          (when (looking-at "^.*:range-end *\"\\([^\"]+\\)\"")
            (setq end (match-string-no-properties 1)))
          (setq lines (endless/decide-line-range file begin end))
          (when lines
            (if (looking-at ".*:lines *\"\\([-0-9]+\\)\"")
                (replace-match lines :fixedcase :literal nil 1)
              (goto-char (line-end-position))
              (insert " :lines \"" lines "\""))))))))

(defun endless/decide-line-range (file begin end)
  "Visit FILE and decide which lines to include.
BEGIN and END are regexps which define the line range to use."
  (let (l r)
    (save-match-data
      (with-temp-buffer
        (insert-file file)
        (goto-char (point-min))
        (if (null begin)
            (setq l "")
          (search-forward-regexp begin)
          (setq l (line-number-at-pos (match-beginning 0))))
        (if (null end)
            (setq r "")
          (search-forward-regexp end)
          (setq r (1+ (line-number-at-pos (match-end 0)))))
        (format "%s-%s" l (- r 1))))))

(provide 'init-org-publish)
