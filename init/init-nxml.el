;;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Author: Anton Strilchuk <anton@isoty.pe>                       ;;;
;;; URL: http://isoty.pe                                           ;;;
;;; Created: 10-04-2014                                            ;;;
;; Last-Updated: 05-09-2014                                         ;;
;;   By: Anton Strilchuk <anton@ilyfa.cc>                           ;;
;;;                                                                ;;;
;;; Filename: init-nxml                                            ;;;
;;; Description: nxml, by @purcell                                 ;;;
;;;                                                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-auto-mode
 'nxml-mode
 (concat "\\."
         (regexp-opt
          '("xml" "xsd" "sch" "rng" "xslt" "svg" "rss"
            "gpx" "tcx"))
         "\\'"))
(setq magic-mode-alist (cons '("<\\?xml " . nxml-mode) magic-mode-alist))
(fset 'xml-mode 'nxml-mode)
(add-hook 'nxml-mode-hook (lambda () (set (make-local-variable 'ido-use-filename-at-point) nil)))
(setq nxml-slash-auto-complete-flag t)

;; See: http://sinewalker.wordpress.com/2008/06/26/pretty-printing-xml-with-emacs-nxml-mode/
(defun pp-xml-region (begin end)
  "Pretty format XML markup in region. The function inserts
linebreaks to separate tags that have nothing but whitespace
between them.  It then indents the markup by using nxml's
indentation rules."
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t)
        (backward-char) (insert "\n"))
      (indent-region begin end)))

;;----------------------------------------------------------------------------
;; Integration with tidy for html + xml
;;----------------------------------------------------------------------------
(require-package 'tidy)
(add-hook 'nxml-mode-hook (lambda () (tidy-build-menu nxml-mode-map)))
(add-hook 'html-mode-hook (lambda () (tidy-build-menu html-mode-map)))

(add-auto-mode 'html-mode "\\.(jsp|tmpl)\\'")


;;,---------
;;| Fold XML
;;`---------
; hide-show for nxml mode
(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]>\\|<[^/][^>]*[^/]>"
               ""
               "<!--" ;; won't work on its own; uses syntax table
               (lambda (arg) (my-nxml-forward-element))
               nil))

(defun my-nxml-forward-element ()
  (let ((nxml-sexp-element-flag))
    (setq nxml-sexp-element-flag (not (looking-at "<!--")))
    (unless (looking-at outline-regexp)
      (condition-case nil
          (nxml-forward-balanced-item 1)
        (error nil)))))


(provide 'init-nxml)
