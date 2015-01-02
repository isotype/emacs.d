;;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Author: Anton Strilchuk <anton@isoty.pe>                       ;;;
;;; URL: http://isoty.pe                                           ;;;
;;; Created: 10-04-2014                                            ;;;
;;; Last-Updated: 18-12-2014                                       ;;;
;;;   By: Anton Strilchuk <anton@env.sh>                           ;;;
;;;                                                                ;;;
;;; Filename: init-common-lisp                                     ;;;
;;; Version:                                                       ;;;
;;; Description:                                                   ;;;
;;;                                                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; See http://bc.tech.coop/blog/070927.html
(add-auto-mode 'lisp-mode "\\.cl\\'")
(add-hook 'lisp-mode-hook (lambda ()
                            (unless (featurep 'slime)
                              (require-package 'slime)
                              (normal-mode))))

(after-load 'slime
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  (when (executable-find "sbcl")
    (add-to-list 'slime-lisp-implementations
                 '(sbcl ("sbcl") :coding-system utf-8-unix)))
  (when (executable-find "lisp")
    (add-to-list 'slime-lisp-implementations
                 '(cmucl ("lisp") :coding-system iso-latin-1-unix))))

;; From http://bc.tech.coop/blog/070515.html
(defun lispdoc ()
  "Searches lispdoc.com for SYMBOL, which is by default the symbol currently under the curser"
  (interactive)
  (let* ((word-at-point (word-at-point))
         (symbol-at-point (symbol-at-point))
         (default (symbol-name symbol-at-point))
         (inp (read-from-minibuffer
               (if (or word-at-point symbol-at-point)
                   (concat "Symbol (default " default "): ")
                 "Symbol (no default): "))))
    (if (and (string= inp "") (not word-at-point) (not
                                                   symbol-at-point))
        (message "you didn't enter a symbol!")
      (let ((search-type (read-from-minibuffer
                          "full-text (f) or basic (b) search (default b)? ")))
        (browse-url (concat "http://lispdoc.com?q="
                            (if (string= inp "")
                                default
                              inp)
                            "&search="
                            (if (string-equal search-type "f")
                                "full+text+search"
                              "basic+search")))))))

(define-key lisp-mode-map (kbd "C-c l") 'lispdoc)


(provide 'init-common-lisp)
