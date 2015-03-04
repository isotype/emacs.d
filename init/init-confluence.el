;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Filename: init-confluence                                      ;;;
;;; Created: 06-02-2015                                            ;;;
;;; Author: Anton Strilchuk <anton@env.sh>                         ;;;
;;; URL: http://ype.env.sh                                         ;;;
;;; Version:                                                       ;;;
;;; Last-Updated: 06-02-2015                                       ;;;
;;;  Update #: 3                                                   ;;;
;;;   By: Anton Strilchuk <anton@env.sh>                           ;;;
;;;                                                                ;;;
;;; Description:                                                   ;;;
;;;                                                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Provide:
(provide 'init-confluence)
;; assuming confluence.el and xml-rpc.el are in your load path
(require-package 'confluence)
(require 'confluence)

;; confluence customization
(setq confluence-url "https://i.adaptavist.com/rpc/xmlrpc")
(setq confluence-default-space-alist (list (cons confluence-url "~astrilchuk")))
(setq confluence-xml-convert-to-wiki-on-load t)

(after-load "confluence"
  (progn
    (add-hook 'confluence-mode-hook '(lambda () (local-set-key "\C-j" 'confluence-newline-and-indent)))))

;; setup confluence mode
(add-hook 'confluence-mode-hook
          '(lambda ()
             (local-set-key "\C-xw" confluence-prefix-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;init-confluence.el ends here
