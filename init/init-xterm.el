;;; init-xterm.el --- 
;; 
;; Filename: init-xterm.el
;; Description: modification of Steve Purcell's emacs.d/init-xterm.el
;; Created by: Steve Purcell
;; Author: ype
;; Created: Mon Feb 17 16:51:09 2014 (+0000)
;; Package-Requires: ()
;; Last-Updated: 03-08-2014                                         ;;
;;   By: Anton Strilchuk <ype@env.sh>                               ;;
;;  Update #: 3                                                     ;;
;; URL: isoty.pe
;; Doc URL: https://github.com/purcell/emacs.d/blob/master/lisp/init-xterm.el
;; Keywords: Emacs, xterm, init, lisp
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(require 'init-frame-hooks)

(defun fix-up-xterm-control-arrows ()
  (let ((map (if (boundp 'input-decode-map)
                 input-decode-map
               function-key-map)))
    (define-key map "\e[1;5A" [C-up])
    (define-key map "\e[1;5B" [C-down])
    (define-key map "\e[1;5C" [C-right])
    (define-key map "\e[1;5D" [C-left])
    (define-key map "\e[5A"   [C-up])
    (define-key map "\e[5B"   [C-down])
    (define-key map "\e[5C"   [C-right])
    (define-key map "\e[5D"   [C-left])))

(global-set-key [mouse-4] (lambda () (interactive) (scroll-down 1)))
(global-set-key [mouse-5] (lambda () (interactive) (scroll-up 1)))

(add-hook 'after-make-console-frame-hooks
          (lambda ()
            (when (< emacs-major-version 23)
              (fix-up-xterm-control-arrows))
            (xterm-mouse-mode 1) ; Mouse in a terminal (Use shift to paste with middle button)
            (when (fboundp 'mwheel-install)
              (mwheel-install))))

(provide 'init-xterm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-xterm.el ends here
