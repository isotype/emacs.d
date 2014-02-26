;; -------------------------------------------------------------------------------------- ;;
;; ; init-javascript.el ---                                                               ;;
;;                                                                                        ;;
;;  Filename: init-javascript.el                                                          ;;
;;  Description:                                                                          ;;
;;  Author: Steve Purcell                                                                 ;;
;;  Maintainer: y_pe                                                                      ;;
;;  Created: Wed Feb 26 13:59:38 2014 (+0000)                                             ;;
;;  Version: 0.1                                                                          ;;
;;  Package-Requ(ires: (json-mode, js2-mode, ac-js2, coffee-mode, js-comint, skewer-mode) ;;
;;  Last-Updated: Wed Feb 26 14:04:09 2014 (+0000)
;;            By: anton
;;      Update #: 1                                                                       ;;
;;  URL: isoty.pe                                                                         ;;
;;  Original URL: https://github.com/purcell/emacs.d/blob/master/lisp/init-javascript.el  ;;
;;  Keywords: Emacs, Init                                                                 ;;
;;  Tested on: Emacs 24.3.50.1                                                            ;;
;;                                                                                        ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                   ;;
;;                                                                                        ;;
;; ; Commentary:                                                                          ;;
;;   This is a slightly modified version of @purcell init-javascript                      ;;
;;   emacs setup.                                                                         ;;
;;                                                                                        ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                   ;;
;;                                                                                        ;;
;; ; Change Log:                                                                          ;;
;;   - Made compatible with Cask package management model                                 ;;
;;                                                                                        ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                   ;;
;;                                                                                        ;;
;;  This program is free software; you can redistribute it and/or                         ;;
;;  modify it under the terms of the GNU General Public License as                        ;;
;;  published by the Free Software Foundation; either version 3, or                       ;;
;;  (at your option) any later version.                                                   ;;
;;                                                                                        ;;
;;  This program is distributed in the hope that it will be useful,                       ;;
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of                        ;;
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU                     ;;
;;  General Public License for more details.                                              ;;
;;                                                                                        ;;
;;  You should have received a copy of the GNU General Public License                     ;;
;;  along with this program; see the file COPYING.  If not, write to                      ;;
;;  the Free Software Foundation, Inc., 51 Franklin Street, Fifth                         ;;
;;  Floor, Boston, MA 02110-1301, USA.                                                    ;;
;;                                                                                        ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                   ;;
;;                                                                                        ;;
;; ; Code:                                                                                ;;
;; -------------------------------------------------------------------------------------- ;;

(require 'json-mode)
(when (>= emacs-major-version 24)
  (require 'js2-mode)
  (require 'ac-js2)
  (require 'coffee-mode))
(require 'js-comint)

(after-load 'js2-mode
  (define-key js2-mode-map (kbd "TAB") 'indent-for-tab-command))

(defcustom preferred-javascript-mode
  (first (remove-if-not #'fboundp '(js2-mode js-mode)))
  "Javascript mode to use for .js files."
  :type 'symbol
  :group 'programming
  :options '(js2-mode js-mode))
(defvar preferred-javascript-indent-level 2)

;; Need to first remove from list if present, since elpa adds entries too, which
;; may be in an arbitrary order
(eval-when-compile (require 'cl))
(setq auto-mode-alist (cons `("\\.js\\(\\.erb\\)?\\'" . ,preferred-javascript-mode)
                            (loop for entry in auto-mode-alist
                                  unless (eq preferred-javascript-mode (cdr entry))
                                  collect entry)))


;; js2-mode
(after-load 'js2-mode
  (add-hook 'js2-mode-hook '(lambda () (setq mode-name "JS2"))))

(setq js2-use-font-lock-faces t
      js2-mode-must-byte-compile nil
      js2-basic-offset preferred-javascript-indent-level
      js2-indent-on-enter-key t
      js2-auto-indent-p t
      js2-bounce-indent-p nil)

(after-load 'js2-mode
  (js2-imenu-extras-setup))

;; js-mode
(setq js-indent-level preferred-javascript-indent-level)


(add-to-list 'interpreter-mode-alist (cons "node" preferred-javascript-mode))


;; Javascript nests {} and () a lot, so I find this helpful

(require 'rainbow-delimiters)
(dolist (hook '(js2-mode-hook js-mode-hook json-mode-hook))
  (add-hook hook 'rainbow-delimiters-mode))



;;; Coffeescript

(after-load 'coffee-mode
  (setq coffee-js-mode preferred-javascript-mode
        coffee-tab-width preferred-javascript-indent-level))

(when (fboundp 'coffee-mode)
  (add-to-list 'auto-mode-alist '("\\.coffee\\.erb\\'" . coffee-mode)))

;; ---------------------------------------------------------------------------
;; Run and interact with an inferior JS via js-comint.el
;; ---------------------------------------------------------------------------

(setq inferior-js-program-command "js")

(defvar inferior-js-minor-mode-map (make-sparse-keymap))
(define-key inferior-js-minor-mode-map "\C-x\C-e" 'js-send-last-sexp)
(define-key inferior-js-minor-mode-map "\C-\M-x" 'js-send-last-sexp-and-go)
(define-key inferior-js-minor-mode-map "\C-cb" 'js-send-buffer)
(define-key inferior-js-minor-mode-map "\C-c\C-b" 'js-send-buffer-and-go)
(define-key inferior-js-minor-mode-map "\C-cl" 'js-load-file-and-go)

(define-minor-mode inferior-js-keys-mode
  "Bindings for communicating with an inferior js interpreter."
  nil " InfJS" inferior-js-minor-mode-map)

(dolist (hook '(js2-mode-hook js-mode-hook))
  (add-hook hook 'inferior-js-keys-mode))

;; ---------------------------------------------------------------------------
;; Alternatively, use skewer-mode
;; ---------------------------------------------------------------------------

(when (and (>= emacs-major-version 24) (featurep 'js2-mode))
  (require 'skewer-mode)
  (after-load 'skewer-mode
    (add-hook 'skewer-mode-hook
              (lambda () (inferior-js-keys-mode -1)))))


(provide 'init-javascript)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-javascript.el ends here
