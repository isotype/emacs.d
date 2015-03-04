;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Filename: init-company                                         ;;;
;;; Created: 14-02-2015                                            ;;;
;;; Author: Anton Strilchuk <anton@env.sh>                         ;;;
;;; URL: http://ype.env.sh                                         ;;;
;;; Version:                                                       ;;;
;;; Last-Updated: 26-02-2015                                       ;;;
;;;  Update #: 30                                                  ;;;
;;;   By: Anton Strilchuk <anton@env.sh>                           ;;;
;;;                                                                ;;;
;;; Description:                                                   ;;;
;;;                                                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init-company)

(require-package 'company)

;; Use C-\ to activate the Company autocompleter.
(require 'company)
(global-company-mode)
;;(global-set-key (kbd "C-]") 'company-complete)
;; Fix integration of company and yasnippet
;; (define-key company-active-map (kbd "TAB") nil)
;; (define-key company-active-map (kbd "<tab>") nil)
;; (define-key company-active-map [tab] nil)

(setq company-global-modes '(not term-mode))

(setq company-minimum-prefix-length 2
      company-selection-wrap-around t
      company-show-numbers t
      company-tooltip-align-annotations t
      company-require-match nil)

;; Sort completion candidates that already occur in the current
;; buffer at the top of the candidate list.
(setq company-transformers '(company-sort-by-occurrence))

;; Show documentation where available for selected completion
;; after a short delay.
(require-package 'pos-tip)
(require 'pos-tip)
(after-load 'company-quickhelp-mode
  (setq pos-tip-avoid-mouse t
        pos-tip-background-color "#343C40"
        pos-tip-foreground-color "#AA767A"))

(require-package 'company-quickhelp)
(require 'company-quickhelp)
(setq company-quickhelp-delay 1)
(setq company-quickhelp-frontend '(company-pseudo-tooltip-frontend))
(company-quickhelp-mode 1)

;; Company's default colours look OK with the light scheme,
;; but hideous with the dark one, so let's pick something nicer.
(set-face-foreground 'company-tooltip "#000")
(set-face-background 'company-tooltip "#ddd")
(set-face-background 'company-scrollbar-bg "#fff")
(set-face-background 'company-scrollbar-fg "#999")
(set-face-background 'company-tooltip-selection "#aaa")
(set-face-foreground 'company-tooltip-common "#9a0000")
(set-face-foreground 'company-tooltip-common-selection "#9a0000")
(set-face-foreground 'company-tooltip-annotation "#00008e")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;init-company.el ends here
