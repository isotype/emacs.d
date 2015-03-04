;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author: Anton Strilchuk <ype@env.sh>                             ;;
;; URL: http://ype.env.sh                                           ;;
;; Created: 06-10-2014                                              ;;
;;; Last-Updated: 30-01-2015                                       ;;;
;;;  Update #: 74                                                  ;;;
;;;   By: Anton Strilchuk <anton@env.sh>                           ;;;
;;                                                                  ;;
;; Filename: 36-symbols-theme                                       ;;
;; Version: 0.0.0.1                                                 ;;
;; Description: a dark theme for dark people                        ;;
;;                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'dash)

(defcustom 36-symbols-use-more-italic nil
  "Use italic slant more often."
  :type 'boolean
  :group '36-symbols)

(defcustom 36-symbols-height-minus-1 0.8
  "Font size -1."
  :type 'number
  :group '36-symbols)

(defcustom 36-symbols-height-plus-1 1.1
  "Font size +1."
  :type 'number
  :group '36-symbols)

(defcustom 36-symbols-height-plus-2 1.15
  "Font size +2."
  :type 'number
  :group '36-symbols)

(defcustom 36-symbols-height-plus-3 1.2
  "Font size +3."
  :type 'number
  :group '36-symbols)

(defcustom 36-symbols-height-plus-4 1.3
  "Font size +4."
  :type 'number
  :group '36-symbols)

(defcustom 36-symbols-height-plus-5 1.75
  "Font size +4."
  :type 'number
  :group '36-symbols)

(defun hsl-to-hex (hue saturation lightness)
  "Converts HSL values to coorisponding HEX values"
  (let ((color-hsl (color-hsl-to-rgb hue saturation lightness)))
    (color-rgb-to-hex (nth 0 color-hsl) (nth 1 color-hsl) (nth 2 color-hsl))))

;; (let ((count 0))
;;   (while (< count 255)
;;     (setq count (+ count 1))
;;     (when (= (% count 9) 0)
;;       (print (hsl-to-hex  (/ count 100.0) (/ count 100.0) (/ count 360.0))))))

(deftheme 36-symbols "The 36-Symbols color theme")

;;; Color Palette
(defvar 36-symbols-colors-alist
  '(("36-symbols-fg+1"     . "#F6ABB1")
    ("36-symbols-fg"       . "#AA767A")
    ("36-symbols-fg-1"     . "#6A4A4C")
    ("36-symbols-bg-2"     . "#383832")
    ("36-symbols-bg-1"     . "#2B2B2B")
    ("36-symbols-bg-05"    . "#293033")
    ("36-symbols-bg"       . "#343C40")
    ("36-symbols-bg+1"     . "#3b4549")
    ("36-symbols-bg+2"     . "#455156")
    ("36-symbols-bg+3"     . "#526971")
    ("36-symbols-bg+05"    . "#72848C")
    ("36-symbols-magenta"  . "#F6616D")
    ("36-symbols-red+1"    . "#F1A4AA")
    ("36-symbols-red"      . "#a57074")
    ("36-symbols-red-1"    . "#654547")
    ("36-symbols-red-2"    . "#593D3F")
    ("36-symbols-red-3"    . "#4B3435")
    ("36-symbols-red-4"    . "#332324")
    ("36-symbols-orange"   . "#A8554A")
    ("36-symbols-yellow"   . "#f8e56d")
    ("36-symbols-yellow-1" . "#EDDB68")
    ("36-symbols-yellow-2" . "#BFB154")
    ("36-symbols-green-1"  . "#5C7576")
    ("36-symbols-green"    . "#708e90")
    ("36-symbols-green+1"  . "#7A9B9D")
    ("36-symbols-green+2"  . "#8CB1B4")
    ("36-symbols-green+3"  . "#96BEC1")
    ("36-symbols-green+4"  . "#ABD9DC")
    ("36-symbols-cyan"     . "#75EEF5")
    ("36-symbols-blue+1"   . "#627a8c")
    ("36-symbols-blue"     . "#596F7F")
    ("36-symbols-blue-1"   . "#6B8599")
    ("36-symbols-blue-2"   . "#506472")
    ("36-symbols-blue-3"   . "#3E4E59")
    ("36-symbols-blue-4"   . "#35424C")
    ("36-symbols-blue-5"   . "#242C33")
    )
  "List of 36-Symbols colors.
Each element has the form (NAME . HEX).

`+N' suffixes indicate a color is lighter.
`-N' suffixes indicate a color is darker.")

(defmacro 36-symbols-with-color-variables (&rest body)
  "`let' bind all colors defined in `36-symbols-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   36-symbols-colors-alist))
     ,@body))

;;; Theme Faces
(36-symbols-with-color-variables
  (custom-theme-set-faces
   '36-symbols
;;;; Built-in
;;;;; basic coloring
   '(button ((t (:underline t))))
   `(link ((t (:foreground ,36-symbols-yellow :underline t :weight bold))))
   `(link-visited ((t (:foreground ,36-symbols-yellow-2 :underline t :weight normal))))
   `(default ((t (:foreground ,36-symbols-fg :background ,36-symbols-bg))))
   `(cursor ((t (:foreground ,36-symbols-fg :background ,36-symbols-fg+1))))
   `(escape-glyph ((t (:foreground ,36-symbols-yellow :bold t))))
   `(fringe ((t (:foreground ,36-symbols-fg :background ,36-symbols-bg+1))))
   `(header-line ((t (:foreground ,36-symbols-yellow
                                  :background ,36-symbols-bg-1
                                  :box nil))))
   `(highlight ((t (:background ,36-symbols-bg-05))))
   `(success ((t (:foreground ,36-symbols-green :weight bold))))
   `(warning ((t (:foreground ,36-symbols-orange :weight bold))))
;;;;; compilation
   `(compilation-column-face ((t (:foreground ,36-symbols-yellow))))
   `(compilation-enter-directory-face ((t (:foreground ,36-symbols-green))))
   `(compilation-error-face ((t (:foreground ,36-symbols-red-1 :weight bold :underline t))))
   `(compilation-face ((t (:foreground ,36-symbols-fg))))
   `(compilation-info-face ((t (:foreground ,36-symbols-blue))))
   `(compilation-info ((t (:foreground ,36-symbols-green+4 :underline t))))
   `(compilation-leave-directory-face ((t (:foreground ,36-symbols-green))))
   `(compilation-line-face ((t (:foreground ,36-symbols-yellow))))
   `(compilation-line-number ((t (:foreground ,36-symbols-yellow))))
   `(compilation-message-face ((t (:foreground ,36-symbols-blue))))
   `(compilation-warning-face ((t (:foreground ,36-symbols-orange :weight bold :underline t))))
   `(compilation-mode-line-exit ((t (:foreground ,36-symbols-green+2 :weight bold))))
   `(compilation-mode-line-fail ((t (:foreground ,36-symbols-red :weight bold))))
   `(compilation-mode-line-run ((t (:foreground ,36-symbols-yellow :weight bold))))
;;;;; grep
   `(grep-context-face ((t (:foreground ,36-symbols-fg))))
   `(grep-error-face ((t (:foreground ,36-symbols-red-1 :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,36-symbols-blue))))
   `(grep-match-face ((t (:foreground ,36-symbols-orange :weight bold))))
   `(match ((t (:background ,36-symbols-bg-1 :foreground ,36-symbols-orange :weight bold))))
;;;;; isearch
   `(isearch ((t (:foreground ,36-symbols-yellow-2 :weight bold :background ,36-symbols-bg+2))))
   `(isearch-fail ((t (:foreground ,36-symbols-fg :background ,36-symbols-red-4))))
   `(lazy-highlight ((t (:foreground ,36-symbols-yellow-2 :weight bold :background ,36-symbols-bg-05))))

   `(menu ((t (:foreground ,36-symbols-fg :background ,36-symbols-bg))))
   `(minibuffer-prompt ((t (:foreground ,36-symbols-yellow))))
   `(mode-line
     ((,class (:foreground ,36-symbols-green+1
                           :background ,36-symbols-bg-1
                           :box nil))
      (t :inverse-video t)))
   `(mode-line-buffer-id ((t (:foreground ,36-symbols-yellow :weight bold))))
   `(mode-line-inactive
     ((t (:foreground ,36-symbols-green-1
                      :background ,36-symbols-bg-05
                      :box nil))))
   `(region ((,class (:background ,36-symbols-bg-1))
             (t :inverse-video t)))
   `(secondary-selection ((t (:background ,36-symbols-bg+2))))
   `(trailing-whitespace ((t (:background ,36-symbols-red))))
   `(vertical-border ((t (:foreground ,36-symbols-fg))))
;;;;; font lock
   `(font-lock-builtin-face ((t (:foreground ,36-symbols-fg :weight bold))))
   `(font-lock-comment-face ((t (:foreground ,36-symbols-green))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,36-symbols-green-1))))
   `(font-lock-constant-face ((t (:foreground ,36-symbols-green+4))))
   `(font-lock-doc-face ((t (:foreground ,36-symbols-green+2))))
   `(font-lock-function-name-face ((t (:foreground ,36-symbols-cyan))))
   `(font-lock-keyword-face ((t (:foreground ,36-symbols-yellow :weight bold))))
   `(font-lock-negation-char-face ((t (:foreground ,36-symbols-yellow :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,36-symbols-blue+1))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,36-symbols-yellow :weight bold))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,36-symbols-green :weight bold))))
   `(font-lock-string-face ((t (:foreground ,36-symbols-red))))
   `(font-lock-type-face ((t (:foreground ,36-symbols-blue-1))))
   `(font-lock-variable-name-face ((t (:foreground ,36-symbols-orange))))
   `(font-lock-warning-face ((t (:foreground ,36-symbols-yellow-2 :weight bold))))

   `(c-annotation-face ((t (:inherit font-lock-constant-face))))
;;;;; newsticker
   `(newsticker-date-face ((t (:foreground ,36-symbols-fg))))
   `(newsticker-default-face ((t (:foreground ,36-symbols-fg))))
   `(newsticker-enclosure-face ((t (:foreground ,36-symbols-green+3))))
   `(newsticker-extra-face ((t (:foreground ,36-symbols-bg+2 :height 0.8))))
   `(newsticker-feed-face ((t (:foreground ,36-symbols-fg))))
   `(newsticker-immortal-item-face ((t (:foreground ,36-symbols-green))))
   `(newsticker-new-item-face ((t (:foreground ,36-symbols-blue))))
   `(newsticker-obsolete-item-face ((t (:foreground ,36-symbols-red))))
   `(newsticker-old-item-face ((t (:foreground ,36-symbols-bg+3))))
   `(newsticker-statistics-face ((t (:foreground ,36-symbols-fg))))
   `(newsticker-treeview-face ((t (:foreground ,36-symbols-fg))))
   `(newsticker-treeview-immortal-face ((t (:foreground ,36-symbols-green))))
   `(newsticker-treeview-listwindow-face ((t (:foreground ,36-symbols-fg))))
   `(newsticker-treeview-new-face ((t (:foreground ,36-symbols-blue :weight bold))))
   `(newsticker-treeview-obsolete-face ((t (:foreground ,36-symbols-red))))
   `(newsticker-treeview-old-face ((t (:foreground ,36-symbols-bg+3))))
   `(newsticker-treeview-selection-face ((t (:background ,36-symbols-bg-1 :foreground ,36-symbols-yellow))))
;;;; Third-party
;;;;; ace-jump
   `(ace-jump-face-background
     ((t (:foreground ,36-symbols-fg-1 :background ,36-symbols-bg :inverse-video nil))))
   `(ace-jump-face-foreground
     ((t (:foreground ,36-symbols-green+2 :background ,36-symbols-bg :inverse-video nil))))
;;;;; android mode
   `(android-mode-debug-face ((t (:foreground ,36-symbols-green+1))))
   `(android-mode-error-face ((t (:foreground ,36-symbols-orange :weight bold))))
   `(android-mode-info-face ((t (:foreground ,36-symbols-fg))))
   `(android-mode-verbose-face ((t (:foreground ,36-symbols-green))))
   `(android-mode-warning-face ((t (:foreground ,36-symbols-yellow))))
;;;;; anzu
   `(anzu-mode-line ((t (:foreground ,36-symbols-cyan :weight bold))))
;;;;; auctex
   `(font-latex-bold-face ((t (:inherit bold))))
   `(font-latex-warning-face ((t (:foreground nil :inherit font-lock-warning-face))))
   `(font-latex-sectioning-5-face ((t (:foreground ,36-symbols-red :weight bold ))))
   `(font-latex-sedate-face ((t (:foreground ,36-symbols-yellow))))
   `(font-latex-italic-face ((t (:foreground ,36-symbols-cyan :slant italic))))
   `(font-latex-string-face ((t (:inherit ,font-lock-string-face))))
   `(font-latex-math-face ((t (:foreground ,36-symbols-orange))))
;;;;; auto-complete
   `(ac-candidate-face ((t (:background ,36-symbols-bg+3 :foreground ,36-symbols-bg-2))))
   `(ac-selection-face ((t (:background ,36-symbols-blue-4 :foreground ,36-symbols-fg))))
   `(popup-tip-face ((t (:background ,36-symbols-bg+3 :foreground ,36-symbols-green+4))))
   `(popup-scroll-bar-foreground-face ((t (:background ,36-symbols-blue-5))))
   `(popup-scroll-bar-background-face ((t (:background ,36-symbols-bg+3))))
   `(popup-isearch-match ((t (:background ,36-symbols-bg+3 :foreground ,36-symbols-fg))))
;;;;; company-mode
   `(company-tooltip ((t (:foreground ,36-symbols-fg :background ,36-symbols-bg+1))))
   `(company-tooltip-selection ((t (:foreground ,36-symbols-fg :background ,36-symbols-bg-1))))
   `(company-tooltip-mouse ((t (:background ,36-symbols-bg-1))))
   `(company-tooltip-common ((t (:foreground ,36-symbols-green+2))))
   `(company-tooltip-common-selection ((t (:foreground ,36-symbols-green+2))))
   `(company-scrollbar-fg ((t (:background ,36-symbols-green+1))))
   `(company-scrollbar-bg ((t (:background ,36-symbols-bg-1))))
   `(company-preview ((t (:background ,36-symbols-green+1))))
   `(company-preview-common ((t (:background ,36-symbols-bg-1))))
;;;;; bm
   `(bm-face ((t (:background ,36-symbols-yellow-1 :foreground ,36-symbols-bg))))
   `(bm-fringe-face ((t (:background ,36-symbols-yellow-1 :foreground ,36-symbols-bg))))
   `(bm-fringe-persistent-face ((t (:background ,36-symbols-green-1 :foreground ,36-symbols-bg))))
   `(bm-persistent-face ((t (:background ,36-symbols-green-1 :foreground ,36-symbols-bg))))
;;;;; clojure-test-mode
   `(clojure-test-failure-face ((t (:foreground ,36-symbols-orange :weight bold :underline t))))
   `(clojure-test-error-face ((t (:foreground ,36-symbols-red :weight bold :underline t))))
   `(clojure-test-success-face ((t (:foreground ,36-symbols-green+1 :weight bold :underline t))))
;;;;; coq
   `(coq-solve-tactics-face ((t (:foreground nil :inherit font-lock-constant-face))))
;;;;; ctable
   `(ctbl:face-cell-select ((t (:background ,36-symbols-blue :foreground ,36-symbols-bg))))
   `(ctbl:face-continue-bar ((t (:background ,36-symbols-bg-05 :foreground ,36-symbols-bg))))
   `(ctbl:face-row-select ((t (:background ,36-symbols-cyan :foreground ,36-symbols-bg))))
;;;;; diff
   `(diff-added ((,class (:foreground ,36-symbols-green+4 :background nil))
                 (t (:foreground ,36-symbols-green-1 :background nil))))
   `(diff-changed ((t (:foreground ,36-symbols-yellow))))
   `(diff-removed ((,class (:foreground ,36-symbols-red :background nil))
                   (t (:foreground ,36-symbols-red-3 :background nil))))
   `(diff-refine-added ((t (:inherit diff-added :weight bold))))
   `(diff-refine-change ((t (:inherit diff-changed :weight bold))))
   `(diff-refine-removed ((t (:inherit diff-removed :weight bold))))
   `(diff-header ((,class (:background ,36-symbols-bg+2))
                  (t (:background ,36-symbols-fg :foreground ,36-symbols-bg))))
   `(diff-file-header
     ((,class (:background ,36-symbols-bg+2 :foreground ,36-symbols-fg :bold t))
      (t (:background ,36-symbols-fg :foreground ,36-symbols-bg :bold t))))
;;;;; diff-hl
   `(diff-hl-change ((,class (:foreground ,36-symbols-blue-2 :background ,36-symbols-bg-05))))
   `(diff-hl-delete ((,class (:foreground ,36-symbols-red+1 :background ,36-symbols-bg-05))))
   `(diff-hl-insert ((,class (:foreground ,36-symbols-green+1 :background ,36-symbols-bg-05))))
   `(diff-hl-unknown ((,class (:foreground ,36-symbols-yellow :background ,36-symbols-bg-05))))
;;;;; dim-autoload
   `(dim-autoload-cookie-line ((t :foreground ,36-symbols-bg+1)))
;;;;; dired+
   `(diredp-display-msg ((t (:foreground ,36-symbols-blue))))
   `(diredp-compressed-file-suffix ((t (:foreground ,36-symbols-orange))))
   `(diredp-date-time ((t (:foreground ,36-symbols-magenta))))
   `(diredp-deletion ((t (:foreground ,36-symbols-yellow))))
   `(diredp-deletion-file-name ((t (:foreground ,36-symbols-red))))
   `(diredp-dir-heading ((t (:foreground ,36-symbols-blue :background ,36-symbols-bg-1))))
   `(diredp-dir-priv ((t (:foreground ,36-symbols-cyan))))
   `(diredp-exec-priv ((t (:foreground ,36-symbols-red))))
   `(diredp-executable-tag ((t (:foreground ,36-symbols-green+1))))
   `(diredp-file-name ((t (:foreground ,36-symbols-blue))))
   `(diredp-file-suffix ((t (:foreground ,36-symbols-green))))
   `(diredp-flag-mark ((t (:foreground ,36-symbols-yellow))))
   `(diredp-flag-mark-line ((t (:foreground ,36-symbols-orange))))
   `(diredp-ignored-file-name ((t (:foreground ,36-symbols-red))))
   `(diredp-link-priv ((t (:foreground ,36-symbols-yellow))))
   `(diredp-mode-line-flagged ((t (:foreground ,36-symbols-yellow))))
   `(diredp-mode-line-marked ((t (:foreground ,36-symbols-orange))))
   `(diredp-no-priv ((t (:foreground ,36-symbols-fg))))
   `(diredp-number ((t (:foreground ,36-symbols-green+1))))
   `(diredp-other-priv ((t (:foreground ,36-symbols-yellow-1))))
   `(diredp-rare-priv ((t (:foreground ,36-symbols-red-1))))
   `(diredp-read-priv ((t (:foreground ,36-symbols-green-1))))
   `(diredp-symlink ((t (:foreground ,36-symbols-yellow))))
   `(diredp-write-priv ((t (:foreground ,36-symbols-magenta))))
;;;;; ediff
   `(ediff-current-diff-A ((t (:foreground ,36-symbols-fg :background ,36-symbols-red-4))))
   `(ediff-current-diff-Ancestor ((t (:foreground ,36-symbols-fg :background ,36-symbols-red-4))))
   `(ediff-current-diff-B ((t (:foreground ,36-symbols-fg :background ,36-symbols-green-1))))
   `(ediff-current-diff-C ((t (:foreground ,36-symbols-fg :background ,36-symbols-blue-5))))
   `(ediff-even-diff-A ((t (:background ,36-symbols-bg+1))))
   `(ediff-even-diff-Ancestor ((t (:background ,36-symbols-bg+1))))
   `(ediff-even-diff-B ((t (:background ,36-symbols-bg+1))))
   `(ediff-even-diff-C ((t (:background ,36-symbols-bg+1))))
   `(ediff-fine-diff-A ((t (:foreground ,36-symbols-fg :background ,36-symbols-red-2 :weight bold))))
   `(ediff-fine-diff-Ancestor ((t (:foreground ,36-symbols-fg :background ,36-symbols-red-2 weight bold))))
   `(ediff-fine-diff-B ((t (:foreground ,36-symbols-fg :background ,36-symbols-green :weight bold))))
   `(ediff-fine-diff-C ((t (:foreground ,36-symbols-fg :background ,36-symbols-blue-3 :weight bold ))))
   `(ediff-odd-diff-A ((t (:background ,36-symbols-bg+2))))
   `(ediff-odd-diff-Ancestor ((t (:background ,36-symbols-bg+2))))
   `(ediff-odd-diff-B ((t (:background ,36-symbols-bg+2))))
   `(ediff-odd-diff-C ((t (:background ,36-symbols-bg+2))))
;;;;; egg
   `(egg-text-base ((t (:foreground ,36-symbols-fg))))
   `(egg-help-header-1 ((t (:foreground ,36-symbols-yellow))))
   `(egg-help-header-2 ((t (:foreground ,36-symbols-green+3))))
   `(egg-branch ((t (:foreground ,36-symbols-yellow))))
   `(egg-branch-mono ((t (:foreground ,36-symbols-yellow))))
   `(egg-term ((t (:foreground ,36-symbols-yellow))))
   `(egg-diff-add ((t (:foreground ,36-symbols-green+4))))
   `(egg-diff-del ((t (:foreground ,36-symbols-red+1))))
   `(egg-diff-file-header ((t (:foreground ,36-symbols-yellow-2))))
   `(egg-section-title ((t (:foreground ,36-symbols-yellow))))
   `(egg-stash-mono ((t (:foreground ,36-symbols-green+4))))
;;;;; elfeed
   `(elfeed-search-date-face ((t (:foreground ,36-symbols-yellow-1 :underline t
                                              :weight bold))))
   `(elfeed-search-tag-face ((t (:foreground ,36-symbols-green))))
   `(elfeed-search-feed-face ((t (:foreground ,36-symbols-cyan))))
;;;;; emacs-w3m
   `(w3m-anchor ((t (:foreground ,36-symbols-yellow :underline t
                                 :weight bold))))
   `(w3m-arrived-anchor ((t (:foreground ,36-symbols-yellow-2
                                         :underline t :weight normal))))
   `(w3m-form ((t (:foreground ,36-symbols-red-1 :underline t))))
   `(w3m-header-line-location-title ((t (:foreground ,36-symbols-yellow
                                                     :underline t :weight bold))))
   '(w3m-history-current-url ((t (:inherit match))))
   `(w3m-lnum ((t (:foreground ,36-symbols-green+2 :background ,36-symbols-bg))))
   `(w3m-lnum-match ((t (:background ,36-symbols-bg-1
                                     :foreground ,36-symbols-orange
                                     :weight bold))))
   `(w3m-lnum-minibuffer-prompt ((t (:foreground ,36-symbols-yellow))))
;;;;; erc
   `(erc-action-face ((t (:inherit erc-default-face))))
   `(erc-bold-face ((t (:weight bold))))
   `(erc-current-nick-face ((t (:foreground ,36-symbols-blue :weight bold))))
   `(erc-dangerous-host-face ((t (:inherit font-lock-warning-face))))
   `(erc-default-face ((t (:foreground ,36-symbols-fg))))
   `(erc-direct-msg-face ((t (:inherit erc-default))))
   `(erc-error-face ((t (:inherit font-lock-warning-face))))
   `(erc-fool-face ((t (:inherit erc-default))))
   `(erc-highlight-face ((t (:inherit hover-highlight))))
   `(erc-input-face ((t (:foreground ,36-symbols-yellow))))
   `(erc-keyword-face ((t (:foreground ,36-symbols-blue :weight bold))))
   `(erc-nick-default-face ((t (:foreground ,36-symbols-yellow :weight bold))))
   `(erc-my-nick-face ((t (:foreground ,36-symbols-red :weight bold))))
   `(erc-nick-msg-face ((t (:inherit erc-default))))
   `(erc-notice-face ((t (:foreground ,36-symbols-green))))
   `(erc-pal-face ((t (:foreground ,36-symbols-orange :weight bold))))
   `(erc-prompt-face ((t (:foreground ,36-symbols-orange :background ,36-symbols-bg :weight bold))))
   `(erc-timestamp-face ((t (:foreground ,36-symbols-green+4))))
   `(erc-underline-face ((t (:underline t))))
;;;;; ert
   `(ert-test-result-expected ((t (:foreground ,36-symbols-green+4 :background ,36-symbols-bg))))
   `(ert-test-result-unexpected ((t (:foreground ,36-symbols-red :background ,36-symbols-bg))))
;;;;; eshell
   `(eshell-prompt ((t (:foreground ,36-symbols-yellow :weight bold))))
   `(eshell-ls-archive ((t (:foreground ,36-symbols-red-1 :weight bold))))
   `(eshell-ls-backup ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-clutter ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-directory ((t (:foreground ,36-symbols-blue+1 :weight bold))))
   `(eshell-ls-executable ((t (:foreground ,36-symbols-red+1 :weight bold))))
   `(eshell-ls-unreadable ((t (:foreground ,36-symbols-fg))))
   `(eshell-ls-missing ((t (:inherit font-lock-warning-face))))
   `(eshell-ls-product ((t (:inherit font-lock-doc-face))))
   `(eshell-ls-special ((t (:foreground ,36-symbols-yellow :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,36-symbols-cyan :weight bold))))
;;;;; flx
   `(flx-highlight-face ((t (:foreground ,36-symbols-green+2 :weight bold))))
;;;;; flycheck
   `(flycheck-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,36-symbols-red-1) :inherit unspecified))
      (t (:foreground ,36-symbols-red-1 :weight bold :underline t))))
   `(flycheck-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,36-symbols-yellow) :inherit unspecified))
      (t (:foreground ,36-symbols-yellow :weight bold :underline t))))
   `(flycheck-info
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,36-symbols-cyan) :inherit unspecified))
      (t (:foreground ,36-symbols-cyan :weight bold :underline t))))
   `(flycheck-fringe-error ((t (:foreground ,36-symbols-red-1 :weight bold))))
   `(flycheck-fringe-warning ((t (:foreground ,36-symbols-yellow :weight bold))))
   `(flycheck-fringe-info ((t (:foreground ,36-symbols-cyan :weight bold))))
;;;;; flymake
   `(flymake-errline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,36-symbols-red)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,36-symbols-red-1 :weight bold :underline t))))
   `(flymake-warnline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,36-symbols-orange)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,36-symbols-orange :weight bold :underline t))))
   `(flymake-infoline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,36-symbols-green)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,36-symbols-green-1 :weight bold :underline t))))
;;;;; flyspell
   `(flyspell-duplicate
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,36-symbols-orange) :inherit unspecified))
      (t (:foreground ,36-symbols-orange :weight bold :underline t))))
   `(flyspell-incorrect
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,36-symbols-red) :inherit unspecified))
      (t (:foreground ,36-symbols-red-1 :weight bold :underline t))))
;;;;; full-ack
   `(ack-separator ((t (:foreground ,36-symbols-fg))))
   `(ack-file ((t (:foreground ,36-symbols-blue))))
   `(ack-line ((t (:foreground ,36-symbols-yellow))))
   `(ack-match ((t (:foreground ,36-symbols-orange :background ,36-symbols-bg-1 :weight bold))))
;;;;; git-gutter
   `(git-gutter:added ((t (:foreground ,36-symbols-green :weight bold :inverse-video t))))
   `(git-gutter:deleted ((t (:foreground ,36-symbols-red :weight bold :inverse-video t))))
   `(git-gutter:modified ((t (:foreground ,36-symbols-magenta :weight bold :inverse-video t))))
   `(git-gutter:unchanged ((t (:foreground ,36-symbols-fg :weight bold :inverse-video t))))
;;;;; git-gutter-fr
   `(git-gutter-fr:added ((t (:foreground ,36-symbols-green  :weight bold))))
   `(git-gutter-fr:deleted ((t (:foreground ,36-symbols-red :weight bold))))
   `(git-gutter-fr:modified ((t (:foreground ,36-symbols-magenta :weight bold))))
;;;;; git-rebase-mode
   `(git-rebase-hash ((t (:foreground ,36-symbols-orange))))
;;;;; gnus
   `(gnus-group-mail-1 ((t (:bold t :inherit gnus-group-mail-1-empty))))
   `(gnus-group-mail-1-empty ((t (:inherit gnus-group-news-1-empty))))
   `(gnus-group-mail-2 ((t (:bold t :inherit gnus-group-mail-2-empty))))
   `(gnus-group-mail-2-empty ((t (:inherit gnus-group-news-2-empty))))
   `(gnus-group-mail-3 ((t (:bold t :inherit gnus-group-mail-3-empty))))
   `(gnus-group-mail-3-empty ((t (:inherit gnus-group-news-3-empty))))
   `(gnus-group-mail-4 ((t (:bold t :inherit gnus-group-mail-4-empty))))
   `(gnus-group-mail-4-empty ((t (:inherit gnus-group-news-4-empty))))
   `(gnus-group-mail-5 ((t (:bold t :inherit gnus-group-mail-5-empty))))
   `(gnus-group-mail-5-empty ((t (:inherit gnus-group-news-5-empty))))
   `(gnus-group-mail-6 ((t (:bold t :inherit gnus-group-mail-6-empty))))
   `(gnus-group-mail-6-empty ((t (:inherit gnus-group-news-6-empty))))
   `(gnus-group-mail-low ((t (:bold t :inherit gnus-group-mail-low-empty))))
   `(gnus-group-mail-low-empty ((t (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-1 ((t (:bold t :inherit gnus-group-news-1-empty))))
   `(gnus-group-news-2 ((t (:bold t :inherit gnus-group-news-2-empty))))
   `(gnus-group-news-3 ((t (:bold t :inherit gnus-group-news-3-empty))))
   `(gnus-group-news-4 ((t (:bold t :inherit gnus-group-news-4-empty))))
   `(gnus-group-news-5 ((t (:bold t :inherit gnus-group-news-5-empty))))
   `(gnus-group-news-6 ((t (:bold t :inherit gnus-group-news-6-empty))))
   `(gnus-group-news-low ((t (:bold t :inherit gnus-group-news-low-empty))))
   `(gnus-header-content ((t (:inherit message-header-other))))
   `(gnus-header-from ((t (:inherit message-header-from))))
   `(gnus-header-name ((t (:inherit message-header-name))))
   `(gnus-header-newsgroups ((t (:inherit message-header-other))))
   `(gnus-header-subject ((t (:inherit message-header-subject))))
   `(gnus-summary-cancelled ((t (:foreground ,36-symbols-orange))))
   `(gnus-summary-high-ancient ((t (:foreground ,36-symbols-blue))))
   `(gnus-summary-high-read ((t (:foreground ,36-symbols-green :weight bold))))
   `(gnus-summary-high-ticked ((t (:foreground ,36-symbols-orange :weight bold))))
   `(gnus-summary-high-unread ((t (:foreground ,36-symbols-fg :weight bold))))
   `(gnus-summary-low-ancient ((t (:foreground ,36-symbols-blue))))
   `(gnus-summary-low-read ((t (:foreground ,36-symbols-green))))
   `(gnus-summary-low-ticked ((t (:foreground ,36-symbols-orange :weight bold))))
   `(gnus-summary-low-unread ((t (:foreground ,36-symbols-fg))))
   `(gnus-summary-normal-ancient ((t (:foreground ,36-symbols-blue))))
   `(gnus-summary-normal-read ((t (:foreground ,36-symbols-green))))
   `(gnus-summary-normal-ticked ((t (:foreground ,36-symbols-orange :weight bold))))
   `(gnus-summary-normal-unread ((t (:foreground ,36-symbols-fg))))
   `(gnus-summary-selected ((t (:foreground ,36-symbols-yellow :weight bold))))
   `(gnus-cite-1 ((t (:foreground ,36-symbols-blue))))
   `(gnus-cite-10 ((t (:foreground ,36-symbols-yellow-1))))
   `(gnus-cite-11 ((t (:foreground ,36-symbols-yellow))))
   `(gnus-cite-2 ((t (:foreground ,36-symbols-blue-1))))
   `(gnus-cite-3 ((t (:foreground ,36-symbols-blue-2))))
   `(gnus-cite-4 ((t (:foreground ,36-symbols-green+2))))
   `(gnus-cite-5 ((t (:foreground ,36-symbols-green+1))))
   `(gnus-cite-6 ((t (:foreground ,36-symbols-green))))
   `(gnus-cite-7 ((t (:foreground ,36-symbols-red))))
   `(gnus-cite-8 ((t (:foreground ,36-symbols-red-1))))
   `(gnus-cite-9 ((t (:foreground ,36-symbols-red-2))))
   `(gnus-group-news-1-empty ((t (:foreground ,36-symbols-yellow))))
   `(gnus-group-news-2-empty ((t (:foreground ,36-symbols-green+3))))
   `(gnus-group-news-3-empty ((t (:foreground ,36-symbols-green+1))))
   `(gnus-group-news-4-empty ((t (:foreground ,36-symbols-blue-2))))
   `(gnus-group-news-5-empty ((t (:foreground ,36-symbols-blue-3))))
   `(gnus-group-news-6-empty ((t (:foreground ,36-symbols-bg+2))))
   `(gnus-group-news-low-empty ((t (:foreground ,36-symbols-bg+2))))
   `(gnus-signature ((t (:foreground ,36-symbols-yellow))))
   `(gnus-x ((t (:background ,36-symbols-fg :foreground ,36-symbols-bg))))
;;;;; guide-key
   `(guide-key/highlight-command-face ((t (:foreground ,36-symbols-blue))))
   `(guide-key/key-face ((t (:foreground ,36-symbols-green))))
   `(guide-key/prefix-command-face ((t (:foreground ,36-symbols-green+1))))
;;;;; helm
   `(helm-header
     ((t (:foreground ,36-symbols-green
                      :background ,36-symbols-bg
                      :underline nil
                      :box nil))))
   `(helm-source-header
     ((t (:foreground ,36-symbols-yellow
                      :background ,36-symbols-bg-1
                      :underline nil
                      :weight bold
                      :box nil))))
   `(helm-selection ((t (:background ,36-symbols-bg+1 :underline nil))))
   `(helm-selection-line ((t (:background ,36-symbols-bg+1))))
   `(helm-visible-mark ((t (:foreground ,36-symbols-bg :background ,36-symbols-yellow-2))))
   `(helm-candidate-number ((t (:foreground ,36-symbols-green+4 :background ,36-symbols-bg-1))))
   `(helm-separator ((t (:foreground ,36-symbols-red :background ,36-symbols-bg))))
   `(helm-time-zone-current ((t (:foreground ,36-symbols-green+2 :background ,36-symbols-bg))))
   `(helm-time-zone-home ((t (:foreground ,36-symbols-red :background ,36-symbols-bg))))
   `(helm-bookmark-addressbook ((t (:foreground ,36-symbols-orange :background ,36-symbols-bg))))
   `(helm-bookmark-directory ((t (:foreground nil :background nil :inherit helm-ff-directory))))
   `(helm-bookmark-file ((t (:foreground nil :background nil :inherit helm-ff-file))))
   `(helm-bookmark-gnus ((t (:foreground ,36-symbols-magenta :background ,36-symbols-bg))))
   `(helm-bookmark-info ((t (:foreground ,36-symbols-green+2 :background ,36-symbols-bg))))
   `(helm-bookmark-man ((t (:foreground ,36-symbols-yellow :background ,36-symbols-bg))))
   `(helm-bookmark-w3m ((t (:foreground ,36-symbols-magenta :background ,36-symbols-bg))))
   `(helm-buffer-not-saved ((t (:foreground ,36-symbols-red :background ,36-symbols-bg))))
   `(helm-buffer-process ((t (:foreground ,36-symbols-cyan :background ,36-symbols-bg))))
   `(helm-buffer-saved-out ((t (:foreground ,36-symbols-fg :background ,36-symbols-bg))))
   `(helm-buffer-size ((t (:foreground ,36-symbols-fg-1 :background ,36-symbols-bg))))
   `(helm-ff-directory ((t (:foreground ,36-symbols-cyan :background ,36-symbols-bg :weight bold))))
   `(helm-ff-file ((t (:foreground ,36-symbols-fg :background ,36-symbols-bg :weight normal))))
   `(helm-ff-executable ((t (:foreground ,36-symbols-green+2 :background ,36-symbols-bg :weight normal))))
   `(helm-ff-invalid-symlink ((t (:foreground ,36-symbols-red :background ,36-symbols-bg :weight bold))))
   `(helm-ff-symlink ((t (:foreground ,36-symbols-yellow :background ,36-symbols-bg :weight bold))))
   `(helm-ff-prefix ((t (:foreground ,36-symbols-bg :background ,36-symbols-yellow :weight normal))))
   `(helm-grep-cmd-line ((t (:foreground ,36-symbols-cyan :background ,36-symbols-bg))))
   `(helm-grep-file ((t (:foreground ,36-symbols-fg :background ,36-symbols-bg))))
   `(helm-grep-finish ((t (:foreground ,36-symbols-green+2 :background ,36-symbols-bg))))
   `(helm-grep-lineno ((t (:foreground ,36-symbols-fg-1 :background ,36-symbols-bg))))
   `(helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))
   `(helm-grep-running ((t (:foreground ,36-symbols-red :background ,36-symbols-bg))))
   `(helm-moccur-buffer ((t (:foreground ,36-symbols-cyan :background ,36-symbols-bg))))
   `(helm-mu-contacts-address-face ((t (:foreground ,36-symbols-fg-1 :background ,36-symbols-bg))))
   `(helm-mu-contacts-name-face ((t (:foreground ,36-symbols-fg :background ,36-symbols-bg))))
;;;;; hl-line-mode
   `(hl-line-face ((,class (:background ,36-symbols-bg-05))
                   (t :weight bold)))
   `(hl-line ((,class (:background ,36-symbols-bg-05)) ; old emacsen
              (t :weight bold)))
;;;;; hl-sexp
   `(hl-sexp-face ((,class (:background ,36-symbols-bg+1))
                   (t :weight bold)))
;;;;; ido-mode
   `(ido-first-match ((t (:foreground ,36-symbols-yellow :weight bold))))
   `(ido-only-match ((t (:foreground ,36-symbols-orange :weight bold))))
   `(ido-subdir ((t (:foreground ,36-symbols-yellow))))
   `(ido-indicator ((t (:foreground ,36-symbols-yellow :background ,36-symbols-red-4))))
;;;;; iedit-mode
   `(iedit-occurrence ((t (:background ,36-symbols-bg+2 :weight bold))))
;;;;; jabber-mode
   `(jabber-roster-user-away ((t (:foreground ,36-symbols-green+2))))
   `(jabber-roster-user-online ((t (:foreground ,36-symbols-blue-1))))
   `(jabber-roster-user-dnd ((t (:foreground ,36-symbols-red+1))))
   `(jabber-rare-time-face ((t (:foreground ,36-symbols-green+1))))
   `(jabber-chat-prompt-local ((t (:foreground ,36-symbols-blue-1))))
   `(jabber-chat-prompt-foreign ((t (:foreground ,36-symbols-red+1))))
   `(jabber-activity-face((t (:foreground ,36-symbols-red+1))))
   `(jabber-activity-personal-face ((t (:foreground ,36-symbols-blue+1))))
   `(jabber-title-small ((t (:height 1.1 :weight bold))))
   `(jabber-title-medium ((t (:height 1.2 :weight bold))))
   `(jabber-title-large ((t (:height 1.3 :weight bold))))
;;;;; js2-mode
   `(js2-warning ((t (:underline ,36-symbols-orange))))
   `(js2-error ((t (:foreground ,36-symbols-red :weight bold))))
   `(js2-jsdoc-tag ((t (:foreground ,36-symbols-green-1))))
   `(js2-jsdoc-type ((t (:foreground ,36-symbols-green+2))))
   `(js2-jsdoc-value ((t (:foreground ,36-symbols-green+3))))
   `(js2-function-param ((t (:foreground ,36-symbols-green+3))))
   `(js2-external-variable ((t (:foreground ,36-symbols-orange))))
;;;;; ledger-mode
   `(ledger-font-payee-uncleared-face ((t (:foreground ,36-symbols-fg+1 :weight bold))))
   `(ledger-font-payee-cleared-face ((t (:foreground ,36-symbols-fg :weight normal))))
   `(ledger-font-xact-highlight-face ((t (:background ,36-symbols-cyan))))
   `(ledger-font-pending-face ((t (:foreground ,36-symbols-yellow weight: normal))))
   `(ledger-font-other-face ((t (:foreground ,36-symbols-fg+1))))
   `(ledger-font-posting-account-face ((t (:foreground ,36-symbols-blue-1))))
   `(ledger-font-posting-account-cleared-face ((t (:foreground ,36-symbols-fg))))
   `(ledger-font-posting-account-pending-face ((t (:foreground ,36-symbols-yellow))))
   `(ledger-font-posting-amount-face ((t (:foreground ,36-symbols-yellow))))
   `(ledger-font-posting-account-pending-face ((t (:foreground ,36-symbols-yellow))))
   `(ledger-occur-narrowed-face ((t (:foreground ,36-symbols-red+1 :invisible t))))
   `(ledger-occur-xact-face ((t (:background ,36-symbols-green+2))))
   `(ledger-font-comment-face ((t (:foreground ,36-symbols-green))))
   `(ledger-font-reconciler-uncleared-face ((t (:foreground ,36-symbols-red-1 :weight bold))))
   `(ledger-font-reconciler-cleared-face ((t (:foreground ,36-symbols-magenta :weight normal))))
   `(ledger-font-reconciler-pending-face ((t (:foreground ,36-symbols-yellow :weight normal))))
   `(ledger-font-report-clickable-face ((t (:foreground ,36-symbols-green+3 :weight normal))))
;;;;; linum-mode
   `(linum ((t (:foreground ,36-symbols-green+2 :background ,36-symbols-bg))))
;;;;; macrostep
   `(macrostep-gensym-1
     ((t (:foreground ,36-symbols-green+2 :background ,36-symbols-bg-1))))
   `(macrostep-gensym-2
     ((t (:foreground ,36-symbols-red+1 :background ,36-symbols-bg-1))))
   `(macrostep-gensym-3
     ((t (:foreground ,36-symbols-blue+1 :background ,36-symbols-bg-1))))
   `(macrostep-gensym-4
     ((t (:foreground ,36-symbols-magenta :background ,36-symbols-bg-1))))
   `(macrostep-gensym-5
     ((t (:foreground ,36-symbols-yellow :background ,36-symbols-bg-1))))
   `(macrostep-expansion-highlight-face
     ((t (:inherit highlight))))
   `(macrostep-macro-face
     ((t (:underline t))))
;;;;; magit
   `(magit-item-highlight ((t (:background ,36-symbols-bg+05))))
   `(magit-section-title ((t (:foreground ,36-symbols-yellow :weight bold))))
   `(magit-process-ok ((t (:foreground ,36-symbols-green :weight bold))))
   `(magit-process-ng ((t (:foreground ,36-symbols-red :weight bold))))
   `(magit-branch ((t (:foreground ,36-symbols-blue :weight bold))))
   `(magit-log-author ((t (:foreground ,36-symbols-orange))))
   `(magit-log-sha1 ((t (:foreground ,36-symbols-orange))))
;;;;; message-mode
   `(message-cited-text ((t (:inherit font-lock-comment-face))))
   `(message-header-name ((t (:foreground ,36-symbols-green+1))))
   `(message-header-other ((t (:foreground ,36-symbols-green))))
   `(message-header-to ((t (:foreground ,36-symbols-yellow :weight bold))))
   `(message-header-from ((t (:foreground ,36-symbols-yellow :weight bold))))
   `(message-header-cc ((t (:foreground ,36-symbols-yellow :weight bold))))
   `(message-header-newsgroups ((t (:foreground ,36-symbols-yellow :weight bold))))
   `(message-header-subject ((t (:foreground ,36-symbols-orange :weight bold))))
   `(message-header-xheader ((t (:foreground ,36-symbols-green))))
   `(message-mml ((t (:foreground ,36-symbols-yellow :weight bold))))
   `(message-separator ((t (:inherit font-lock-comment-face))))
;;;;; mew
   `(mew-face-header-subject ((t (:foreground ,36-symbols-orange))))
   `(mew-face-header-from ((t (:foreground ,36-symbols-yellow))))
   `(mew-face-header-date ((t (:foreground ,36-symbols-green))))
   `(mew-face-header-to ((t (:foreground ,36-symbols-red))))
   `(mew-face-header-key ((t (:foreground ,36-symbols-green))))
   `(mew-face-header-private ((t (:foreground ,36-symbols-green))))
   `(mew-face-header-important ((t (:foreground ,36-symbols-blue))))
   `(mew-face-header-marginal ((t (:foreground ,36-symbols-fg :weight bold))))
   `(mew-face-header-warning ((t (:foreground ,36-symbols-red))))
   `(mew-face-header-xmew ((t (:foreground ,36-symbols-green))))
   `(mew-face-header-xmew-bad ((t (:foreground ,36-symbols-red))))
   `(mew-face-body-url ((t (:foreground ,36-symbols-orange))))
   `(mew-face-body-comment ((t (:foreground ,36-symbols-fg :slant italic))))
   `(mew-face-body-cite1 ((t (:foreground ,36-symbols-green))))
   `(mew-face-body-cite2 ((t (:foreground ,36-symbols-blue))))
   `(mew-face-body-cite3 ((t (:foreground ,36-symbols-orange))))
   `(mew-face-body-cite4 ((t (:foreground ,36-symbols-yellow))))
   `(mew-face-body-cite5 ((t (:foreground ,36-symbols-red))))
   `(mew-face-mark-review ((t (:foreground ,36-symbols-blue))))
   `(mew-face-mark-escape ((t (:foreground ,36-symbols-green))))
   `(mew-face-mark-delete ((t (:foreground ,36-symbols-red))))
   `(mew-face-mark-unlink ((t (:foreground ,36-symbols-yellow))))
   `(mew-face-mark-refile ((t (:foreground ,36-symbols-green))))
   `(mew-face-mark-unread ((t (:foreground ,36-symbols-red-2))))
   `(mew-face-eof-message ((t (:foreground ,36-symbols-green))))
   `(mew-face-eof-part ((t (:foreground ,36-symbols-yellow))))
;;;;; mic-paren
   `(paren-face-match ((t (:foreground ,36-symbols-cyan :background ,36-symbols-bg :weight bold))))
   `(paren-face-mismatch ((t (:foreground ,36-symbols-bg :background ,36-symbols-magenta :weight bold))))
   `(paren-face-no-match ((t (:foreground ,36-symbols-bg :background ,36-symbols-red :weight bold))))
;;;;; mingus
   `(mingus-directory-face ((t (:foreground ,36-symbols-blue))))
   `(mingus-pausing-face ((t (:foreground ,36-symbols-magenta))))
   `(mingus-playing-face ((t (:foreground ,36-symbols-cyan))))
   `(mingus-playlist-face ((t (:foreground ,36-symbols-cyan ))))
   `(mingus-song-file-face ((t (:foreground ,36-symbols-yellow))))
   `(mingus-stopped-face ((t (:foreground ,36-symbols-red))))
;;;;; nav
   `(nav-face-heading ((t (:foreground ,36-symbols-yellow))))
   `(nav-face-button-num ((t (:foreground ,36-symbols-cyan))))
   `(nav-face-dir ((t (:foreground ,36-symbols-green))))
   `(nav-face-hdir ((t (:foreground ,36-symbols-red))))
   `(nav-face-file ((t (:foreground ,36-symbols-fg))))
   `(nav-face-hfile ((t (:foreground ,36-symbols-red-4))))
;;;;; mu4e
   `(mu4e-cited-1-face ((t (:foreground ,36-symbols-blue    :slant italic))))
   `(mu4e-cited-2-face ((t (:foreground ,36-symbols-green+2 :slant italic))))
   `(mu4e-cited-3-face ((t (:foreground ,36-symbols-blue-2  :slant italic))))
   `(mu4e-cited-4-face ((t (:foreground ,36-symbols-green   :slant italic))))
   `(mu4e-cited-5-face ((t (:foreground ,36-symbols-blue-4  :slant italic))))
   `(mu4e-cited-6-face ((t (:foreground ,36-symbols-green-1 :slant italic))))
   `(mu4e-cited-7-face ((t (:foreground ,36-symbols-blue    :slant italic))))
   `(mu4e-replied-face ((t (:foreground ,36-symbols-bg+3))))
   `(mu4e-trashed-face ((t (:foreground ,36-symbols-bg+3 :strike-through t))))
;;;;; mu4e maildirs
   `(mu4e-maildirs-extension-maildir-unread-face ((t (:foreground ,36-symbols-magenta :underline t :weight semi-light))))
;;;;; mumamo
   `(mumamo-background-chunk-major ((t (:background nil))))
   `(mumamo-background-chunk-submode1 ((t (:background ,36-symbols-bg-1))))
   `(mumamo-background-chunk-submode2 ((t (:background ,36-symbols-bg+2))))
   `(mumamo-background-chunk-submode3 ((t (:background ,36-symbols-bg+3))))
   `(mumamo-background-chunk-submode4 ((t (:background ,36-symbols-bg+1))))
;;;;; org-mode
   `(org-agenda-date-today
     ((t (:foreground ,36-symbols-fg+1 :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((t (:inherit font-lock-comment-face))))
   `(org-archived ((t (:foreground ,36-symbols-fg :weight bold))))
   `(org-checkbox ((t (:background ,36-symbols-bg+2 :foreground ,36-symbols-fg+1
                                   :box nil))))
   `(org-date ((t (:foreground ,36-symbols-blue :underline t))))
   `(org-deadline-announce ((t (:foreground ,36-symbols-red-1))))
   `(org-agenda-done ((t (:foreground ,36-symbols-green+3 :strike-through nil))))
   `(org-done ((t (:bold t :weight bold :foreground ,36-symbols-green+3 :strike-through t))))
   `(org-formula ((t (:foreground ,36-symbols-yellow-2))))
   `(org-headline-done ((t (:foreground ,36-symbols-green+3))))
   `(org-hide ((t (:foreground ,36-symbols-bg-1))))
   `(org-level-1 ((t (:height ,36-symbols-height-plus-4
                              :foreground ,36-symbols-orange))))
   `(org-level-2 ((t (:height ,36-symbols-height-plus-3
                              :foreground ,36-symbols-green+4))))
   `(org-level-3 ((t (:height ,36-symbols-height-plus-2
                              :foreground ,36-symbols-blue-1))))
   `(org-level-4 ((t (:height ,36-symbols-height-plus-1
                              :foreground ,36-symbols-yellow-2))))
   `(org-level-5 ((t (:foreground ,36-symbols-cyan))))
   `(org-level-6 ((t (:foreground ,36-symbols-green+2))))
   `(org-level-7 ((t (:foreground ,36-symbols-red-4))))
   `(org-level-8 ((t (:foreground ,36-symbols-blue-4))))
   `(org-link ((t (:foreground ,36-symbols-yellow-2 :underline t))))
   `(org-scheduled ((t (:foreground ,36-symbols-green+4))))
   `(org-scheduled-previously ((t (:foreground ,36-symbols-red))))
   `(org-scheduled-today ((t (:foreground ,36-symbols-blue+1))))
   `(org-sexp-date ((t (:foreground ,36-symbols-blue+1 :underline t))))
   `(org-special-keyword ((t (:inherit font-lock-comment-face))))
   `(org-table ((t (:foreground ,36-symbols-green+2))))
   `(org-tag ((t (:foreground ,36-symbols-orange :height ,36-symbols-height-minus-1 :bold t :weight bold))))
   `(org-time-grid ((t (:foreground ,36-symbols-orange))))
   `(org-todo ((t (:bold t :foreground ,36-symbols-red :weight bold))))
   `(org-agenda-dimmed-todo-face ((t (:foreground ,36-symbols-red+1))))
   `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
   `(org-warning ((t (:bold t :foreground ,36-symbols-red :weight bold :underline nil))))
   `(org-column ((t (:background ,36-symbols-bg))))
   `(org-column-title ((t (:background ,36-symbols-bg :underline t :weight bold))))
   `(org-mode-line-clock ((t (:foreground ,36-symbols-fg :background ,36-symbols-bg-1))))
   `(org-mode-line-clock-overrun ((t (:foreground ,36-symbols-bg :background ,36-symbols-red-1))))
   `(org-ellipsis ((t (:foreground ,36-symbols-yellow-1 :underline t))))
   `(org-footnote ((t (:foreground ,36-symbols-cyan :underline t))))
   `(org-habit-clear-face ((t (:background ,36-symbols-blue+1 :foreground ,36-symbols-blue-5))))
   `(org-habit-clear-future-face ((t (:background ,36-symbols-blue+1))))
   `(org-habit-ready-face ((t (:background ,36-symbols-green+4 :foreground ,36-symbols-green-1))))
   `(org-habit-ready-future-face ((t (:background ,36-symbols-green+4))))
   `(org-habit-alert-face ((t (:background ,36-symbols-yellow :foreground ,36-symbols-yellow-2))))
   `(org-habit-alert-future-face ((t (:background ,36-symbols-yellow-2))))
   `(org-habit-overdue-face ((t (:background ,36-symbols-red-4 :foreground ,36-symbols-red+1))))
   `(org-habit-overdue-future-face ((t (:background ,36-symbols-red+1))))
   `(org-document-title ((t (:foreground ,36-symbols-blue+1
                                         :weight bold
                                         :height ,36-symbols-height-plus-5))))
   `(org-document-info ((t (:foreground ,36-symbols-blue
                                        :weight bold
                                        :height ,36-symbols-height-plus-3))))
   `(org-document-info-keyword ((t (:foreground ,36-symbols-blue-2))))
   `(org-drawer ((t (:foreground ,36-symbols-cyan))))
;;;;; outline
   `(outline-1 ((t (:foreground ,36-symbols-orange))))
   `(outline-2 ((t (:foreground ,36-symbols-green+4))))
   `(outline-3 ((t (:foreground ,36-symbols-blue-1))))
   `(outline-4 ((t (:foreground ,36-symbols-yellow-2))))
   `(outline-5 ((t (:foreground ,36-symbols-cyan))))
   `(outline-6 ((t (:foreground ,36-symbols-green+2))))
   `(outline-7 ((t (:foreground ,36-symbols-red-4))))
   `(outline-8 ((t (:foreground ,36-symbols-blue-4))))
;;;;; p4
   `(p4-depot-added-face ((t :inherit diff-added)))
   `(p4-depot-branch-op-face ((t :inherit diff-changed)))
   `(p4-depot-deleted-face ((t :inherit diff-removed)))
   `(p4-depot-unmapped-face ((t :inherit diff-changed)))
   `(p4-diff-change-face ((t :inherit diff-changed)))
   `(p4-diff-del-face ((t :inherit diff-removed)))
   `(p4-diff-file-face ((t :inherit diff-file-header)))
   `(p4-diff-head-face ((t :inherit diff-header)))
   `(p4-diff-ins-face ((t :inherit diff-added)))
;;;;; perspective
   `(persp-selected-face ((t (:foreground ,36-symbols-yellow-2 :inherit mode-line))))
;;;;; powerline
   `(powerline-active1 ((t (:background ,36-symbols-bg-05 :inherit mode-line))))
   `(powerline-active2 ((t (:background ,36-symbols-bg+2 :inherit mode-line))))
   `(powerline-inactive1 ((t (:background ,36-symbols-bg+1 :inherit mode-line-inactive))))
   `(powerline-inactive2 ((t (:background ,36-symbols-bg+3 :inherit mode-line-inactive))))
;;;;; proofgeneral
   `(proof-active-area-face ((t (:underline t))))
   `(proof-boring-face ((t (:foreground ,36-symbols-fg :background ,36-symbols-bg+2))))
   `(proof-command-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-debug-message-face ((t (:inherit proof-boring-face))))
   `(proof-declaration-name-face ((t (:inherit font-lock-keyword-face :foreground nil))))
   `(proof-eager-annotation-face ((t (:foreground ,36-symbols-bg :background ,36-symbols-orange))))
   `(proof-error-face ((t (:foreground ,36-symbols-fg :background ,36-symbols-red-4))))
   `(proof-highlight-dependency-face ((t (:foreground ,36-symbols-bg :background ,36-symbols-yellow-1))))
   `(proof-highlight-dependent-face ((t (:foreground ,36-symbols-bg :background ,36-symbols-orange))))
   `(proof-locked-face ((t (:background ,36-symbols-blue-5))))
   `(proof-mouse-highlight-face ((t (:foreground ,36-symbols-bg :background ,36-symbols-orange))))
   `(proof-queue-face ((t (:background ,36-symbols-red-4))))
   `(proof-region-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-script-highlight-error-face ((t (:background ,36-symbols-red-2))))
   `(proof-tacticals-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,36-symbols-bg))))
   `(proof-tactics-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,36-symbols-bg))))
   `(proof-warning-face ((t (:foreground ,36-symbols-bg :background ,36-symbols-yellow-1))))
;;;;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,36-symbols-fg))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,36-symbols-green+4))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,36-symbols-yellow-2))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,36-symbols-cyan))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,36-symbols-green+2))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,36-symbols-blue+1))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,36-symbols-yellow-1))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,36-symbols-green+1))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,36-symbols-blue-2))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,36-symbols-orange))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,36-symbols-green))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,36-symbols-blue-5))))
;;;;; rainbow-blocks
   `(rainbow-blocks-depth-1-face ((,class (:foreground ,36-symbols-fg))))
   `(rainbow-blocks-depth-2-face ((,class (:foreground ,36-symbols-green+4))))
   `(rainbow-blocks-depth-3-face ((,class (:foreground ,36-symbols-yellow-2))))
   `(rainbow-blocks-depth-4-face ((,class (:foreground ,36-symbols-cyan))))
   `(rainbow-blocks-depth-5-face ((,class (:foreground ,36-symbols-green+2))))
   `(rainbow-blocks-depth-6-face ((,class (:foreground ,36-symbols-blue+1))))
   `(rainbow-blocks-depth-7-face ((,class (:foreground ,36-symbols-yellow-1))))
   `(rainbow-blocks-depth-8-face ((,class (:foreground ,36-symbols-green+1))))
   `(rainbow-blocks-depth-9-face ((,class (:foreground ,36-symbols-blue-2))))
   `(rainbow-blocks-unmatched-face ((,class (:foreground ,36-symbols-red-2))))
;;;;; rcirc
   `(rcirc-my-nick ((t (:foreground ,36-symbols-blue))))
   `(rcirc-other-nick ((t (:foreground ,36-symbols-orange))))
   `(rcirc-bright-nick ((t (:foreground ,36-symbols-blue+1))))
   `(rcirc-dim-nick ((t (:foreground ,36-symbols-blue-2))))
   `(rcirc-server ((t (:foreground ,36-symbols-green))))
   `(rcirc-server-prefix ((t (:foreground ,36-symbols-green+1))))
   `(rcirc-timestamp ((t (:foreground ,36-symbols-green+2))))
   `(rcirc-nick-in-message ((t (:foreground ,36-symbols-yellow))))
   `(rcirc-nick-in-message-full-line ((t (:bold t))))
   `(rcirc-prompt ((t (:foreground ,36-symbols-yellow :bold t))))
   `(rcirc-track-nick ((t (:inverse-video t))))
   `(rcirc-track-keyword ((t (:bold t))))
   `(rcirc-url ((t (:bold t))))
   `(rcirc-keyword ((t (:foreground ,36-symbols-yellow :bold t))))
;;;;; rpm-mode
   `(rpm-spec-dir-face ((t (:foreground ,36-symbols-green))))
   `(rpm-spec-doc-face ((t (:foreground ,36-symbols-green))))
   `(rpm-spec-ghost-face ((t (:foreground ,36-symbols-red))))
   `(rpm-spec-macro-face ((t (:foreground ,36-symbols-yellow))))
   `(rpm-spec-obsolete-tag-face ((t (:foreground ,36-symbols-red))))
   `(rpm-spec-package-face ((t (:foreground ,36-symbols-red))))
   `(rpm-spec-section-face ((t (:foreground ,36-symbols-yellow))))
   `(rpm-spec-tag-face ((t (:foreground ,36-symbols-blue))))
   `(rpm-spec-var-face ((t (:foreground ,36-symbols-red))))
;;;;; rst-mode
   `(rst-level-1-face ((t (:foreground ,36-symbols-orange))))
   `(rst-level-2-face ((t (:foreground ,36-symbols-green+1))))
   `(rst-level-3-face ((t (:foreground ,36-symbols-blue-1))))
   `(rst-level-4-face ((t (:foreground ,36-symbols-yellow-2))))
   `(rst-level-5-face ((t (:foreground ,36-symbols-cyan))))
   `(rst-level-6-face ((t (:foreground ,36-symbols-green-1))))
;;;;; sh-mode
   `(sh-heredoc     ((t (:foreground ,36-symbols-yellow :bold t))))
   `(sh-quoted-exec ((t (:foreground ,36-symbols-red))))
;;;;; show-paren
   `(show-paren-mismatch ((t (:foreground ,36-symbols-red+1 :background ,36-symbols-bg+3 :weight bold))))
   `(show-paren-match ((t (:background ,36-symbols-bg+3 :weight bold))))
;;;;; smartparens
   `(sp-show-pair-mismatch-face ((t (:foreground ,36-symbols-red+1 :background ,36-symbols-bg+3 :weight bold))))
   `(sp-show-pair-match-face ((t (:background ,36-symbols-bg+3 :weight bold))))
;;;;; sml-mode-line
   '(sml-modeline-end-face ((t :inherit default :width condensed)))
;;;;; SLIME
   `(slime-repl-output-face ((t (:foreground ,36-symbols-red))))
   `(slime-repl-inputed-output-face ((t (:foreground ,36-symbols-green))))
   `(slime-error-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,36-symbols-red)))
      (t
       (:underline ,36-symbols-red))))
   `(slime-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,36-symbols-orange)))
      (t
       (:underline ,36-symbols-orange))))
   `(slime-style-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,36-symbols-yellow)))
      (t
       (:underline ,36-symbols-yellow))))
   `(slime-note-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,36-symbols-green)))
      (t
       (:underline ,36-symbols-green))))
   `(slime-highlight-face ((t (:inherit highlight))))
;;;;; speedbar
   `(speedbar-button-face ((t (:foreground ,36-symbols-green+2))))
   `(speedbar-directory-face ((t (:foreground ,36-symbols-cyan))))
   `(speedbar-file-face ((t (:foreground ,36-symbols-fg))))
   `(speedbar-highlight-face ((t (:foreground ,36-symbols-bg :background ,36-symbols-green+2))))
   `(speedbar-selected-face ((t (:foreground ,36-symbols-red))))
   `(speedbar-separator-face ((t (:foreground ,36-symbols-bg :background ,36-symbols-blue-1))))
   `(speedbar-tag-face ((t (:foreground ,36-symbols-yellow))))
;;;;; tabbar
   `(tabbar-default ((t (:foreground ,36-symbols-fg-1 :background ,36-symbols-bg :box nil))))
   `(tabbar-button ((t (:foreground ,36-symbols-fg-1 :background ,36-symbols-bg :box nil))))
   `(tabbar-selected ((t (:foreground ,36-symbols-fg-1 :background ,36-symbols-bg :box nil))))
   `(tabbar-unselected ((t (:foreground ,36-symbols-bg :background ,36-symbols-fg-1 :box nil))))
   `(tabbar-modified ((t (:foreground ,36-symbols-blue-5 :background ,36-symbols-bg :box nil))))
   `(tabbar-selected-modified ((t (:foreground ,36-symbols-green+4 :background ,36-symbols-bg :slant normal :box nil))))
   `(tabbar-unselected-modified ((t (:foreground ,36-symbols-green+4 :background ,36-symbols-fg :weight extra-bold :box nil))))
;;;;; term
   `(term-color-black ((t (:foreground ,36-symbols-bg
                                       :background ,36-symbols-bg-1))))
   `(term-color-red ((t (:foreground ,36-symbols-red-2
                                     :background ,36-symbols-red-4))))
   `(term-color-green ((t (:foreground ,36-symbols-green
                                       :background ,36-symbols-green+2))))
   `(term-color-yellow ((t (:foreground ,36-symbols-orange
                                        :background ,36-symbols-yellow))))
   `(term-color-blue ((t (:foreground ,36-symbols-blue-1
                                      :background ,36-symbols-blue-4))))
   `(term-color-magenta ((t (:foreground ,36-symbols-magenta
                                         :background ,36-symbols-red))))
   `(term-color-cyan ((t (:foreground ,36-symbols-cyan
                                      :background ,36-symbols-blue))))
   `(term-color-white ((t (:foreground ,36-symbols-fg
                                       :background ,36-symbols-fg-1))))
   '(term-default-fg-color ((t (:inherit term-color-white))))
   '(term-default-bg-color ((t (:inherit term-color-black))))
;;;;; undo-tree
   `(undo-tree-visualizer-active-branch-face ((t (:foreground ,36-symbols-fg+1 :weight bold))))
   `(undo-tree-visualizer-current-face ((t (:foreground ,36-symbols-red-1 :weight bold))))
   `(undo-tree-visualizer-default-face ((t (:foreground ,36-symbols-fg))))
   `(undo-tree-visualizer-register-face ((t (:foreground ,36-symbols-yellow))))
   `(undo-tree-visualizer-unmodified-face ((t (:foreground ,36-symbols-cyan))))
;;;;; volatile-highlights
   `(vhl/default-face ((t (:background ,36-symbols-bg-05))))
;;;;; web-mode
   `(web-mode-builtin-face ((t (:inherit ,font-lock-builtin-face))))
   `(web-mode-comment-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-constant-face ((t (:inherit ,font-lock-constant-face))))
   `(web-mode-css-at-rule-face ((t (:foreground ,36-symbols-orange ))))
   `(web-mode-css-prop-face ((t (:foreground ,36-symbols-orange))))
   `(web-mode-css-pseudo-class-face ((t (:foreground ,36-symbols-green+3 :weight bold))))
   `(web-mode-css-rule-face ((t (:foreground ,36-symbols-blue))))
   `(web-mode-doctype-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-folded-face ((t (:underline t))))
   `(web-mode-function-name-face ((t (:foreground ,36-symbols-blue))))
   `(web-mode-html-attr-name-face ((t (:foreground ,36-symbols-orange))))
   `(web-mode-html-attr-value-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-html-tag-face ((t (:foreground ,36-symbols-cyan))))
   `(web-mode-keyword-face ((t (:inherit ,font-lock-keyword-face))))
   `(web-mode-preprocessor-face ((t (:inherit ,font-lock-preprocessor-face))))
   `(web-mode-string-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-type-face ((t (:inherit ,font-lock-type-face))))
   `(web-mode-variable-name-face ((t (:inherit ,font-lock-variable-name-face))))
   `(web-mode-server-background-face ((t (:background ,36-symbols-bg))))
   `(web-mode-server-comment-face ((t (:inherit web-mode-comment-face))))
   `(web-mode-server-string-face ((t (:inherit web-mode-string-face))))
   `(web-mode-symbol-face ((t (:inherit font-lock-constant-face))))
   `(web-mode-warning-face ((t (:inherit font-lock-warning-face))))
   `(web-mode-whitespaces-face ((t (:background ,36-symbols-red))))
;;;;; whitespace-mode
   `(whitespace-space ((t (:background ,36-symbols-bg+1 :foreground ,36-symbols-bg+1))))
   `(whitespace-hspace ((t (:background ,36-symbols-bg+1 :foreground ,36-symbols-bg+1))))
   `(whitespace-tab ((t (:background ,36-symbols-red-1))))
   `(whitespace-newline ((t (:foreground ,36-symbols-bg+1))))
   `(whitespace-trailing ((t (:background ,36-symbols-red))))
   `(whitespace-line ((t (:background ,36-symbols-bg :foreground ,36-symbols-magenta))))
   `(whitespace-space-before-tab ((t (:background ,36-symbols-orange :foreground ,36-symbols-orange))))
   `(whitespace-indentation ((t (:background ,36-symbols-yellow :foreground ,36-symbols-red))))
   `(whitespace-empty ((t (:background ,36-symbols-yellow))))
   `(whitespace-space-after-tab ((t (:background ,36-symbols-yellow :foreground ,36-symbols-red))))
;;;;; wanderlust
   `(wl-highlight-folder-few-face ((t (:foreground ,36-symbols-red-2))))
   `(wl-highlight-folder-many-face ((t (:foreground ,36-symbols-red-1))))
   `(wl-highlight-folder-path-face ((t (:foreground ,36-symbols-orange))))
   `(wl-highlight-folder-unread-face ((t (:foreground ,36-symbols-blue))))
   `(wl-highlight-folder-zero-face ((t (:foreground ,36-symbols-fg))))
   `(wl-highlight-folder-unknown-face ((t (:foreground ,36-symbols-blue))))
   `(wl-highlight-message-citation-header ((t (:foreground ,36-symbols-red-1))))
   `(wl-highlight-message-cited-text-1 ((t (:foreground ,36-symbols-red))))
   `(wl-highlight-message-cited-text-2 ((t (:foreground ,36-symbols-green+2))))
   `(wl-highlight-message-cited-text-3 ((t (:foreground ,36-symbols-blue))))
   `(wl-highlight-message-cited-text-4 ((t (:foreground ,36-symbols-blue+1))))
   `(wl-highlight-message-header-contents-face ((t (:foreground ,36-symbols-green))))
   `(wl-highlight-message-headers-face ((t (:foreground ,36-symbols-red+1))))
   `(wl-highlight-message-important-header-contents ((t (:foreground ,36-symbols-green+2))))
   `(wl-highlight-message-header-contents ((t (:foreground ,36-symbols-green+1))))
   `(wl-highlight-message-important-header-contents2 ((t (:foreground ,36-symbols-green+2))))
   `(wl-highlight-message-signature ((t (:foreground ,36-symbols-green))))
   `(wl-highlight-message-unimportant-header-contents ((t (:foreground ,36-symbols-fg))))
   `(wl-highlight-summary-answered-face ((t (:foreground ,36-symbols-blue))))
   `(wl-highlight-summary-disposed-face ((t (:foreground ,36-symbols-fg
                                                         :slant italic))))
   `(wl-highlight-summary-new-face ((t (:foreground ,36-symbols-blue))))
   `(wl-highlight-summary-normal-face ((t (:foreground ,36-symbols-fg))))
   `(wl-highlight-summary-thread-top-face ((t (:foreground ,36-symbols-yellow))))
   `(wl-highlight-thread-indent-face ((t (:foreground ,36-symbols-magenta))))
   `(wl-highlight-summary-refiled-face ((t (:foreground ,36-symbols-fg))))
   `(wl-highlight-summary-displaying-face ((t (:underline t :weight bold))))
;;;;; which-func-mode
   `(which-func ((t (:foreground ,36-symbols-green+4))))
;;;;; yascroll
   `(yascroll:thumb-text-area ((t (:background ,36-symbols-bg-1))))
   `(yascroll:thumb-fringe ((t (:background ,36-symbols-bg-1 :foreground ,36-symbols-bg-1))))
   ))

;;; Theme Variables
(36-symbols-with-color-variables
  (custom-theme-set-variables
   '36-symbols
;;;;; ansi-color
   `(ansi-color-names-vector [,36-symbols-bg ,36-symbols-red ,36-symbols-green ,36-symbols-yellow
                                          ,36-symbols-blue ,36-symbols-magenta ,36-symbols-cyan ,36-symbols-fg])
;;;;; fill-column-indicator
   `(fci-rule-color ,36-symbols-bg-05)
;;;;; vc-annotate
   `(vc-annotate-color-map
     '(( 20. . ,36-symbols-red-1)
       ( 40. . ,36-symbols-red)
       ( 60. . ,36-symbols-orange)
       ( 80. . ,36-symbols-yellow-2)
       (100. . ,36-symbols-yellow-1)
       (120. . ,36-symbols-yellow)
       (140. . ,36-symbols-green-1)
       (160. . ,36-symbols-green)
       (180. . ,36-symbols-green+1)
       (200. . ,36-symbols-green+2)
       (220. . ,36-symbols-green+3)
       (240. . ,36-symbols-green+4)
       (260. . ,36-symbols-cyan)
       (280. . ,36-symbols-blue-2)
       (300. . ,36-symbols-blue-1)
       (320. . ,36-symbols-blue)
       (340. . ,36-symbols-blue+1)
       (360. . ,36-symbols-magenta)))
   `(vc-annotate-very-old-color ,36-symbols-magenta)
   `(vc-annotate-background ,36-symbols-bg-1)
   ))

;;; Rainbow Support

(declare-function rainbow-mode 'rainbow-mode)
(declare-function rainbow-colorize-by-assoc 'rainbow-mode)

(defvar 36-symbols-add-font-lock-keywords nil
  "Whether to add font-lock keywords for 36-symbols color names.
In buffers visiting library `36-symbols-theme.el' the 36-symbols
specific keywords are always added.  In all other Emacs-Lisp
buffers this variable controls whether this should be done.
This requires library `rainbow-mode'.")

(defvar 36-symbols-colors-font-lock-keywords nil)

;; (defadvice rainbow-turn-on (after 36-symbols activate)
;;   "Maybe also add font-lock keywords for 36-symbols colors."
;;   (when (and (derived-mode-p 'emacs-lisp-mode)
;;              (or 36-symbols-add-font-lock-keywords
;;                  (equal (file-name-nondirectory (buffer-file-name))
;;                         "36-symbols-theme.el")))
;;     (unless 36-symbols-colors-font-lock-keywords
;;       (setq 36-symbols-colors-font-lock-keywords
;;             `((,(regexp-opt (mapcar 'car 36-symbols-colors-alist) 'words)
;;                (0 (rainbow-colorize-by-assoc 36-symbols-colors-alist))))))
;;     (font-lock-add-keywords nil 36-symbols-colors-font-lock-keywords)))

;; (defadvice rainbow-turn-off (after 36-symbols activate)
;;   "Also remove font-lock keywords for 36-symbols colors."
;;   (font-lock-remove-keywords nil 36-symbols-colors-font-lock-keywords))

;;; Footer

;;;###autoload
(and load-file-name
   (boundp 'custom-theme-load-path)
   (add-to-list 'custom-theme-load-path
                (file-name-as-directory
                 (file-name-directory load-file-name))))

(provide-theme '36-symbols)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (require 'rainbow-mode nil t) (rainbow-mode 1))
;; End:


;;; 36-symbols-theme.el ends here
