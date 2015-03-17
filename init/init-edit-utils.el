;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Author: Anton Strilchuk <ype@env.sh>                           ;;;
;;; URL: http://ype.env.sh                                         ;;;
;;; Created: 16-06-2014                                            ;;;
;;; Last-Updated: 16-03-2015                                       ;;;
;;;  Update #: 157                                                 ;;;
;;;   By: Anton Strilchuk <anton@env.sh>                           ;;;
;;;                                                                ;;;
;;; Filename: init-edit-utils                                      ;;;
;;; Version:                                                       ;;;
;;; Description:                                                   ;;;
;;;                                                                ;;;
;;; Package Requires: ()                                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Global Key Changes
(define-key ctrl-apos (kbd "C-s") 'save-buffer)

(require-package 'unfill)
(require-package 'whole-line-or-region)

(when (fboundp 'electric-pair-mode)
  (electric-pair-mode))

(setq-default blink-cursor-delay 0
              blink-cursor-interval 0.4
              bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory)
              buffers-menu-max-size 30
              case-fold-search t
              tab-width 2
              compilation-scroll-output t
              grep-highlight-matches t
              grep-scroll-output t
              indent-tabs-mode nil
              mouse-yank-at-point t
              save-interprogram-paste-before-kill t
              scroll-preserve-screen-position 'always
              set-mark-command-repeat-pop t
              show-trailing-whitespace t
              tooltip-delay 1.5
              truncate-lines nil
              truncate-partial-width-windows nil)

;; Set Backtab
(define-key function-key-map [S-tab] [backtab])

(transient-mark-mode t)

;; Prev Buffer
(global-set-key (kbd "H-[") (lambda () (interactive) (previous-buffer)))
(global-set-key (kbd "H-]") (lambda () (interactive) (next-buffer)))
;;(global-set-key (kbd "H-]") (lambda () (interactive) (switch-to-buffer (buffer-name (last-buffer)))))

;;Show Marks
(require-package 'show-marks)
(global-set-key (kbd "C-1") 'show-marks)
(global-set-key (kbd "C-2") 'forward-mark)
(global-set-key (kbd "C-3") 'backward-mark)

(global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(require 'find-dired)
(setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))

;;; Whitespace
(defun sanityinc/no-trailing-whitespace ()
  "Turn off display of trailing whitespace in this buffer."
  (setq show-trailing-whitespace nil))

;; But don't show trailing whitespace in SQLi, inf-ruby etc.
(dolist (hook '(special-mode-hook
                eww-mode-hook
                term-mode-hook
                comint-mode-hook
                compilation-mode-hook
                twittering-mode-hook
                minibuffer-setup-hook))
  (add-hook hook #'sanityinc/no-trailing-whitespace))


(require-package 'whitespace-cleanup-mode)
(global-whitespace-cleanup-mode t)
(diminish 'whitespace-cleanup-mode " ⌴")

(global-set-key (kbd "RET") 'newline-and-indent)
(defun sanityinc/newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(global-set-key (kbd "S-<return>") 'sanityinc/newline-at-end-of-line)

(require-package 'aggressive-indent)
;;(global-aggressive-indent-mode 1)

(after-load 'subword
  (diminish 'subword-mode))

(when (fboundp 'global-prettify-symbols-mode)
  (global-prettify-symbols-mode))

;;Undo Tree
;;http://ergoemacs.org/emacs/emacs_best_redo_mode.html
(require-package 'undo-tree)
(global-undo-tree-mode 1)
(diminish 'undo-tree-mode)

(require-package 'highlight-symbol)
(dolist (hook '(prog-mode-hook html-mode-hook))
  (add-hook hook 'highlight-symbol-mode)
  (add-hook hook 'highlight-symbol-nav-mode))
(eval-after-load 'highlight-symbol
  '(diminish 'highlight-symbol-mode))

;;Rainbow Delimiter
(require-package 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;Rainbow Blocks
(require-package 'rainbow-blocks)
(after-load 'rainbow-blocks
  (diminish 'rainbow-blocks-mode))

(add-hook 'prog-mode-hook 'rainbow-blocks-mode)
(global-set-key (kbd "H-d") 'rainbow-blocks-mode)

(autoload 'zap-up-to-char "misc" "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key (kbd "A-z") 'zap-up-to-char)

(require-package 'ace-jump-zap)
(global-set-key (kbd "A-Z") 'ace-jump-zap-up-to-char)

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;;----------------------------------------------------------------------------
;; Expand region
;;----------------------------------------------------------------------------
(require-package 'expand-region)
(global-set-key (kbd "C-\-") 'er/expand-region)

(cua-selection-mode t)

;;; Ace Jump
;; C-c SPC => ace-jump-word-mode
;; C-u C-c SPC => ace-jump-char-mode
;; C-u C-u C-c SPC => ace-jump-line-mode
(require-package 'ace-jump-mode)
(define-key ctrl-apos (kbd "C") 'ace-jump-char-mode)
(define-key ctrl-apos (kbd "L") 'ace-jump-char-mode)

(global-set-key (kbd "C-c J") (lambda () (interactive) (join-line 1)))
(global-set-key (kbd "C-.") 'set-mark-command)
(global-set-key (kbd "C-x C-.") 'pop-global-mark)

(defun duplicate-region (beg end)
  "Insert a copy of the current region after the region."
  (interactive "r")
  (save-excursion
    (goto-char end)
    (insert (buffer-substring beg end))))

(defun duplicate-line-or-region (prefix)
  "Duplicate either the current line or any current region."
  (interactive "*p")
  (whole-line-or-region-call-with-region 'duplicate-region prefix t))

(global-set-key (kbd "C-c C-p") 'duplicate-line-or-region)

;; Force Use Emacs Movement
(global-unset-key [M-left])
(global-unset-key [M-right])

;;----------------------------------------------------------------------------
;; Shift lines up and down with M-up and M-down. When paredit is enabled,
;; it will use those keybindings. For this reason, you might prefer to
;; use M-S-up and M-S-down, which will work even in lisp modes.
;;----------------------------------------------------------------------------
(require-package 'move-dup)
(global-set-key [M-up] 'md/move-lines-up)
(global-set-key [M-down] 'md/move-lines-down)
(global-set-key [M-S-up] 'md/move-lines-up)
(global-set-key [M-S-down] 'md/move-lines-down)

(defun kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(global-set-key (kbd "C-M-<backspace>") 'kill-back-to-indentation)

;;----------------------------------------------------------------------------
;; Fill column indicator
;;----------------------------------------------------------------------------
(when (eval-when-compile (> emacs-major-version 23))
  (require-package 'fill-column-indicator)
  (defun sanityinc/prog-mode-fci-settings ()
    (turn-on-fci-mode)
    (when show-trailing-whitespace
      (set (make-local-variable 'whitespace-style) '(face trailing))
      (whitespace-mode 1)))

  (defun sanityinc/fci-enabled-p ()
    (and (boundp 'fci-mode) fci-mode))

  (defvar sanityinc/fci-mode-suppressed nil)
  (defadvice popup-create (before suppress-fci-mode activate)
    "Suspend fci-mode while popups are visible"
    (let ((fci-enabled (sanityinc/fci-enabled-p)))
      (when fci-enabled
        (set (make-local-variable 'sanityinc/fci-mode-suppressed) fci-enabled)
        (turn-off-fci-mode))))
  (defadvice popup-delete (after restore-fci-mode activate)
    "Restore fci-mode when all popups have closed"
    (when (and sanityinc/fci-mode-suppressed
               (null popup-instances))
      (setq sanityinc/fci-mode-suppressed nil)
      (turn-on-fci-mode)))

  ;; Regenerate fci-mode line images after switching themes
  (defadvice enable-theme (after recompute-fci-face activate)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (sanityinc/fci-enabled-p)
          (turn-on-fci-mode))))))

(defun backward-up-sexp (arg)
  "Jump up to the start of the ARG'th enclosing sexp."
  (interactive "p")
  (let ((ppss (syntax-ppss)))
    (cond ((elt ppss 3)
           (goto-char (elt ppss 8))
           (backward-up-sexp (1- arg)))
          ((backward-up-list arg)))))

(global-set-key [remap backward-up-list] 'backward-up-sexp) ; C-M-u, C-M-up

;;----------------------------------------------------------------------------
;; Cut/copy the current line if no region is active
;;----------------------------------------------------------------------------
(whole-line-or-region-mode t)
(diminish 'whole-line-or-region-mode)
(make-variable-buffer-local 'whole-line-or-region-mode)

(defun suspend-mode-during-cua-rect-selection (mode-name)
  "Add an advice to suspend `MODE-NAME' while selecting a CUA rectangle."
  (let ((flagvar (intern (format "%s-was-active-before-cua-rectangle" mode-name)))
        (advice-name (intern (format "suspend-%s" mode-name))))
    (eval-after-load 'cua-rect
      `(progn
         (defvar ,flagvar nil)
         (make-variable-buffer-local ',flagvar)
         (defadvice cua--activate-rectangle (after ,advice-name activate)
           (setq ,flagvar (and (boundp ',mode-name) ,mode-name))
           (when ,flagvar
             (,mode-name 0)))
         (defadvice cua--deactivate-rectangle (after ,advice-name activate)
           (when ,flagvar
             (,mode-name 1)))))))

(suspend-mode-during-cua-rect-selection 'whole-line-or-region-mode)

(global-set-key (kbd "M-c") 'cua-copy-region)
(global-set-key (kbd "M-v") 'cua-paste)

(require-package 'browse-kill-ring)
(global-set-key (kbd "<f12> DEL") 'browse-kill-ring)

(defun sanityinc/open-line-with-reindent (n)
  "A version of `open-line' which reindents the start and end positions.
If there is a fill prefix and/or a `left-margin', insert them
on the new line if the line would have been blank.
With arg N, insert N newlines."
  (interactive "*p")
  (let* ((do-fill-prefix (and fill-prefix (bolp)))
         (do-left-margin (and (bolp) (> (current-left-margin) 0)))
         (loc (point-marker))
         ;; Don't expand an abbrev before point.
         (abbrev-mode nil))
    (delete-horizontal-space t)
    (newline n)
    (indent-according-to-mode)
    (when (eolp)
      (delete-horizontal-space t))
    (goto-char loc)
    (while (> n 0)
      (cond ((bolp)
             (if do-left-margin (indent-to (current-left-margin)))
             (if do-fill-prefix (insert-and-inherit fill-prefix))))
      (forward-line 1)
      (setq n (1- n)))
    (goto-char loc)
    (end-of-line)
    (indent-according-to-mode)))

(global-set-key (kbd "C-o") 'sanityinc/open-line-with-reindent)

;; Move to beginning of line
;; Source: [[http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/][Smarter Beginning of Line]]

(defun ype:smart-move-beginning-of-line (arg)
  (interactive "^p")
  (setq arg (or arg 1))
  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to smarter-move-beginning-of-line
(oVr-set-key "C-a" 'ype:smart-move-beginning-of-line)

;;----------------------------------------------------------------------------
;; Random line sorting
;;----------------------------------------------------------------------------
(defun sort-lines-random (beg end)
  "Sort lines in region randomly."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr nil 'forward-line 'end-of-line nil nil
                   (lambda (s1 s2) (eq (random 2) 0)))))))

(when (executable-find "ag")
  (require-package 'ag)
  (require-package 'wgrep-ag)
  (setq-default ag-highlight-search t)
  (global-set-key (kbd "H-x q") 'ag-project)
  (global-set-key (kbd "H-z") 'projectile-ag))

(global-set-key (kbd "H-+") 'enlarge-window)
(global-set-key (kbd "H-_") 'shrink-window)

(require-package 'highlight-escape-sequences)
(hes-mode)

(require-package 'guide-key)
(setq guide-key/guide-key-sequence '("C-x" "C-c" "C-h" "C-\'" "H-x" "M-g" "<escape>" "C-\," "H-a")
      guide-key/recursive-key-sequence-flag t
      guide-key/idle-delay 1.0
      guide-key/popup-window-position 'right)
(setq guide-key/highlight-command-regexp
      '("rectangle" ("register" . font-lock-type-face)
        ("undo-tree" . "HotPink1")
        ("org-pomodoro" . "cyan")
        ("org-capture" . "DeepPink1")
        ("pop" . "DeepSkyBlue1")
        ))
(remove-hook 'guide-key-mode 'rainbow-blocks-mode)
(guide-key-mode 1)
(setq guide-key-tip/toggle-enable nil)

(diminish 'guide-key-mode)

(require-package 'rebox2)
(setq rebox-style-loop '(17 27 16 26 21))
(global-set-key [(meta q)] 'rebox-dwim)
(global-set-key [(hyper r)] 'rebox-cycle)

;;,--------------------------------------
;;| Word Count Mode
;;| https://github.com/bnbeckwith/wc-mode
;;`--------------------------------------
(require-package 'wc-mode)
;; Suggested setting
(global-set-key "\C-cw" 'wc-mode)

;;,----------------------------------------------
;;|  Multiple Cursors
;;|  Mark a bunch of stuff just like in sublime
;;| =============================================
;;|  Usage:
;;|  C-c m (vector ?key)
;;|  e.g. mark-all-words-like-this => C-c m a
;;`----------------------------------------------
(require-package 'multiple-cursors)
(define-prefix-command 'ype:multiple-cursors-map)                                     ; Key sequence
(define-key ype:multiple-cursors-map (vector ?r) 'set-rectangular-region-anchor)      ; C-c m r
(define-key ype:multiple-cursors-map (vector ?d) 'mc/edit-lines)                      ; C-c m d
(define-key ype:multiple-cursors-map (vector ?e) 'mc/edit-ends-of-lines)              ; C-c m e
(define-key ype:multiple-cursors-map (vector ?f) 'mc/edit-beginnings-of-lines)        ; C-c m f
(define-key ype:multiple-cursors-map (vector ?a) 'mc/mark-all-like-this)              ; C-c m a
(define-key ype:multiple-cursors-map (vector ?b) 'mc/mark-all-words-like-this)        ; C-c m b
(define-key ype:multiple-cursors-map (vector ?k) 'mc/mark-previous-like-this)         ; C-c m k
(define-key ype:multiple-cursors-map (vector ?j) 'mc/mark-next-like-this)             ; C-c m j
(define-key ype:multiple-cursors-map (vector ?n) 'mc/mark-next-symbol-like-this)      ; C-c m n
(define-key ype:multiple-cursors-map (vector ?m) 'mc/mark-previous-symbol-like-this)  ; C-c m m
(define-key ype:multiple-cursors-map (vector ?o) 'mc/mark-next-word-like-this)        ; C-c m o
(define-key ype:multiple-cursors-map (vector ?p) 'mc/mark-previous-word-like-this)    ; C-c m p
(define-key ype:multiple-cursors-map (vector ?q) 'mc/mark-all-like-this-in-defun)     ; C-c m q
(global-set-key [?\C-c ?m] 'ype:multiple-cursors-map)

;;,-----------------------------------------------------------------
;;|  Drag Stuff
;;| it possible to drag stuff (words, region, lines) around in Emacs
;;`-----------------------------------------------------------------
;; (require-package 'drag-stuff)
;; (drag-stuff-mode t)

;; No annoy emacs beep
(setq ring-bell-function #'ignore)

;;Delete to trash
(setq delete-by-moving-to-trash t)

;;Y for yes N for no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Quick Kill Buffer ESC-<return>
(global-set-key (kbd "M-<return>") 'kill-this-buffer)

;;Confirm Emacs Quit
(set-variable 'confirm-kill-emacs 'yes-or-no-p)

;;Root directory
(setq root-dir (file-name-directory
                (or (buffer-file-name) load-file-name)))

;;Load GTAGS for getting tags from source files
(setq load-path (cons "/usr/local/Cellar/global/6.2.9/share/gtags/" load-path))
(autoload 'gtags-mode "gtags" "" t)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)

;;Reveal Stuff in OSX Finder
(require-package 'reveal-in-finder)
(define-key ctrl-apos (kbd "r") 'reveal-in-finder)

;;,-----------------------
;;| Quick Conversion tools
;;`-----------------------

;; Seconds to Minutes
(defun m2s (mins)
  "Quick convert minutes to seconds"
  (* mins 60))

;; Seconds to Hour
(defun h2s (hours)
  "Quick convert hours to seconds"
  (* (* hours 60) 60))

;;,-------------------------------------------------------------------
;;| Hide Boring Buffers
;;|
;;| Buffers that are ephemeral and generally uninteresting to the user
;;| have names starting with a space, so that the list-buffers and
;;| buffer-menu commands don't mention them (but if such a buffer
;;| visits a file, it is mentioned). A name starting with space also
;;| initially disables recording undo information
;;`-------------------------------------------------------------------

;; (defun hide-boring-buffer ()
;;   "Rename the current buffer to begin with a space"
;;   (interactive)
;;   (unless (string-match-p "^ " (buffer-name))
;;     (rename-buffer (concat " " (buffer-name)))))

;;,-------------------------------------------------------------------
;;| Org-Link-Minor-Mode
;;|
;;| Emacs minor mode that enables org-mode style fontification and
;;| activation of bracket links in modes other than org-mode.
;;|
;;| Org-mode bracket links look like this:
;;|
;;| [[http://www.bbc.co.uk][BBC]]
;;| [[org-link-minor-mode]]
;;|
;;| With this mode enabled, the links will be made active so you can
;;| click on them and displayed so you can see only the description if
;;| present.
;;|
;;| Note that org-toggle-link-display will also work when this mode is
;;| enabled.
;;`-------------------------------------------------------------------

(el-get-bundle seanohalpin/org-link-minor-mode
  (require 'org-link-minor-mode))
(diminish 'org-link-minor-mode " ☌")
(defun ype/toggle-OLMM-1 () (interactive) (org-link-minor-mode 1))
(defun ype/toggle-OLMM-0 () (interactive) (org-link-minor-mode 0))
(global-set-key (kbd "A-x 1") 'ype/toggle-OLMM-1)
(global-set-key (kbd "A-x 2") 'ype/toggle-OLMM-0)
(add-hook 'prog-mode-hook 'org-link-minor-mode)

;;Linum Mode
(require 'linum)
(require-package 'linum-relative)
(global-linum-mode -1)

(defun ype:autohide-linum ()
  "Show linum-mode only when its really needed"
  (interactive)
  (defvar *linum-autohide-delay* 3)
  (linum-mode -1)
  (defvar *linum-autohide-timer* nil)
  (defun renew-linum-autohide-timer (seconds-to-show)
    (if (timerp *linum-autohide-timer*)
        (cancel-timer *linum-autohide-timer*))
    (setf *linum-autohide-timer*
          (run-with-timer
           seconds-to-show nil
           (lambda () (linum-mode -1)
             (setf *linum-autohide-timer* nil)))))
  (global-set-key
   [C-down]
   (lambda ()
     (interactive)
     (if linum-mode
         (forward-paragraph)
       (linum-mode 1))
     (renew-linum-autohide-timer 1)))
  (global-set-key
   [C-up]
   (lambda ()
     (interactive)
     (if linum-mode
         (backward-paragraph)
       (linum-mode 1))
     (renew-linum-autohide-timer 1)))
  (defun show-linum-goto-num ()
    "Shows Line Numbers for goto-line command"
    (interactive)
    (linum-mode 1)
    (let ((num (read-string "Goto line: ")))
      (goto-line (string-to-number num)))
    (renew-linum-autohide-timer 1))
  (global-unset-key (kbd "M-g g"))
  (global-set-key (kbd "M-g g") 'show-linum-goto-num))

(ype:autohide-linum)

;; Unfill Paragraph
(global-set-key (kbd "C-c u") 'unfill-paragraph)

;; Quick Keys
(define-prefix-command 'endless:toggle-map)
;; The manual recommends C-c for user keys, but I like using C-x for
;; global keys and using C-c for mode-specific keys.
(define-key ctl-x-map "t" 'endless:toggle-map)
(define-key endless:toggle-map "l" 'linum-mode)
(define-key endless:toggle-map "r" 'linum-relative-toggle)
(define-key endless:toggle-map "e" 'toggle-debug-on-error)
(define-key endless:toggle-map "F" 'auto-fill-mode)
(define-key endless:toggle-map "c" 'toggle-truncate-lines)
(define-key endless:toggle-map "q" 'toggle-debug-on-quit)
(define-key endless:toggle-map "d" 'read-only-mode)
(define-key endless:toggle-map "g" 'git-gutter+-toggle-fringe)
(define-key endless:toggle-map "t" 'endless/toggle-theme)
(define-key endless:toggle-map "f" 'flycheck-mode)
(define-key endless:toggle-map "a" 'aggressive-indent-mode)

;; FreqAccessFiles
;; C-x r j
(mapc (lambda (r) (set-register (car r) (cons 'file (cdr r))))
      '((?e . "~/.emacs.d/init.el")
        (?l . "/Volumes/ype/finances/ledgers/ledger-monthly.ledger")
        (?y . "~/Dev/AVSTSUP")
        (?d . "~/Dev")
        (?n . "~/Dev/OrgFiles/")
        (?v . "~/Dev/vendor")))

;; Ledger
(set-register ?l (cons 'file "/Volumes/ype/finances/ledgers/ledger-monthly.ledger"))

;; Reformat Buffer
(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil))
  (message "Buffer Reformatted"))
(global-set-key (kbd "\C-c r r") 'indent-buffer)

(defun camelCase-to_underscores (start end)
  "Convert any string matching something like aBc to a_bc"
  (interactive "r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char 1)
    (let ((case-fold-search nil))
      (while (search-forward-regexp "\\([a-z]\\)\\([A-Z]\\)\\([a-z]\\)" nil t)
        (replace-match (concat (match-string 1)
                               "_"
                               (downcase (match-string 2))
                               (match-string 3))
                       t nil)))))

;;(require-package 'hideshow-org)
;;(require 'hideshow-org)
;;(global-set-key (kbd "C-c s") 'hs-org/minor-mode)

(provide 'init-edit-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-edit-utils.el ends here
