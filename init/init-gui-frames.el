;;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Author: Anton Strilchuk <anton@isoty.pe>                       ;;;
;;; URL: http://isoty.pe                                           ;;;
;;; Created: 17-04-2014                                            ;;;
;; Last-Updated: 21-08-2014                                         ;;
;;   By: Anton Strilchuk <ype@env.sh>                               ;;
;;;                                                                ;;;
;;; Filename: init-gui-frames                                      ;;;
;;; Version:                                                       ;;;
;;; Description:                                                   ;;;
;;;                                                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;----------------------------------------------------------------------------
;; Stop C-z from minimizing windows under OS X
;;----------------------------------------------------------------------------
(defun maybe-suspend-frame ()
  (interactive)
  (unless (and *is-a-mac* window-system)
    (suspend-frame)))

(global-set-key (kbd "C-z") 'maybe-suspend-frame)
;;----------------------------------------------------------------------------
;; Suppress GUI features
;;----------------------------------------------------------------------------
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
;;----------------------------------------------------------------------------
;; Show a marker in the left fringe for lines not in the buffer
;;----------------------------------------------------------------------------
(setq indicate-empty-lines t)
;;----------------------------------------------------------------------------
;; Window size and features
;;----------------------------------------------------------------------------
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

(defun adjust-opacity (frame incr)
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))

(when (and *is-a-mac* (fboundp 'toggle-frame-fullscreen))
  ;; Option-super-f to toggle fullscreen mode
  ;; Hint: Customize `ns-use-native-fullscreen'
  (global-set-key (kbd "M-s-f") 'toggle-frame-fullscreen))

(global-set-key (kbd "M-C-8") '(lambda () (interactive) (adjust-opacity nil -5)))
(global-set-key (kbd "M-C-9") '(lambda () (interactive) (adjust-opacity nil 5)))
(global-set-key (kbd "M-C-0") '(lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (unless window-system
                (set-frame-parameter nil 'menu-bar-lines 0)))))

(defun frame-title-prefix()
  (cond (multiple-frames (buffer-name))
        (t (abbreviate-file-name (file-name-sans-extension (buffer-name))))))

(defun ype/mail-count ()
  (setq count
        (+ (- (length (directory-files "~/.mail/anton-ilyfa.cc/INBOX/cur/")) 2)
           (- (length (directory-files "~/.mail/anton-ilyfa.cc/INBOX/new/")) 2)))
  (if (> count 0) (propertize (format "%d" count))))


(defadvice vc-git-mode-line-string (after plus-minus (file) compile activate)
  (setq ad-return-value
    (concat ad-return-value
            (let ((plus-minus (vc-git--run-command-string
                               file "diff" "--numstat" "--")))
              (and plus-minus
                   (string-match "^\\([0-9]+\\)\t\\([0-9]+\\)\t" plus-minus)
                   (format " [+%s][-%s]" (match-string 1 plus-minus) (match-string 2 plus-minus)))))))

(setq frame-title-format
      '("%m "
        " ↂ "
        (:eval (ype/mail-count))
        " ↂ "
        (:eval (vc-git-mode-line-string (buffer-file-name)))
        " ↂ "
        (:eval (frame-title-prefix))
        " %-"))

;;,-----------------------------------------------------------------
;;| Non-zero values for `line-spacing' can mess up ansi-term and co,
;;| so we zero it explicitly in those cases.
;;`-----------------------------------------------------------------
(add-hook 'term-mode-hook
          (lambda ()
            (set (make-local-variable 'line-spacing)
                 0)))

;; OSX 10.6 Plus - Auto Show/Hide Menubar
(setq ns-auto-hide-menu-bar t)


(provide 'init-gui-frames)
