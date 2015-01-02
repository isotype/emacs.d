;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Author: Anton Strilchuk <anton@isoty.pe>                       ;;;
;;; URL: http://isoty.pe                                           ;;;
;;; Created: 17-04-2014                                            ;;;
;;; Last-Updated: 30-11-2014                                       ;;;
;;;   By: Anton Strilchuk <anton@env.sh>                           ;;;
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
(setq ns-auto-hide-menu-bar nil)
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

(global-set-key (kbd "A-\-") (lambda () (interactive) (adjust-opacity nil -5)))
(global-set-key (kbd "A-\=") (lambda () (interactive) (adjust-opacity nil 5)))
(global-set-key (kbd "A-\+") (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (unless window-system
                (set-frame-parameter nil 'menu-bar-lines 0)))))

(defun frame-title-prefix()
  (cond (multiple-frames (buffer-name))
        (t (abbreviate-file-name (file-name-sans-extension (buffer-name))))))

(defun ype:mail-unread-message-count ()
  (setq unread
        (length
         (directory-files "~/.mail/ilyfa.cc/INBOX/new/" t "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)" t)))
  (if (> unread 0) (propertize (format "%d" unread))))

(defadvice vc-git-mode-line-string (after plus-minus (file) compile activate)
  (setq ad-return-value
        (concat ad-return-value
                (let ((plus-minus (vc-git--run-command-string
                                   file "diff" "--numstat" "--")))
                  (and plus-minus (string-match "^\\([0-9]+\\)\t\\([0-9]+\\)\t" plus-minus)
                     (format " [+%s][-%s]"
                             (match-string 1 plus-minus)
                             (match-string 2 plus-minus)))))))

(defun ype:show-tabbar-groups-in-gui-frame ()
  (when (tabbar-mode-on-p)
    (concat (propertize
             (car (funcall tabbar-buffer-groups-function)) 'face 'font-lock-string-face)
            " > ")))

(defun ype:vc-check ()
  (if (vc-backend (buffer-file-name))
      (progn
        (propertize (vc-git-mode-line-string (buffer-file-name))))
    (propertize (format "[No Repo Found]"))))

(setq display-time-24hr-format t
      display-time-use-mail-icon t)
(display-time-mode t)

(setq frame-title-format
      '("%m "
        " %+ "
        (:eval (ype:vc-check))
        " %+ "
        (:eval (frame-title-prefix))
        " %+ "
        " ["
        (:eval (ype:mail-unread-message-count))
        "] "
        " %+ "
        (:eval (ype:show-tabbar-groups-in-gui-frame))
        ))

;;,-----------------------------------------------------------------
;;| Non-zero values for `line-spacing' can mess up ansi-term and co,
;;| so we zero it explicitly in those cases.
;;`-----------------------------------------------------------------
(add-hook 'term-mode-hook (lambda () (set (make-local-variable 'line-spacing) 0)))


(provide 'init-gui-frames)
