;,-------------------------------------------------------------
;| Moves Some information from the Mode-line to the frame title
;| This is to declutter the already cluttered mode-line
;|
;| From:
;| bit.ly/1nEUm2h
;`-------------------------------------------------------------

(setq display-time-day-and-date t
      display-time-24hr-format t
      display-time-mail-directory "~/.mail/anton-ilyfa.cc/INBOX/"
      display-time-use-mail-icon nil)
(display-time)
(setq global-mode-string nil)

(defun frame-title-prefix()
  (cond (multiple-frames (buffer-name))
        (t (abbreviate-file-name
            (file-name-sans-extension
             (buffer-name))))))

(defun ype/mail-count ()
  (setq count
        (+ (- (length
               (directory-files "~/.mail/anton-ilyfa.cc/INBOX/cur/")) 2)
           (- (length
               (directory-files "~/.mail/anton-ilyfa.cc/INBOX/new/")) 2)))
  (if (> count 0)
    (propertize (format "❝ %d ❞" count))))

(setq frame-title-format
      '(""
        (:eval
         (frame-title-prefix)) " | " display-time-string " | " (:eval (ype/mail-count))))


(provide 'init-mode-line-declutter)
