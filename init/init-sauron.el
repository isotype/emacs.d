;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Filename: init-sauron                                          ;;;
;;; Created: 10-02-2015                                            ;;;
;;; Author: Anton Strilchuk <anton@env.sh>                         ;;;
;;; URL: http://ype.env.sh                                         ;;;
;;; Version:                                                       ;;;
;;; Last-Updated: 04-03-2015                                       ;;;
;;;  Update #: 10                                                  ;;;
;;;   By: Anton Strilchuk <anton@env.sh>                           ;;;
;;;                                                                ;;;
;;; Description:                                                   ;;;
;;;                                                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init-sauron)

(require-package 'sauron)
(require 'sauron)
(require 'sauron-jabber)

(add-hook 'sauron-event-added-functions 'sauron-alert-el-adapter)
(global-set-key (kbd "C-c s") 'sauron-toggle-hide-show)
(global-set-key (kbd "C-c t") 'sauron-clear)


(setq sauron-timestamp-format "%H:%M:%S")
(setq sauron-column-alist
      '((timestamp . 10)
        (origin . 7)
        (priority . 4)
        (message . nil)))
(setq sauron-max-line-length 150
      sauron-hide-mode-line t
      sauron-separate-frame nil
      sauron-watch-patterns '("astrilchuk")
      sauron-watch-nicks '("astrilchuk"))

;; (setq display-buffer-alist              ; Treat sauron specially.
;;       '("*Sauron*" . ""))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;init-sauron.el ends here
