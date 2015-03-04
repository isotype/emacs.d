;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Filename: init-tramp                                           ;;;
;;; Created: 02-03-2015                                            ;;;
;;; Author: Anton Strilchuk <anton@env.sh>                         ;;;
;;; URL: http://ype.env.sh                                         ;;;
;;; Version:                                                       ;;;
;;; Last-Updated: 02-03-2015                                       ;;;
;;;  Update #: 3                                                   ;;;
;;;   By: Anton Strilchuk <anton@env.sh>                           ;;;
;;;                                                                ;;;
;;; Description:                                                   ;;;
;;;                                                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init-tramp)

(require 'tramp)
(setq tramp-default-method "ssh")
(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))

(defun really-toggle-read-only (&optional force)
  "Change whether this buffer is visiting its file read-only by really
trying to acquire the rights with sudo (and tramp)"
  (interactive "P")
  (let* ((currentfilename buffer-file-name)
   (newfilename
    ;; We first check that the buffer is linked to a file
    (if (not currentfilename)
        ;; If not, we just toggle the read-only mark
        nil
      ;; What is the current state
      (if buffer-read-only
    ;; The buffer is read-only, we should acquire rights to edit it
    (if (and (not force) (file-writable-p currentfilename))
        ;; The file is writable, we don't need to acquire rights
        nil
      (if (buffer-modified-p)
          (error "Buffer is read-only and has been modified. Don't know what to do.")
        ;; To acquire rights, we need to use sudo
        ;; Do we have a tramp file name ?
        (if (eq (string-match tramp-file-name-regexp currentfilename) 0)
      ;; Yes, we add sudo to it
      (let* ((v (tramp-dissect-file-name currentfilename))
             (l (tramp-file-name-localname v)))
        (if (or (string= "sudo"
             (let ((m (tramp-file-name-method v)))
               (if (not (stringp m))
             (car (last (append m nil)))
                 m)))
          (eq (string-match "^sudo::" l) 0))
            (error "This file is already opened with sudo")
          ;; We add sudo
          (let ((toarray (lambda (a) (if (and (not (stringp a)) (arrayp a))
                 a (vector a)))))
            (tramp-make-tramp-file-name
             "multi"
             (vconcat (apply toarray (list (tramp-file-name-method v))) ["sudo"])
             (vconcat (apply toarray (list (tramp-file-name-user v))) ["root"])
             (vconcat (apply toarray (list (tramp-file-name-host v))) ["localhost"])
             l))))
          ;; It is not a tramp file-name
          (tramp-make-tramp-file-name
           nil "sudo" nil "" currentfilename))))
        ;; The buffer is not read-only, we must drop rights
        (if (buffer-modified-p)
      (error "Buffer is modified, save it first first.")
    (if (eq (string-match tramp-file-name-regexp currentfilename) 0)
        ;; We should remove sudo
        (let* ((v (tramp-dissect-file-name currentfilename))
         (l (tramp-file-name-localname v))
         (m (tramp-file-name-method v)))
          ;; Two cases, either sudo is in local file name part or in the methods
          (if (eq (string-match "^sudo::" l) 0)
        ;; Necessary, we have a multi (otherwise, sudo would not have been
        ;; in the localname)
        ;; Do we have more than one method left ?
        (if (> (length m) 1)
            ;; Yes, still multi
            (tramp-make-tramp-file-name
             "multi"
             m
             (tramp-file-name-user v)
             (tramp-file-name-host v)
             (progn (string-match "^sudo::\\(.*\\)$" l)
              (match-string 1 l)))
          ;; We don't need multi anymore
          (tramp-make-tramp-file-name
           nil
           (car (append m nil))
           (car (append (tramp-file-name-user v) nil))
           (car (append (tramp-file-name-host v) nil))
           (progn (string-match "^sudo::\\(.*\\)$" l)
            (match-string 1 l))))
      ;; sudo should be in the methods
      (if (and (stringp m) (string= m "sudo"))
          l
        (if (and (not (stringp m)) (string= (car (last (append m nil))) "sudo"))
            ;; Do we still need multi ?
            (if (> (length m) 2)
          (tramp-make-tramp-file-name
           "multi"
           (apply 'vector (butlast (append
                  m nil)))
           (apply 'vector (butlast (append
                  (tramp-file-name-user v) nil)))
           (apply 'vector (butlast (append
                  (tramp-file-name-host v) nil)))
           l)
        (tramp-make-tramp-file-name
         nil
         (car (append m nil))
         (car (append (tramp-file-name-user v) nil))
         (car (append (tramp-file-name-host v) nil))
         l))
          ;; No sudo found
          nil))))
      ;; This is not a tramp file
      nil))))))
    (if newfilename
  (find-alternate-file newfilename)
      (read-only-mode))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;init-tramp.el ends here
