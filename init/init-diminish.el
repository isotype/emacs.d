(defun ype/diminish-bulk ()
  (interactive)
  (diminish 'visual-line-mode " ⏎")
  (diminish 'auto-complete-mode " ⌦")
  (diminish 'paredit-mode " ⑊")
  (diminish 'whitespace-cleanup-mode " ⌴")
  (diminish 'elisp-slime-nav-mode " ⍄")
  (diminish 'page-break-lines-mode " -")
  (diminish 'eldoc-mode " ⎓")
  (message "diminished (visual-line-mode | auto-complete-mode | paredit-mode | whitespace-cleanup-mode | elisp-slime-nav-mode | page-break-lines-mode | eldoc-mode)"))

(ype/diminish-bulk)

(provide 'init-diminish)
