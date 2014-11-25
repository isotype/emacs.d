;;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Author: Anton Strilchuk <anton@isoty.pe>                       ;;;
;;; URL: http://isoty.pe                                           ;;;
;;; Created: 24-04-2014                                            ;;;
;; Last-Updated: 03-11-2014                                         ;;
;;   By: Anton Strilchuk <anton@env.sh>                             ;;
;;;                                                                ;;;
;;; Filename: init-latex                                           ;;;
;;; Version:                                                       ;;;
;;; Description:                                                   ;;;
;;;                                                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require-package 'auctex)
(require-package 'flycheck)
(require-package 'auto-complete-auctex)
(require-package 'magic-latex-buffer)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-save-query nil)
(setq TeX-PDF-mode t) ; Compile to PDF by default
(add-to-list 'ac-modes 'LaTeX-mode)

;; set XeTeX mode in TeX/LaTeX
(add-hook 'LaTeX-mode-hook
          (lambda()
            (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
            (setq TeX-command-default "XeLaTeX")
            (setq TeX-save-query nil)
            (setq TeX-show-compilation t)))

;;Spell Check for LaTeX
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-buffer)

(defun turn-on-outline-minor-mode ()
  (outline-minor-mode 1))

(add-hook 'LaTeX-mode-hook 'turn-on-outline-minor-mode)
(add-hook 'latex-mode-hook 'turn-on-outline-minor-mode)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
(add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode

(setq TeX-view-program-list
      '(("PDF Viewer" "/opt/homebrew-cask/Caskroom/skim/1.4.8/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))

;; AC Maths
(require-package 'ac-math)
(add-to-list 'ac-modes 'latex-mode)   ; make auto-complete aware of `latex-mode`
(setq ac-math-unicode-in-math-p t)

(defun ac-latex-mode-setup ()
  (setq ac-sources
        (append '(ac-source-math-unicode ac-source-latex-commands)
                ac-sources)))
(add-hook 'latex-mode-hook 'ac-latex-mode-setup)
(ac-flyspell-workaround)

;;,-=========-
;;| Org Latex
;;`-=========-
(require 'ob-latex)
(require 'ox-latex)
(require 'ox-html)

(setq org-export-with-sub-superscripts '{})
(setq org-export-with-smart-quotes t)

(setq org-html-preamble nil)
(setq org-html-postamble nil)
(setq org-html-doctype "html5")
(setq org-html-html5-fancy t)
(setq org-html-head-extra
      (concat
       "<style type=\"text/css\">\n"
       "body { font-family: sans-serif; font-size: 1em; line-height: 1.5; color: #222222 }"
       "table { border: 1px solid black; }"
       "td { padding: 2px 4px; }"
       "</style>\n"))

(setq org-html-mathjax-options (quote ((path "http://cdn.mathjax.org/mathjax/latest/MathJax.js")
                                       (scale "100")
                                       (align "center")
                                       (indent "2em")
                                       (mathml nil))))

(add-to-list 'org-latex-packages-alist '("" "array"))
(add-to-list 'org-latex-packages-alist '("" "booktabs"))
(add-to-list 'org-latex-packages-alist '("stretch=10" "microtype"))
(add-to-list 'org-latex-packages-alist '("" "lmodern"))

;; use xelatex
(setq org-latex-pdf-process
      '("xelatex -shell-escape -interaction nonstopmode %f; bibtex %f; xelatex -shell-escape -interaction nonstopmode %f; xelatex -shell-escape -interaction nonstopmode %f"))

(setq org-latex-listings 'listings
      org-latex-listings-options
      '(("basicstyle" "\\listingsfont")
        ("xleftmargin" "0.7cm")
        ("xrightmargin" "0.5cm")
        ("showstringspaces" "false")
        ("keepspaces" "true")
        ("numbers" "left")
        ("numberstyle" "\\scriptsize")
        ("stepnumber" "1")
        ("frame" "leftline")
        ("rulesepcolor" "\\color{gray}")))

(add-to-list 'org-latex-classes
             '("yarticle"
               "\\documentclass[11pt,a4paper, titlepage]{article}
\\usepackage[math-style=TeX,vargreek-shape=unicode]{unicode-math}
\\newfontfamily\\setmainfont[%
Mapping=tex-text,
Color=textcolor,
Path=/Users/anton/Documents/Fonts/a2d/B/OpenType/]{BaskervilleBook-Regular.otf}
\\newfontfamily\\listingsfont[Scale=0.8, Path=/Users/anton/Library/Fonts/]{FiraMonoOT-Regular.otf}
\\setmathfont{xits-math.otf}
\\usepackage{tikz}
\\usepackage{graphicx}
\\usepackage{geometry}
\\usepackage{color}
\\usepackage{xcolor}
\\definecolor{lightgrey}{gray}{0.8}

\\usepackage{algorithm}
\\usepackage[noend]{algpseudocode}

\\usepackage{listings}
\\usepackage{hyperref}
\\usepackage{enumitem}
\\setitemize{noitemsep}

\\setlist[itemize,1]{label=\\textbullet}
\\setlist[itemize,2]{label=\\textopenbullet}
\\setlist[itemize,3]{label=\\textbullet}
\\setlist[itemize,4]{label=\\textopenbullet}

\\usepackage{fancyhdr}
\\setlength{\\headheight}{15pt}
\\pagestyle{fancy}
\\renewcommand{\\sectionmark}[1]{ \\markright{#1}{} }
\\fancyhf{}
\\fancyhead[LE,RO]{\\thepage}
\\fancyhead[RE]{\\textit{ \\nouppercase{\\leftmark}} }
\\fancyhead[LO]{\\textit{ \\nouppercase{\\rightmark}} }

\\fancypagestyle{plain}{ %
  \\fancyhf{} % remove everything
  \\renewcommand{\\headrulewidth}{0pt} % remove lines as well
  \\renewcommand{\\footrulewidth}{0pt}
}

\\usepackage{caption}
\\DeclareCaptionFont{white}{\\color{white}}
\\DeclareCaptionFormat{listing}{\\colorbox{gray}{\\parbox{\\textwidth}{#1#2#3}}}
\\captionsetup[lstlisting]{format=listing,labelfont=white,textfont=white}

[NO-PACKAGES]
[NO-DEFAULT-PACKAGES]"
("\\section{%s}" . "\\section*{%s}")
("\\subsection{%s}" . "\\subsection*{%s}")
("\\subsubsection{%s}" . "\\subsubsection*{%s}")
("\\paragraph{%s}" . "\\paragraph*{%s}")
("\\subparagraph{%s}" . "\\subparagraph*{%s}")))



(provide 'init-latex)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-latex.el ends here
