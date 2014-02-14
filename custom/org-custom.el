;;ORG MODE
(require 'org)
(require 'org-src)
(require 'org-id)
(require 'htmlize)
(require 'org-location-google-maps)
(require 'org-habit)
(require 'org-protocol)
(require 'ox-pandoc)

;;Give buffers in different major-modes
;;the 'look-and-feel' of Org-mode buffers
(require 'outshine)
(add-hook 'outline-minor-mode-hook 'outshine-hook-function)
;;Org-style comment editing
(require 'outorg)

;; Enable habit tracking (and a bunch of other modules)
(setq org-modules (quote (org-bbdb
                          org-bibtex
                          org-crypt
                          org-gnus
                          org-id
                          org-info
                          org-habit
                          org-inlinetask
                          org-irc
                          org-mew
                          org-mhe
                          org-protocol
                          org-vm
                          org-wl
                          org-w3m)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (ditaa . t)
   (dot . t)
   (plantuml . t)
   (emacs-lisp . t)
   (gnuplot . t)
   (haskell . nil)
   (latex . t)
   (ledger . t)
   (ocaml . nil)
   (octave . t)
   (python . t)
   (ruby . t)
   (screen . nil)
   (sh . t)
   (sql . nil)
   (sqlite . t)))

(setq org-plantuml-jar-path
      (expand-file-name "/Users/antonstrilchuk/.emacs.d/plantuml.jar"))

;;Save all org buffers 1 minute before every hour
(run-at-time "00:59" 3600 'org-save-all-org-buffers)

;;Function to eval-buffer and org-reload when editing orgmode preferences
;; (defun yy_pe/eval-buffer-and-org-reload ()
;;   (interactive)
;;   (setq buffnam '(".emacs"))
;;   (when t 'buffer-name ".emacs")
;;   (eval-buffer)
;;   (org-reload))

(setq org-directory "~/Dropbox/ORGS/")
(setq org-archive-location (concat org-directory "archive/%s_archive::"))
(setq org-default-notes-file (concat org-directory "gtd.org"))

;;ORGFILES
(setq org-agenda-files
      '("~/Dropbox/ORGS/gtd.org"
	"~/Dropbox/ORGS/clients/c_homeroom.org"
	"~/Dropbox/ORGS/school/s_main.org"
	"~/Dropbox/ORGS/refile.org"))
(setq org-alphabetical-lists t)
(setq org-startup-folded t)
(setq org-startup-indented t)
(setq org-odd-levels-only t)
(setq org-hide-leading-stars nil)
(setq org-pretty-entities t)
(setq org-enforce-todo-dependencies t)
(setq org-clone-delete-id t)
(setq org-return-follows-link t)
(setq org-cycle-separator-lines 0)
(setq org-insert-heading-respect-content nil)

;;Use System Settings For File-Application Selection
(setq org-file-apps (quote ((auto-mode . emacs)
                            ("\\.mm\\'" . system)
                            ("\\.x?html?\\'" . system)
                            ("\\.pdf\\'" . system))))

;;UNIQUE IDs for TASKS
(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

;;If Priority needed
;;Priorities A-E where tasks without a specific priority are lowest priority E
(setq org-enable-priority-commands t)
(setq org-default-priority ?E)
(setq org-lowest-priority ?E)

(setq org-habit-graph-column 50)
(run-at-time "06:00" 86400 '(lambda () (setq org-habit-show-habits t)))

;;Change Org Bullets
(font-lock-add-keywords
 'org-mode `(("\\(?:^\\(?1:\\*+\\)[[:blank:]]\\)"
              (0 (progn (compose-region
                         (match-beginning 1) (match-end 1)
                         (pcase (length (match-string 1))
                           (1 ?\u25C9)
                           (2 ?\u2022)
                           (3 ?\u25CE)
			   (_ ?\u26AB))
			 nil))))))

;;Custom Defaults
(setq org-upcoming-deadline '(:foreground "blue" :weight bold))
;;fontify code in code blocks
(setq org-src-fontify-natively t)

;;AGENDA
;;Show Today in Agenda
(setq org-agenda-span 'day)
(setq org-agenda-sticky t)

(setq org-agenda-include-diary nil)
(setq org-agenda-show-all-dates t)

(setq org-agenda-skip-deadline-if-done t)

(setq org-agenda-skip-scheduled-if-done t)

;; Start the weekly agenda on Monday
(setq org-agenda-start-on-weekday 1)
(setq org-deadline-warning-days 30)

(setq org-agenda-persistent-filter t)

;;Remove Multiple State Change Log Details From The Agenda
(setq org-agenda-skip-additional-timestamps-same-entry t)

;; Show all future entries for repeating tasks
(setq org-agenda-repeating-timestamp-show-all t)

;; Show all agenda dates - even if they are empty
(setq org-agenda-show-all-dates t)

;; Sorting order for tasks on the agenda
(setq org-agenda-sorting-strategy
      (quote ((agenda habit-down time-up user-defined-up priority-down effort-up category-keep)
              (todo category-up priority-down effort-up)
              (tags category-up priority-down effort-up)
              (search category-up))))

;; Enable display of the time grid so we can see the marker for the current time
(setq org-agenda-time-grid (quote ((daily today remove-match)
                                   #("----------------" 0 16 (org-heading t))
                                   (0900 1100 1300 1500 1700))))

;;Include Archive in Search
(setq org-agenda-text-search-extra-files '((agenda-archives)))

;; Do not dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)

;; Compact the block agenda view
(setq org-agenda-compact-blocks t)

;; Keep tasks with dates on the global todo lists
(setq org-agenda-todo-ignore-with-date nil)

;; Keep tasks with deadlines on the global todo lists
(setq org-agenda-todo-ignore-deadlines nil)

;; Keep tasks with scheduled dates on the global todo lists
(setq org-agenda-todo-ignore-scheduled nil)

;; Keep tasks with timestamps on the global todo lists
(setq org-agenda-todo-ignore-timestamp nil)

;; Remove completed deadline tasks from the agenda view
(setq org-agenda-skip-deadline-if-done t)

;; Remove completed scheduled tasks from the agenda view
(setq org-agenda-skip-scheduled-if-done t)

;; Remove completed items from search results
(setq org-agenda-skip-timestamp-if-done t)

;;Unique IDs
(setq org-id-method 'uuid)

;;Search
(setq org-show-following-heading t)
(setq org-show-hierarchy-above t)
(setq org-show-siblings '(default))

;;#+STARTUP
(setq org-startup-align-all-tables t)

;;LOGGING
(setq org-log-into-drawer t)
(setq org-log-state-notes-insert-after-drawers nil)
(setq org-log-done 'time)
(setq org-log-done 'note)
;;show notes at top
(setq org-reverse-note-order nil)
;; Agenda log mode items to display (closed and state changes by default)
(setq org-agenda-log-mode-items '(closed state))

;;CLOCK
;;ORG CLOCK IDLE
(setq org-clock-idle-time 2)
(org-clock-persistence-insinuate)
;; Show lot of clocking history so it's easy to pick items off the C-F11 list
(setq org-clock-history-length 23)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
;; Separate drawers for clocking and logs
(setq org-drawers '("PROPERTIES" "LOGBOOK"))
;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist 'history)
;; Do not prompt to resume an active clock
(setq org-clock-persist-query-resume nil)
;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution 'when-no-clock-is-running)
;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)
(setq org-clock-sound "/Users/antonstrilchuk/Library/Sounds/Quack.aiff")
(setq org-time-stamp-rounding-minutes '(1 1))
(setq org-agenda-clock-consistency-checks
      '((:max-duration "4:00"
		       :min-duration 0
		       :max-gap 0
		       :gap-ok-around ("4:00"))))

(defvar y_pe/default-task-id "219B72C2-369B-4169-B6AA-C228DB3F2B11")
(defvar y_pe/default-email-id "AC4B5743-4FDC-4886-9084-D2F1AA29047E")
(defvar y_pe/default-elisp-id "8FE1162A-C5E3-4B7C-B0B6-9B3EDED7B477")
(defvar y_pe/default-school-id "20024CD9-3C36-468A-B17C-1505EA13E794")

(defun y_pe/clock-in-default-task-as-default ()
  (interactive)
  (org-with-point-at (org-id-find y_pe/default-task-id 'marker)
    (org-clock-in '(16))))

(defun y_pe/clock-in-default-school ()
  (interactive)
  (org-with-point-at (org-id-find y_pe/default-school-id 'marker)
    (org-clock-in '(16))))

(defun y_pe/clock-in-default-email ()
  (interactive)
  (org-with-point-at (org-id-find y_pe/default-email-id 'marker)
    (org-clock-in '(16))))

(defun y_pe/clock-in-default-elisp ()
  (interactive)
  (org-with-point-at (org-id-find y_pe/default-elisp-id 'marker)
    (org-clock-in '(16))))

;;ORG Archive All Done Tasks
(defun y_pe/org-archive-done-tasks ()
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))

;;LATEX
(setq org-export-latex-listings t)

;; Originally taken from Bruno Tavernier: http://thread.gmane.org/gmane.emacs.orgmode/31150/focus=31432
;; but adapted to use latexmk 4.20 or higher.
(defun y_pe/auto-tex-cmd ()
  "When exporting from .org with latex, automatically run latex,
     pdflatex, or xelatex as appropriate, using latexmk."
  (let ((texcmd)))
  ;; default command: oldstyle latex via dvi
  (setq texcmd "latexmk -dvi -pdfps -quiet %f")
  ;; pdflatex -> .pdf
  (if (string-match "LATEX_CMD: pdflatex" (buffer-string))
      (setq texcmd "pdflatex %f; bibtex %f; pdflatex %f"))
  ;; xelatex -> .pdf
  (if (string-match "LATEX_CMD: xelatex" (buffer-string))
      (setq texcmd "latexmk -xelatex -pdf -bibtex -quiet %f"))
  ;; LaTeX compilation command
  (setq org-latex-to-pdf-process (list texcmd)))
(add-hook 'org-export-latex-after-initial-vars-hook 'y_pe/auto-tex-cmd)

;; Specify default packages to be included in every tex file, whether pdflatex or xelatex
(setq org-export-latex-packages-alist
      '(("" "graphicx" t)
	("" "longtable" nil)
	("" "float" nil)))

(defun y_pe/auto-tex-parameters ()
  "Automatically select the tex packages to include."
  ;; default packages for ordinary latex or pdflatex export
  (setq org-export-latex-default-packages-alist
	'(("AUTO" "inputenc" t)
	  ("T1"   "fontenc"   t)
	  (""     "fixltx2e"  nil)
	  (""     "wrapfig"   nil)
	  (""     "soul"      t)
	  (""     "textcomp"  t)
	  (""     "marvosym"  t)
	  (""     "wasysym"   t)
	  (""     "latexsym"  t)
	  (""     "amssymb"   t)
	  (""     "hyperref"  nil)))

  ;; Packages to include when xelatex is used
  (if (string-match "LATEX_CMD: xelatex" (buffer-string))
      (setq org-export-latex-default-packages-alist
	    '(("" "fontspec" t)
	      ("" "xunicode" t)
	      ("" "url" t)
	      ("" "rotating" t)
	      ("UKenglish" "babel" t)
	      ("babel" "csquotes" t)
	      ("" "soul" t)
	      ("xetex" "hyperref" nil)
	      )))

  (if (string-match "LATEX_CMD: xelatex" (buffer-string))
      (setq org-export-latex-classes
	    (cons '("article"
		    "\\documentclass[a4paper,11pt,article,oneside]{memoir}"
		    ("\\section{%s}" . "\\section*{%s}")
		    ("\\subsection{%s}" . "\\subsection*{%s}")
		    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		    ("\\paragraph{%s}" . "\\paragraph*{%s}")
		    ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
		  org-export-latex-classes))))

(add-hook 'org-export-latex-after-initial-vars-hook 'y_pe/auto-tex-parameters)

;;iCalendar
(setq org-icalendar-combined-agenda-file "~/Dropbox/ORGS/combi.ics")
(setq org-icalendar-combined-name "GTD")
(setq org-icalendar-include-todo t)
(setq org-icalendar-include-body t)
(setq org-icalendar-categories '(todo-state all-tags category))
(setq org-icalendar-exclude-tags '("HABIT" "IGNORE"))
(setq org-icalendar-store-UID t)
(setq org-icalendar-alarm-time 30)
(setq org-icalendar-use-scheduled '(todo-start event-if-todo))
(setq org-icalendar-use-deadline '(todo-due event-if-todo))
(setq org-icalendar-use-plain-timestamp nil)
(setq org-icalendar-timezone "Europe/London")

;;REFILE Settings
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9)))) ;; 1
(setq org-refile-use-outline-path t);; 2
(setq org-outline-path-complete-in-steps nil);; 3
(setq org-completion-use-ido t);; 4
(setq ido-everywhere t);; 4
(setq ido-max-directory-size 100000);; 4
(ido-mode (quote both));; 4
(setq ido-default-file-method 'selected-window);; 5
(setq ido-default-buffer-method 'selected-window);; 5
(setq org-indirect-buffer-display 'current-window);; 6
;; 1 Targets include this file and any file contributing to the agenda - up to 9 levels deep
;; 2 Use full outline paths for refile targets - we file directly with IDO
;; 3 Targets complete directly with IDO
;; 4 Use IDO for both buffer and file completion and ido-everywhere to t
;; 5 Use the current window when visiting files and buffers with ido
;; 6 Use the current window for indirect buffer display

;;Change List Bullets
(setq org-list-demote-modify-bullet (quote (("+" . "-")
                                            ("*" . "-")
                                            ("1." . "-")
                                            ("1)" . "-")
                                            ("A)" . "-")
                                            ("B)" . "-")
                                            ("a)" . "-")
                                            ("b)" . "-")
                                            ("A." . "-")
                                            ("B." . "-")
                                            ("a." . "-")
                                            ("b." . "-"))))

;; Set default column view headings: Task Effort Clock_Summary
(setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")
					; global Effort estimate values
					; global STYLE property values for completion
(setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                    ("STYLE_ALL" . "habit"))))

;;TODO STATES
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d@/!)")
	(sequence "REPORT(R@/!)" "BUG(B@/!)" "CAUSE(C@/!)" "|" "FIXED(F@/!)")
	(sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "KILLED(k@/!)" "REPLY(r!)" "MEETING(m@/!)")
	(sequence "PENDING(p@/!)" "|" "PAID(P@/!)" "CANCELLED(c@/!)")
	(sequence "READING(a!)" "|" "READ(s!)" "SCRAPED(S!)")))

(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "#FF0066" :weight bold))
	("NEXT" . (:foreground "#FF9999" :weight bold))
	("DOING" . (:foreground "#FF9966" :weight bold))
	("DONE" . (:foreground "#006666" :weight bold-italic))
	("REPORT" . (:foreground "#993300" :weight bold))
	("BUG" . (:foreground "#990000" :weight bold))
	("CAUSE" . (:foreground "#663300" :weight bold))
	("FIXED" . (:foreground "#336600" :weight bold))
	("WAITING" . (:foreground "#FF0F00" :weight bold))
	("HOLD" . (:foreground "#FFFF00" :weight bold))
	("KILLED" . (:foreground "#006666" :weight bold))
	("REPLY" . (:foreground "#3366FF" :weight bold))
	("MEETING" . (:foreground "#FF5100" :weight bold))
	("PENDING" . (:foreground "#66d9ef" :weight bold))
	("PAID" . (:foreground "#a6e22e" :weight bold))
	("CANCELLED" . (:foreground "#BF2822" :weight bold))
	("READING" . (:foreground "#AAF22e" :weight bold))
	("READ" . (:foreground "#AAFFF1" :weight bold))
	("SCRAPED" . (:foreground "#BB6e22" :weight bold))))

;; TAGS
(setq org-tag-alist '((:startgroup . nil)
		      ("@office" . ?O)
		      ("@home" . ?H)
		      ("@school" . ?S)
		      ("@errand" . ?E)
		      (:endgroup . nil)
		      ("HOLD" . ?h)
		      ("FLAGGED" . ?f)
		      ("ADMIN" . ?a)
		      ("RESEARCH" . ?r)
		      ("TEACHING" . ?t)
		      ("NOTE" . ?n)
		      ("EXPENSE" .?e)
		      ("MUU" . ?m)
		      ("HOMEROOM" . ?1)
		      ("CodaSign" . ?3)
		      ("CRYPT" . ?C)
		      ("PERSONAL" . ?p)
		      ("MAJ_PROJ" . ?j)))

(setq org-fast-tag-selection-single-key (quote expert))
(setq org-agenda-tags-todo-honor-ignore-options t)

;;ORG Remember Templates
;; | %a        | annotation normally the link created with org-store-link      |
;; | %A        | like %a but prompt for the description part                   |
;; | %i        | initial content the region when remember is called with C-u.  |
;; | %t        | timestamp date only                                           |
;; | %T        | timestamp with date and time                                  |
;; | %u %U     | like the above but inactive timestamps                        |
;; | %^t       | like %t but prompt for date.                                  |
;; | %n        | user name (taken from user-full-name)                         |
;; | %c        | Current kill ring head.                                       |
;; | %x        | Content of the X clipboard.                                   |
;; | %^C       | Interactive selection of which kill or clip to use.           |
;; | %^L       | Like %^C but insert as link.                                  |
;; | %k        | title of the currently clocked task                           |
;; | %K        | link to the currently clocked task                            |
;; | %^g       | prompt for tags with completion on tags in target ﬁle.        |
;; | %^G       | prompt for tags, with completion all tags in all agenda ﬁles. |
;; | %^{prop}p | Prompt the user for a value for property prop %:keyword       |
;; | %[file]   | insert the contents of the ﬁle given by ﬁle                   |
;; | %(sexp)   | evaluate Elisp sexp and replace with the result               |
;; | %!        | immediately store note after completing the template          |
;; | %&        | jump to target location immediately after storing note        |

(setq org-capture-templates
      '(("t" "TODO" entry (file "~/Dropbox/ORGS/refile.org")
	 "* NEXT %^{Brief Description} %^G\nDEADLINE: %^t \n%? \nAdded: %U" :clock-in t :clock-resume t)
	("m" "MEETING" entry (file "~/Dropbox/ORGS/refile.org")
	 "* MEETING %^{Brief Description} :MEETING:\n- WHO: %^{Who?}\n- WHAT: %^{What?}\n   SCHEDULED:%^t\n:PROPERTIES:\n:ADDRESS: %^{Where?}\n:END:\n" :clock-in t :clock-resume t)
	("n" "NOTE" entry (file "~/Dropbox/ORGS/refile.org")
	 "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
	("r" "RESPOND" entry (file "~/Dropbox/ORGS/refile.org")
	 "* NEXT Respond to %^{Who's it From} about %^{What's it about?}\nSCHEDULED: %^t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
	("h" "HABIT" entry (file "~/Dropbox/ORGS/refile.org")
	 "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n" :clock-in t :clock-resume t)
	("p" "PHONE" entry (file "~/Dropbox/ORGS/refile.org")
	 "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
	("e" "EXPENSE" entry (file "~/Dropbox/ORGS/refile.org")
	 "* PENDING %^{What are the items?} for %^{Who are the items for?}, %^{Which Project?} project %^G\nDEADLINE: %^t\n:PROPERTIES:\nCATEGORY: EXPENSE\n:\nADDED:%U\n:END:\n")
	("j" "Journals")
	("jp" "Project Journal" plain
	 (file+datetree+prompt "~/Dropbox/ORGS/journals/projects.org")
	 "- *%^{Title}*\n\t- *Time Spent:* %^{Time spent}\n\t- *Working on:* %^{What have you been working on?}\n\t- *Additional Comments*\n\t- %? \n\t- *Created at:* %T")
	 ("jd" "Code Journal" plain
	  (file+datetree+prompt "~/Dropbox/ORGS/journal.org")
	 "* %^{Title}\n1. *What did you code*\n%^{What did you code?}\n2. *How long did you code for*\n%^{How long did you code for?}\n3. *Where did you code*\n%^{Where did you code?}\n4. *Why did you code this*\n%^{Why did you code this?}")
	 ))
;;END Templates

;;ORG AGENDA CUSTOM COMMANDS
(setq org-agenda-custom-commands
      '(("c" . "Custom queries")
	("cd" "Upcoming deadlines" agenda ""
	 ((org-agenda-entry-types '(:deadline))
	  (org-agenda-ndays 1)
	  (org-deadline-warning-days 60)
	  (org-agenda-time-grid nil)))
	("n" "Next Task" todo "NEXT"
	 ((org-agenda-overriding-header "Next Tasks")
	  (org-agenda-overriding-columns-format "%15ITEM %25SCHEDULED %25DEADLINE")
	  (org-agenda-view-columns-initially t)
	  (org-agenda-todo-ignore-scheduled nil)
	  (org-agenda-todo-ignore-deadlines nil)
	  (org-agenda-todo-ignore-with-date nil)
	  (org-tags-match-list-sublevels t)
	  (org-agenda-sorting-strategy
	   '(scheduled-up deadline-down todo-state-up category-keep))))
	("u" "Meetings" todo "MEETING"
	 ((org-agenda-overriding-header "Meeting")
	  (org-agenda-overriding-columns-format "%30ITEM %SCHEDULED")
	  (org-agenda-view-columns-initially t)
	  (org-agenda-todo-ignore-scheduled nil)
	  (org-agenda-todo-ignore-deadlines nil)
	  (org-agenda-todo-ignore-with-date nil)
	  (org-tags-match-list-sublevels t)
	  (org-agenda-sorting-strategy
	   '(scheduled-up todo-state-up category-keep))))
	("p" "Pending Payments" todo "PENDING"
	 ((org-agenda-overriding-header "Next Tasks")
	  (org-agenda-todo-ignore-scheduled nil)
	  (org-agenda-todo-ignore-deadlines nil)
	  (org-agenda-todo-ignore-with-date nil)
	  (org-tags-match-list-sublevels t)
	  (org-agenda-sorting-strategy
	   '(scheduled-up todo-state-up effort-up category-keep))))
	("ca" "All Tags"
	 ((tags-todo "@office")
	  (tags-todo "@home")
	  (tags-todo "@school")
	  (tags-todo "@errand")
	  (tags-todo "HOLD")
	  (tags-todo "FLAGGED")
	  (tags-todo "ADMIN")
	  (tags-todo "R&D")
	  (tags-todo "TEACHING")
	  (tags-todo "NOTE")
	  (tags-todo "EXPENSE")
	  (tags-todo "MUU")
	  (tags-todo "HOMEROOM")
	  (tags-todo "CodaSign")
	  (tags-todo "CRYPT")
	  (tags-todo "PERSONAL")
	  (tags-todo "MAJ_PROJ")))
	("w" "Waiting Tasks" todo "WAITING"
	 ((org-agenda-overriding-header "Waiting")
	  (org-tags-match-list-sublevels nil)
	  (org-agenda-sorting-strategy '(priority-down))))
	("cx" "With deadline columns" todo "TODO"
	 ((org-agenda-overriding-columns-format "%20ITEM %DEADLINE")
	  (org-agenda-view-columns-initially t)))
	("r" "Tasks to Refile" tags "REFILE"
	 ((org-agenda-overriding-header "Tasks to Refile")
	  (org-tags-match-list-sublevels nil)))
	("cs" "School"
	 ((tags-todo "@school"))
	 ((org-agenda-compact-blocks t)))
	("cc" "All Clients"
	 ((tags-todo "@office"))
	 ((org-agenda-compact-blocks t)))
	("ca" "All Tasks" todo "NEXT"
	 ((org-agenda-todo-ignore-scheduled nil)
	  (org-agenda-todo-ignore-deadlines nil)
	  (org-agenda-todo-ignore-with-date nil)))
	("cN" "Notes" tags "NOTE"
	 ((org-agenda-overriding-header "Notes")
	  (org-tags-match-list-sublevels t)))
	("ch" "Daily habits" 
         ((agenda ""))
         ((org-agenda-show-log t)
          (org-agenda-ndays 7)
          (org-agenda-log-mode-items '(state))
          (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp ":HABIT:"))))
	))
;;END CUSTOM COMMANDS

;;DIARY
(setq org-agenda-diary-file "~/Dropbox/ORGS/personal/diary")
(setq org-agenda-insert-diary-extract-time t)
(add-hook 'org-agenda-cleanup-fancy-diary-hook
          (lambda ()
            (goto-char (point-min))
            (save-excursion
              (while (re-search-forward "^[a-z]" nil t)
                (goto-char (match-beginning 0))
                (insert "0:00-24:00 ")))
            (while (re-search-forward "^ [a-z]" nil t)
              (goto-char (match-beginning 0))
              (save-excursion
                (re-search-backward "^[0-9]+:[0-9]+-[0-9]+:[0-9]+ " nil t))
              (insert (match-string 0)))))

;;AUTOFILLS
;;(add-hook 'org-mode-hook 'turn-on-auto-fill) ;;WORD WRAP
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;;CUSTOM Functions by Bernt Hansen (bh/)
(defun bh/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at "LOGBOOK" (point))))
(add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)

(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))
(setq org-refile-target-verify-function 'bh/verify-refile-target)

(defvar bh/insert-inactive-timestamp t)

(defun bh/toggle-insert-inactive-timestamp ()
  (interactive)
  (setq bh/insert-inactive-timestamp (not bh/insert-inactive-timestamp))
  (message "Heading timestamps are %s" (if bh/insert-inactive-timestamp "ON" "OFF")))

(defun bh/insert-inactive-timestamp ()
  (interactive)
  (org-insert-time-stamp nil t t nil nil nil))

(defun bh/insert-heading-inactive-timestamp ()
  (save-excursion
    (when bh/insert-inactive-timestamp
      (org-return)
      (org-cycle)
      (bh/insert-inactive-timestamp))))

(add-hook 'org-insert-heading-hook 'bh/insert-heading-inactive-timestamp 'append)
(setq org-export-with-timestamps nil)


;;ORG AUTO-COMPLETE
(defvar ac-org-candidates nil)
(defvar ac-org-pattern nil)
(defun ac-org-construct-candidates ()
  "Pabbrev source for org."
  (let* ((end (point))
	 (beg1 (save-excursion
		 (skip-chars-backward (org-re "[:alnum:]_@"))
		 (point)))
	 (beg (save-excursion
		(skip-chars-backward "a-zA-Z0-9_:$")
		(point)))
	 (confirm (lambda (x) (stringp (car x))))
	 (searchhead (equal (char-before beg) ?*))
	 (struct
	  (when (and (member (char-before beg1) '(?. ?<))
		     (setq a (assoc (buffer-substring beg1 (point))
				    org-structure-template-alist)))
	    (org-complete-expand-structure-template (1- beg1) a)
	    (throw 'exit t)))
	 (tag (and (equal (char-before beg1) ?:)
		   (equal (char-after (point-at-bol)) ?*)))
	 (prop (and (equal (char-before beg1) ?:)
		    (not (equal (char-after (point-at-bol)) ?*))))
	 (texp (equal (char-before beg) ?\\))
	 (link (equal (char-before beg) ?\[))
	 (opt (equal (buffer-substring (max (point-at-bol) (- beg 2))
				       beg)
		     "#+"))
	 (startup (string-match "^#\\+STARTUP:.*"
				(buffer-substring (point-at-bol) (point))))
	 (completion-ignore-case opt)
	 (type nil)
	 (tbl nil)
	 (table (cond
		 (opt
		  (setq type :opt)
		  (require 'org-exp)
		  (append
		   (mapcar
		    (lambda (x)
		      (string-match "^#\\+\\(\\([A-Z_]+:?\\).*\\)" x)
		      (cons (match-string 2 x) (match-string 1 x)))
		    (org-split-string (org-get-current-options) "\n"))
		   (mapcar 'list org-additional-option-like-keywords)))
		 (startup
		  (setq type :startup)
		  org-startup-options)
		 (link (append org-link-abbrev-alist-local
			       org-link-abbrev-alist))
		 (texp
		  (setq type :tex)
		  org-html-entities)
		 ((string-match "\\`\\*+[ \t]+\\'"
				(buffer-substring (point-at-bol) beg))
		  (setq type :todo)
		  (mapcar 'list org-todo-keywords-1))
		 (searchhead
		  (setq type :searchhead)
		  (save-excursion
		    (goto-char (point-min))
		    (while (re-search-forward org-todo-line-regexp nil t)
		      (push (list
			     (org-make-org-heading-search-string
			      (match-string 3) t))
			    tbl)))
		  tbl)
		 (tag (setq type :tag beg beg1)
		      (or org-tag-alist (org-get-buffer-tags)))
		 (prop (setq type :prop beg beg1)
		       (mapcar 'list (org-buffer-property-keys nil t t)))
		 (t (progn
		      (call-interactively org-completion-fallback-command)
		      (throw 'exit nil))))))
    (setq ac-org-pattern (buffer-substring-no-properties beg end))
    table))

(defvar ac-source-org nil)
(setq ac-source-org
      `((sigil . "o")
	(init . (lambda () (setq ac-org-candidates
				 (condition-case nil
				     (ac-org-construct-candidates)))))
	(candidates . (lambda ()
			(all-completions ac-target ac-org-candidates)))))

;;TaskJuggler
(setq org-export-taskjuggler-target-version 3.1)
;; :timezone: Europe/London
;; :dailyworkinghours: 8
;; :timeformat: %d-%m-%Y %H:%M
;; :weekstarts: monday
;; :workinghours: mon - fri 9:00 - 13:00, 14:00 - 18:00
;; :workinghours: sat, sun off

;;RevealJS
(require 'ox-reveal)
(setq org-reveal-root "file:///Users/anton/git_repos/reveal.js")

(require 'appt)
(setq appt-time-msg-list nil)    ;; clear existing appt list
(setq appt-display-interval '10) ;; warn every 10 minutes from t - appt-message-warning-time
(setq
 appt-message-warning-time '10  ;; send first warning 10 minutes before appointment
 appt-display-mode-line nil     ;; don't show in the modeline
 appt-display-format 'window)   ;; pass warnings to the designated window function
 (appt-activate 1)                ;; activate appointment notification
(display-time)                   ;; activate time display

;; Erase all reminders and rebuilt reminders for today from the agenda
(defun y_pe/org-agenda-to-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))
;; Rebuild the reminders everytime the agenda is displayed
(add-hook 'org-finalize-agenda-hook 'y_pe/org-agenda-to-appt 'append)
(run-at-time "24:01" 3600 'y_pe/org-agenda-to-appt) ;; update appt list hourly

;; set up the call to terminal-notifier
(defun my-appt-send-notification (title msg)
  (tn-notify msg title " "))
;; designate the window function for my-appt-send-notification
(defun my-appt-display (min-to-app new-time msg)
  (my-appt-send-notification 
   (format "'Appointment in %s minutes'" min-to-app)    ;; passed to -title in terminal-notifier call
   (format "'%s'" msg)))                                ;; passed to -message in terminal-notifier call
(setq appt-disp-window-function (function my-appt-display))

;; This is at the end of my .emacs - so appointments are set up when Emacs starts
(y_pe/org-agenda-to-appt)
;;Clock in default task (Daily Dose)
;;(y_pe/clock-in-default-task-as-default)
