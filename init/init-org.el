;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author: Anton Strilchuk <ype@env.sh>                             ;;
;; URL: http://ype.env.sh                                           ;;
;; Created: 06-06-2014                                              ;;
;; Last-Updated: 07-08-2014                                         ;;
;;   By: Anton Strilchuk <ype@env.sh>                               ;;
;;                                                                  ;;
;; Filename: init-org                                               ;;
;; Version: 0.0.1                                                   ;;
;; Description: Org-mode Configuration                              ;;
;;                                                                  ;;
;; Package Requires: ()                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;,---------
;;| PACKAGES
;;`---------

;; ELPA Check if Installed and Require
(require-package 'org)
(require-package 'org-plus-contrib)
(require-package 'htmlize)
(require-package 'google-maps)
(require-package 'org-pandoc)

;; Require Packages
(require 'org-src)
(require 'org-id)
(require 'org-location-google-maps)
(require 'org-habit)
(require 'ox-pandoc)

;;Give buffers in different major-modes
;;the 'look-and-feel' of Org-mode buffers
;;Org-style comment editing
(require-package 'outorg)
(require-package 'outshine)
(autoload 'outshine-hook-function "outshine")
(autoload 'outorg-edit-as-org "outorg")
(add-hook 'outline-minor-mode-hook 'outshine-hook-function)

(require 'org-collector)

;; Enable habit tracking (and a bunch of other modules)
(setq org-modules '(org-bbdb
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
                    org-vm
                    org-wl
                    org-w3m))

(org-babel-do-load-languages 'org-babel-load-languages
                             '((R . t)
                               (ditaa . t)
                               (dot . t)
                               (plantuml . t)
                               (emacs-lisp . t)
                               (clojure . t)
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

;; (setq org-plantuml-jar-path
;;       (expand-file-name "/Users/anton/.emacs.d/plantuml.jar"))

;;Save all org buffers 1 minute before every hour
(run-at-time "00:59" 3600 'org-save-all-org-buffers)


;;,-----------------------------------------------------------
;;| Let Emacs Find Org-files on multiple systems
;;| Sets different directory location depending on system-name
;;`-----------------------------------------------------------
(print system-name)
(when (or (string-equal system-name "bastard.local")
         (string-equal system-name "bastard.lan"))
  (setq org-directory "/Volumes/Ed/Dropbox/ORGS/"
        org-agenda-files '("/Volumes/Ed/Dropbox/ype/org-issues/"
                           "/Volumes/Ed/Dropbox/ORGS/gtd.org"
                           "/Volumes/Ed/Dropbox/ORGS/gcal.org"
                           ;; "/Volumes/Ed/Dropbox/ORGS/school/s_main.org"
                           "/Volumes/Ed/Dropbox/ORGS/refile.org"
                           )
        org-icalendar-combined-agenda-file (concat org-directory "combi.ics")
        org-archive-location (concat org-directory "archive/%s_archive::")
        org-default-notes-file (concat org-directory "gtd.org")))

(when (or (string-equal system-name "fennec.local")
         (string-equal system-name "fennec.lan"))
  (setq org-directory "~/Dropbox/ORGS/"
        org-agenda-files '("~/Dropbox/ORGS/gtd.org"
                           "~/Dropbox/ORGS/gcal.org"
                           "~/Dropbox/ype/org-issues/"
                           ;; "~/Dropbox/ORGS/school/s_main.org"
                           "~/Dropbox/ORGS/refile.org"
                           )
        org-icalendar-combined-agenda-file (concat org-directory "combi.ics")
        org-archive-location (concat org-directory "archive/%s_archive::")
        org-default-notes-file (concat org-directory "gtd.org")))


;;,------------------------
;;| Global File Preferences
;;`------------------------

(setq org-list-allow-alphabetical t
      org-support-shift-select t
      org-startup-folded t
      org-startup-indented t
      org-odd-levels-only t
      org-hide-leading-stars nil
      org-pretty-entities t
      org-enforce-todo-dependencies t
      org-clone-delete-id t
      org-return-follows-link t
      org-cycle-separator-lines 0
      org-insert-heading-respect-content nil)

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

;;,-------
;;| AGENDA
;;`-------

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
(setq org-startup-folded t)

;;LOGGING
(setq org-log-into-drawer t)
(setq org-log-state-notes-insert-after-drawers nil)
(setq org-log-done 'time)
(setq org-log-done 'note)
;;show notes at top
(setq org-reverse-note-order nil)
;; Agenda log mode items to display (closed and state changes by default)
(setq org-agenda-log-mode-items '(closed state))


;;,------
;;| CLOCK
;;`------

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

;; Change tasks quickly
;; this removes clocked tasks with 0:00 duration
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

;;(setq org-clock-sound "/Users/anton/Library/Sounds/Quack.aiff")

(setq org-time-stamp-rounding-minutes '(1 1))

(setq org-agenda-clock-consistency-checks
      '((:max-duration "4:00"
                       :min-duration 0
                       :max-gap 0
                       :gap-ok-around ("4:00"))))


;;,-----------------
;;| DEFAULT TASK IDs
;;`-----------------
(defvar ype/default-task-id "0CE8B026-E93A-4021-AD99-2ACECAA974DE")

(when (or (string-equal system-name "bastard.local")
         (string-equal system-name "bastard.lan"))
  (setq ype/default-task-id-and-dir
        (org-id-find-id-in-file ype/default-task-id "/Volumes/Ed/Dropbox/ORGS/gtd.org" 'marker)))

(when (or (string-equal system-name "fennec.local")
         (string-equal system-name "fennec.lan"))
  (setq ype/default-task-id-and-dir
        (org-id-find-id-in-file ype/default-task-id "~/Dropbox/ORGS/gtd.org" 'marker)))

(defvar ype/default-email-id "AC4B5743-4FDC-4886-9084-D2F1AA29047E")
(defvar ype/default-elisp-id "8FE1162A-C5E3-4B7C-B0B6-9B3EDED7B477")


(defun ype/clock-in-default-task-as-default ()
  (interactive)
  (org-with-point-at (print ype/default-task-id-and-dir)
    (org-clock-in '(16))))

(defun ype/clock-in-default-school ()
  (interactive)
  (org-with-point-at (org-id-find ype/default-school-id 'marker)
    (org-clock-in '(16))))

(defun ype/clock-in-default-email ()
  (interactive)
  (org-with-point-at (org-id-find ype/default-email-id 'marker)
    (org-clock-in '(16))))

(defun ype/clock-in-default-elisp ()
  (interactive)
  (org-with-point-at (org-id-find ype/default-elisp-id 'marker)
    (org-clock-in '(16))))

;;ORG Archive All Done Tasks
(defun ype/org-archive-done-tasks ()
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))


;;,----------
;;| iCalendar
;;`----------

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


;;,----------------
;;| REFILE Settings
;;`----------------

;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

;; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)

;; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

;; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t
      ido-everywhere t
      ido-max-directory-size 100000)
(ido-mode (quote both))

;; Use the current window when visiting files and buffers with ido
(setq ido-default-file-method 'selected-window
      ido-default-buffer-method 'selected-window)

;; Use the current window for indirect buffer display
(setq org-indirect-buffer-display 'current-window)


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

(setq org-global-properties
      '(("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
        ("STYLE_ALL" . "habit")))


;;,----------
;;| TODO Conf
;;`----------
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "REVIEW(r@/!)" "|" "DONE(d)" "WAITING(w@/!)") ;; NORMAL
        (sequence "BUG(B@/!)" "HOTFIX(H@/!)" "|" "FIXED(F@/!)" "STUCK(S@/!)")
        (sequence "OPEN(o)" "CLOSED(c)") ;; GitHub Issues
        (sequence "EVENT(e)" "|" "ATTENDED(a)" "SKIPPED(u)")
        (sequence "CALL(p)" "|" "CALLED(f)")
        (sequence "REPLY(R@/!)" "|" "REPLIED(s@/!)")
        (sequence "INVOICE(I@/!)" "PENDING(P@/!)" "|" "INVOICED(i@/!)" "CANCELLED(C@/!)")))

(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "#D26464" :weight bold))
        ("NEXT" . (:foreground "#D2BF64" :weight bold))
        ("REVIEW" . (:foreground "#8477AE" :weight bold))
        ("DONE" . (:foreground "#82CA82" :weight bold-italic))
        ("WAITING" . (:foreground "#6B8E23" :weight bold))
        ("BUG" . (:foreground "#990000" :weight bold))
        ("HOTFIX" . (:foreground "#663300" :weight bold))
        ("FIXED" . (:foreground "#336600" :weight bold))
        ("STUCK" . (:foreground "#FF0F00" :weight bold))
        ("OPEN" . (:foreground "#00FF7F" :weight bold))
        ("CLOSED" . (:foreground "#FF4500" :weight bold))
        ("EVENT" . (:foreground "#82CA82" :weight bold))
        ("ATTENDED" . (:foreground "#8477AE" :weight bold))
        ("SKIPPED" . (:foreground "#FDEEA3" :weight bold))
        ("CALL" . (:foreground "#C71585" :weight bold))
        ("CALLED" . (:foreground "#FF4500" :weight bold))
        ("REPLY" . (:foreground "#FFC0CB" :weight bold))
        ("REPLIED" . (:foreground "#B0C4DE" :weight bold))
        ("INVOICE" . (:foreground "#FFA07A" :weight bold))
        ("PENDING" . (:foreground "#FF6347" :weight bold))
        ("INVOICED" . (:foreground "#40E0D0" :weight bold))
        ("CANCELLED" . (:foreground "#FDA3A3" :weight bold))))

;; TAGS
(setq org-tag-alist '((:startgroup . nil)
                      ("@work" . ?W)
                      ("@home" . ?H)
                      ("@expense" . ?E)
                      (:endgroup . nil)
                      ("EVENT" . ?e)
                      ("HABIT" . ?h)
                      ("MEETING" . ?m)
                      ("REPLY" . ?r)
                      ("FLAGGED" . ?f)
                      ("ADMIN" . ?a)
                      ("NOTE" . ?n)
                      ("INVOICE" .?i)
                      ("HOMEROOM" . ?1)
                      ("CodaSign" . ?3)
                      ("CRYPT" . ?C)
                      ("PERSONAL" . ?p)
                      ("MAJ_PROJ" . ?j)))

(setq org-fast-tag-selection-single-key (quote expert))
(setq org-agenda-tags-todo-honor-ignore-options t)


;;,-------------
;;| Capture Conf
;;`-------------

;; ORG Capture Templates
;;  | %a        | annotation normally the link created with org-store-link      |
;;  | %A        | like %a but prompt for the description part                   |
;;  | %i        | initial content the region when remember is called with C-u.  |
;;  | %t        | timestamp date only                                           |
;;  | %T        | timestamp with date and time                                  |
;;  | %u %U     | like the above but inactive timestamps                        |
;;  | %^t       | like %t but prompt for date.                                  |
;;  | %n        | user name (taken from user-full-name)                         |
;;  | %c        | Current kill ring head.                                       |
;;  | %x        | Content of the X clipboard.                                   |
;;  | %^C       | Interactive selection of which kill or clip to use.           |
;;  | %^L       | Like %^C but insert as link.                                  |
;;  | %k        | title of the currently clocked task                           |
;;  | %K        | link to the currently clocked task                            |
;;  | %^g       | prompt for tags with completion on tags in target ﬁle.        |
;;  | %^G       | prompt for tags, with completion all tags in all agenda ﬁles. |
;;  | %^{prop}p | Prompt the user for a value for property prop %:keyword       |
;;  | %[file]   | insert the contents of the ﬁle given by ﬁle                   |
;;  | %(sexp)   | evaluate Elisp sexp and replace with the result               |
;;  | %!        | immediately store note after completing the template          |
;;  | %&        | jump to target location immediately after storing note        |

(setq org-capture-templates
      '(("t" "TODO" entry
         (file (concat org-directory "refile.org"))
         "* NEXT %^{Brief Description} %^G\nDEADLINE: %^t \n%? \nAdded: %U"
         :clock-in t :clock-resume t)

        ("m" "MEETING" entry
         (file (concat org-directory "refile.org"))
         "* MEETING %^{Brief Description} :MEETING:\n- WHO: %^{Who?}\n- WHAT: %^{What?}\n   SCHEDULED:%^t\n:PROPERTIES:\n:ADDRESS: %^{Where?}\n:END:\n"
         :clock-in t :clock-resume t)

        ("n" "NOTE" entry
         (file (concat org-directory "refile.org"))
         "* %? :NOTE:\n%U\n%a\n"
         :clock-in t :clock-resume t)

        ("r" "RESPOND" entry
         (file (concat org-directory "refile.org"))
         "* NEXT Respond to %^{Who's it From} about %^{What's it about?}\nSCHEDULED: %^t\n%U\n%a\n"
         :clock-in t :clock-resume t :immediate-finish t)

        ("h" "HABIT" entry
         (file (concat org-directory "refile.org"))
         "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"
         :clock-in t :clock-resume t)

        ("p" "PHONE" entry
         (file (concat org-directory "refile.org"))
         "* PHONE %? :PHONE:\n%U"
         :clock-in t :clock-resume t)

        ("e" "EXPENSE" entry
         (file (concat org-directory "refile.org"))
         "* PENDING %^{What are the items?} for %^{Who are the items for?}, %^{Which Project?} project %^G\nDEADLINE: %^t\n:PROPERTIES:\nCATEGORY: EXPENSE\n:\nADDED:%U\n:END:\n")

        ("j" "Journals")
        ("jp" "Project Journal" plain
         (file+datetree+prompt
          (concat org-directory "journals/projects.org"))
         "- *%^{Title}*\n\t- *Time Spent:* %^{Time spent}\n\t- *Working on:* %^{What have you been working on?}\n\t- *Additional Comments*\n\t- %? \n\t- *Created at:* %T")

        ("jd" "Code Journal" plain
         (file+datetree+prompt
          (concat org-directory "journal.org"))
         "* %^{Title}\n1. *What did you code*\n%^{What did you code?}\n2. *How long did you code for*\n%^{How long did you code for?}\n3. *Where did you code*\n%^{Where did you code?}\n4. *Why did you code this*\n%^{Why did you code this?}")
        ))
;;END Templates

;;,---------------------------
;;| ORG AGENDA CUSTOM COMMANDS
;;`---------------------------

(setq org-agenda-custom-commands
      '(("c" . "Custom queries")
        ("cd" "Upcoming deadlines" agenda ""
         ((org-agenda-entry-types '(:deadline))
          (org-agenda-ndays 1)
          (org-deadline-warning-days 60)
          (org-agenda-time-grid nil)))

        ("n" "Next Task" todo "TODO"
         ((org-agenda-overriding-header "Next Tasks")
          (org-agenda-overriding-columns-format "%55ITEM %35SCHEDULED %25DEADLINE")
          (org-agenda-view-columns-initially t)
          (org-agenda-todo-ignore-scheduled nil)
          (org-agenda-todo-ignore-deadlines nil)
          (org-agenda-todo-ignore-with-date nil)
          (org-tags-match-list-sublevels t)
          (org-agenda-skip-function '(org-agenda-skip-entry-if 'regexp ":HABIT:"))
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

;;,------
;;| DIARY
;;`------

(setq org-agenda-diary-file (concat org-directory "personal/diary")
      org-agenda-insert-diary-extract-time t)

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

;;,----------
;;| AUTOFILLS
;;`----------

(add-hook 'text-mode-hook 'turn-on-auto-fill)


;;,---------------------------------------
;;| CUSTOM Functions by Bernt Hansen (bh/)
;;`---------------------------------------

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

(setq org-export-with-timestamps nil)

;;,------------------
;;| ORG AUTO-COMPLETE
;;`------------------

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
          (when
              (and (member (char-before beg1) '(?. ?<))
                 (setq a (assoc
                          (buffer-substring beg1 (point))
                          org-structure-template-alist)))
            (org-complete-expand-structure-template (1- beg1) a)
            (throw 'exit t)))

         (tag (and (equal (char-before beg1) ?:)
                 (equal (char-after (point-at-bol)) ?*)))

         (prop (and (equal (char-before beg1) ?:)
                  (not (equal (char-after (point-at-bol)) ?*))))

         (texp (equal (char-before beg) ?\\))
         (link (equal (char-before beg) ?\[))
         (opt (equal
               (buffer-substring
                (max (point-at-bol) (- beg 2)) beg) "#+"))

         (startup
          (string-match "^#\\+STARTUP:.*"
                        (buffer-substring (point-at-bol) (point))))

         (completion-ignore-case opt)
         (type nil)
         (tbl nil)
         (table
          (cond
           (opt (setq type :opt)
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

           (texp (setq type :tex)
                 org-html-entities)
           ((string-match "\\`\\*+[ \t]+\\'"
                          (buffer-substring (point-at-bol) beg))
            (setq type :todo)
            (mapcar 'list org-todo-keywords-1))

           (searchhead
            (setq type :searchhead)
            (save-excursion
              (goto-char (point-min))
              (while
                  (re-search-forward org-todo-line-regexp nil t)
                (push
                 (list
                  (org-make-org-heading-search-string
                   (match-string 3) t)) tbl))) tbl)

           (tag (setq type :tag beg beg1)
                (or org-tag-alist (org-get-buffer-tags)))

           (prop (setq type :prop beg beg1)
                 (mapcar 'list (org-buffer-property-keys nil t t)))

           (t (progn
                (call-interactively org-completion-fallback-command)
                (throw 'exit nil)))
           )))
    (setq ac-org-pattern
          (buffer-substring-no-properties beg end)) table))

(defvar ac-source-org nil)
(setq ac-source-org
      `((sigil . "o")
        (init . (lambda ()
                  (setq ac-org-candidates
                        (condition-case nil
                            (ac-org-construct-candidates)))))
        (candidates . (lambda ()
                        (all-completions ac-target ac-org-candidates)))))

;;,---------
;;| ORG GCAL
;;`---------
;; NOTE: ORG-GCAL Needs the occasional refresh, use org-gcal-refresh-token

(require-package 'org-gcal)
(require-git-package 'tkf/emacs-request)
(require-git-package 'jwiegley/alert)
(require 'org-gcal)

;; Client ID and Secret kept in seperate file: init-keys.el
;; (after-load 'org-gcal
;;   (setq org-gcal-client-id "long numbering thing here"
;;         org-gcal-client-secret "super secret key"))

(when (or (string-equal system-name "bastard.local")
         (string-equal system-name "bastard.lan"))
  (setq org-gcal-file-alist '(("anton@ilyfa.cc" .  "/Volumes/Ed/Dropbox/ORGS/gcal.org"))))

(when (or (string-equal system-name "fennec.local")
         (string-equal system-name "fennec.lan"))
  (setq org-gcal-file-alist '(("anton@ilyfa.cc" .  "~/Dropbox/ORGS/gcal.org"))))

(run-at-time "30 min" 3600 'org-gcal-fetch)

;;,------------
;;| TaskJuggler
;;`------------

(setq org-export-taskjuggler-target-version 3.1)
;; :timezone: Europe/London
;; :dailyworkinghours: 8
;; :timeformat: %d-%m-%Y %H:%M
;; :weekstarts: monday
;; :workinghours: mon - fri 9:00 - 13:00, 14:00 - 18:00
;; :workinghours: sat, sun off

;;,---------
;;| RevealJS
;;`---------

(require-package 'ox-reveal)
(setq org-reveal-root "file:///Users/anton/git_repos/reveal.js")

;;,-----------------------
;;| APPT and Notifications
;;`-----------------------

(require 'appt)
(setq appt-time-msg-list nil           ;; clear existing appt list
      appt-display-interval '5       ;; warn every 10 minutes from t - appt-message-warning-time
      appt-message-warning-time '60  ;; send first warning 10 minutes before appointment
      appt-display-mode-line nil      ;; don't show in the modeline
      appt-display-format 'window)  ;; pass warnings to the designated window function

(appt-activate 1) ;; activate appointment notification
(display-time)    ;; activate time display

;; Erase all reminders and rebuilt reminders for today from the agenda
(defun ype/org-agenda-to-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

;; Rebuild the reminders everytime the agenda is displayed
(add-hook 'org-finalize-agenda-hook 'ype/org-agenda-to-appt 'append)
(run-at-time "24:01" nil 'ype/org-agenda-to-appt) ;; update appt list hourly

;;,-------------------------------------
;;| TERMINAL NOTIFIER
;;`-------------------------------------

;; set up the call to terminal-notifier
(defun my-appt-send-notification (title msg)
  (tn-notify msg title " "))

;; designate the window function for my-appt-send-notification
(defun my-appt-display (min-to-app new-time msg)
  (my-appt-send-notification
   (format "'Appointment in %s minutes'" min-to-app) ;; passed to -title in terminal-notifier call
   (format "'%s'" msg))) ;; passed to -message in terminal-notifier call

(setq appt-disp-window-function (function my-appt-display))

;; This is at the end of my .emacs - so appointments are set up when Emacs starts
(ype/org-agenda-to-appt)

;;,-------------
;;| Org Mac Link
;;`-------------
(require-package 'org-mac-link)
(require 'org-mac-link)
(define-key org-mode-map (kbd "C-'") 'org-mac-grab-link)


(provide 'init-org)
;;; init-org.el ends here
