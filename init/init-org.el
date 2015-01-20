;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author: Anton Strilchuk <ype@env.sh>                             ;;
;; URL: http://ype.env.sh                                           ;;
;; Created: 06-06-2014                                              ;;
;;; Last-Updated: 16-01-2015                                       ;;;
;;;   By: Anton Strilchuk <anton@env.sh>                           ;;;
;;                                                                  ;;
;; Filename: init-org                                               ;;
;; Version: 0.0.1                                                   ;;
;; Description: Org-mode Configuration                              ;;
;;                                                                  ;;
;; Package Requires: ()                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init-org)

;;-------------------
;; PACKAGES
;;-------------------
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

(setq org-plantuml-jar-path
      (expand-file-name "/usr/local/Cellar/plantuml/8002/plantuml.8002.jar"))


;;,-----------------------------------------------------------
;;| Let Emacs Find Org-files on multiple systems
;;| Sets different directory location depending on system-name
;;`-----------------------------------------------------------

;; (when (or (string-equal system-name "fennec.local")
;;           (string-equal system-name "fennec.lan")))
(setq org-directory "~/Dropbox/ORGS/"
      org-agenda-files '("~/Dropbox/ORGS/gtd.org"
                         "~/Dropbox/ORGS/gcal.org"
                         "~/Dropbox/ORGS/refile.org"
                         "~/Dropbox/org-project-todos/projects.org")
      org-archive-location (concat org-directory "archive/%s_archive::")
      org-default-notes-file (concat org-directory "refile.org"))


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
      org-insert-heading-respect-content nil
      org-treat-S-cursor-todo-selection-as-state-change nil)

;;Use System Settings For File-Application Selection
(setq org-file-apps (quote ((auto-mode . emacs)
                            ("\\.mm\\'" . system)
                            ("\\.x?html?\\'" . system)
                            ("\\.pdf\\'" . system))))

;;UNIQUE IDs for TASKS
(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

;;If Priority needed
;;Priorities A-E where tasks without a specific priority are lowest priority E
(setq org-enable-priority-commands t
      org-default-priority ?E
      org-lowest-priority ?E)

(setq org-habit-graph-column 50)
(run-at-time "06:00" 86400 (lambda () (setq org-habit-show-habits t)))

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

(setq org-agenda-span 'day                               ;;Show Today in Agenda
      org-agenda-sticky t
      org-agenda-include-diary nil
      org-agenda-show-all-dates t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-start-on-weekday 1                      ;; Start the weekly agenda on Monday
      org-deadline-warning-days 30
      org-agenda-persistent-filter t
      org-agenda-skip-additional-timestamps-same-entry t ;;Remove Multiple State Change Log Details From The Agenda
      org-agenda-repeating-timestamp-show-all t          ;; Show all future entries for repeating tasks
      org-agenda-show-all-dates t)                       ;; Show all agenda dates - even if they are empty

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


(setq org-agenda-text-search-extra-files '((agenda-archives)) ;;Include Archive in Search
      org-agenda-dim-blocked-tasks nil       ;; Do not dim blocked tasks
      org-agenda-compact-blocks t          ;; Compact the block agenda view
      org-agenda-todo-ignore-with-date nil   ;; Keep tasks with dates on the global todo lists
      org-agenda-todo-ignore-deadlines nil   ;; Keep tasks with deadlines on the global todo lists
      org-agenda-todo-ignore-scheduled nil   ;; Keep tasks with scheduled dates on the global todo lists
      org-agenda-todo-ignore-timestamp nil   ;; Keep tasks with timestamps on the global todo lists
      org-agenda-skip-deadline-if-done t   ;; Remove completed deadline tasks from the agenda view
      org-agenda-skip-scheduled-if-done t  ;; Remove completed scheduled tasks from the agenda view
      org-agenda-skip-timestamp-if-done t) ;; Remove completed items from search results



(setq org-id-method 'uuid                          ;;UIDs
      org-show-following-heading t                 ;;SEARCH
      org-show-hierarchy-above t                   ;;SEARCH
      org-show-siblings '(default)                 ;;SEARCH
      org-startup-align-all-tables t               ;;#+STARTUP
      org-startup-folded t                         ;;#+STARTUP
      org-log-into-drawer t                        ;;LOGGING
      org-log-state-notes-insert-after-drawers nil   ;;LOGGING
      org-log-done 'time                           ;;LOGGING
      org-log-done 'note                           ;;LOGGING
      org-reverse-note-order nil                     ;;show notes at top
      org-agenda-log-mode-items '(closed state))


;;,------
;;| CLOCK
;;`------

;; [[https://github.com/lolownia/org-pomodoro][Org Pomodoro]]
(require-package 'org-pomodoro)

;;ORG CLOCK IDLE
(setq org-clock-idle-time 2)
(org-clock-persistence-insinuate)

(setq org-clock-history-length 23                               ;; clocking history lenth
      org-clock-in-resume t                                     ;; Resume clocking task on clock-in
      org-drawers '("PROPERTIES" "LOGBOOK")                     ;; Separate drawers for clocking and logs
      org-clock-into-drawer t                                   ;; Save clock data, state changes, notes
      org-clock-out-remove-zero-time-clocks t                   ;; removes 0:00 duration
      org-clock-out-when-done t                                 ;; Clock out when state DONE
      org-clock-persist 'history                                ;; Save the running clock & all history on exit
      org-clock-persist-query-resume nil                          ;; Do not prompt resume clock
      org-clock-auto-clock-resolution 'when-no-clock-is-running ;; Enable auto clock for finding open clocks
      org-clock-report-include-clocking-task t                  ;; Current clocking task in clock reports
      org-clock-sound "/Users/anton/Library/Sounds/Timer.wav")  ;; make some noise when over task time

(setq org-time-stamp-rounding-minutes '(1 1))

(setq org-agenda-clock-consistency-checks
      '((:max-duration "4:00"
                       :min-duration 0
                       :max-gap 0
                       :gap-ok-around ("4:00"))))


;;,-----------------
;;| DEFAULT TASK IDs
;;`-----------------
(defvar ype/default-task-id "deftask0001")
(defvar ype/default-email-id "AC4B5743-4FDC-4886-9084-D2F1AA29047E")
(defvar ype/default-elisp-id "8FE1162A-C5E3-4B7C-B0B6-9B3EDED7B477")



(defun ype/clock-in-default-task-as-default ()
  (interactive)
  (org-with-point-at (org-id-find ype/default-task-id 'marker)
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
;; (setq org-icalendar-combined-name "GTD"
;;       org-icalendar-include-todo t
;;       org-icalendar-include-body t
;;       org-icalendar-categories '(todo-state all-tags category)
;;       org-icalendar-exclude-tags '("HABIT" "IGNORE")
;;       org-icalendar-store-UID t
;;       org-icalendar-alarm-time 30
;;       org-icalendar-use-scheduled '(todo-start event-if-todo)
;;       org-icalendar-use-deadline '(todo-due event-if-todo)
;;       org-icalendar-use-plain-timestamp nil
;;       org-icalendar-timezone "Europe/London")


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
      '((sequence "TODO(t)" "NEXT(n)" "REVIEW(r@/!)" "|" "DONE(d)" "WAITING(w@/!)" "KILLED(K@/!)")
        (sequence "BUG(B@/!)" "HOTFIX(H@/!)" "|" "FIXED(F@/!)" "STUCK(S@/!)")
        (sequence "OPEN(o)" "CLOSED(c)") ;; GitHub Issues
        (sequence "EVENT(e)" "|" "WENT(a)" "SKIPPED(u)")
        (sequence "CALL(k)" "|" "CALLED(l)")
        (sequence "REPLY(R@/!)" "|" "SENT(s@/!)")
        (sequence "INVOICE(i@/!)" "PENDING(p@/!)" "|" "FILED(f@/!)")))

(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "#a6ff99" :weight bold))
        ("NEXT" . (:foreground "#ff999f" :weight bold))
        ("REVIEW" . (:foreground "#ffff99" :weight bold))
        ("DONE" . (:foreground "#99b6ff" :weight bold))
        ("WAITING" . (:foreground "#ffce99" :weight bold))
        ("KILLED" . (:foreground "#c3c3c3" :weight bold))
        ("BUG" . (:foreground "#990000" :weight bold))
        ("HOTFIX" . (:foreground "#663300" :weight bold))
        ("FIXED" . (:foreground "#336600" :weight bold))
        ("STUCK" . (:foreground "#FF0F00" :weight bold))
        ("OPEN" . (:foreground "#00FF7F" :weight bold))
        ("CLOSED" . (:foreground "#FF4500" :weight bold))
        ("EVENT" . (:foreground "#82CA82" :weight bold))
        ("WENT" . (:foreground "#8477AE" :weight bold))
        ("SKIPPED" . (:foreground "#FDEEA3" :weight bold))
        ("CALL" . (:foreground "#C71585" :weight bold))
        ("CALLED" . (:foreground "#FF4500" :weight bold))
        ("REPLY" . (:foreground "#FFC0CB" :weight bold))
        ("SENT" . (:foreground "#B0C4DE" :weight bold))
        ("INVOICE" . (:foreground "#FFA07A" :weight bold))
        ("PENDING" . (:foreground "#FF6347" :weight bold))
        ("FILED" . (:foreground "#40E0D0" :weight bold))))

;; TAGS
(setq org-tag-alist '((:startgroup . nil)
                      ("@work" . ?q)
                      (:grouptags . nil)
                      ("admin" . ?w)
                      ("past" . ?e)
                      ("upcoming" . ?r)
                      ("meeting" . ?t)
                      ("flagged" . ?y)
                      ("invoice" .?u)
                      ("needs_reply" . ?i)
                      (:endgroup . nil)

                      (:startgroup . nil)
                      ("@home" . ?a)
                      (:grouptags . nil)
                      ("note" . ?s)
                      ("ledger" . ?d)
                      ("health" . ?f)
                      ("important" . ?g)
                      ("URGENT" . ?h)
                      (:endgroup . nil)

                      (:startgroup . nil)
                      ("@habit" . ?z)
                      (:grouptags . nil)
                      ("notebook" . ?x)
                      ("maths" . ?c)
                      ("reading" . ?v)
                      ("life" . ?b)
                      (:endgroup . nil)

                      (:startgroup . nil)
                      ("@gcal" . nil)
                      (:endgroup . nil)
                      ))

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
      '(("n" "Next Task" todo "NEXT"
         ((org-agenda-files '("~/Dropbox/ORGS/gtd.org"))
          (org-agenda-overriding-header "Next Tasks")
          (org-agenda-overriding-columns-format "%55ITEM %35SCHEDULED %25DEADLINE")
          (org-agenda-view-columns-initially t)
          (org-agenda-todo-ignore-scheduled nil)
          (org-agenda-todo-ignore-deadlines nil)
          (org-agenda-todo-ignore-with-date nil)
          (org-tags-match-list-sublevels t)
          (org-agenda-skip-function '(org-agenda-skip-entry-if 'regexp "\\(:habit:\\)"))
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
        ("w" "Waiting Tasks" todo "WAITING"
         ((org-agenda-overriding-header "Waiting")
          (org-tags-match-list-sublevels nil)
          (org-agenda-sorting-strategy '(priority-down))))
        ("g" "Google Calendar" alltodo ""
         ((org-agenda-files '("~/Dropbox/ORGS/gcal.org"))
          (org-agenda-ndays 31)
          (org-agenda-time-grid nil)))
        ("r" "Tasks to Refile" tags "@refile"
         ((org-agenda-files '("~/Dropbox/ORGS/gcal.org" "~/Dropbox/ORGS/refile.org"))
          (org-agenda-overriding-header "Tasks to Refile")
          (org-tags-match-list-sublevels nil)))
        ("c" . "Custom queries")
        ("cd" "Upcoming deadlines" agenda ""
         ((org-agenda-entry-types '(:deadline))
          (org-agenda-ndays 1)
          (org-deadline-warning-days 60)
          (org-agenda-time-grid nil)))
        ("cx" "With deadline columns" todo "TODO"
         ((org-agenda-overriding-columns-format "%20ITEM %DEADLINE")
          (org-agenda-view-columns-initially t)))
        ("cc" "ALL WORK"
         ((tags-todo "@work"))
         ((org-agenda-compact-blocks t)))
        ("ce" "ALL HABITS"
         ((tags-todo "@habits"))
         ((org-agenda-compact-blocks t)))
        ("cr" "ALL HOME"
         ((tags-todo "@home"))
         ((org-agenda-compact-blocks t)))
        ("cn" "ALL TASKS" todo "NEXT"
         ((org-agenda-todo-ignore-scheduled nil)
          (org-agenda-todo-ignore-deadlines nil)
          (org-agenda-todo-ignore-with-date nil)))
        ("ca" "Notes" tags "notes"
         ((org-agenda-overriding-header "Notes")
          (org-tags-match-list-sublevels t)))
        ("ch" "Daily habits"
         ((agenda ""))
         ((org-agenda-show-log t)
          (org-agenda-ndays 7)
          (org-agenda-log-mode-items '(state))
          (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp ":habit:"))))))


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


;;Org Structure Blocks
(add-to-list 'org-structure-template-alist '("cp" "#+BEGIN_SRC python :results output both :session\n?\n#+END_SRC" "<src lang=\"python\">\n?\n</src>"))
(add-to-list 'org-structure-template-alist '("eq" "\\begin{equation}\n?\n\\end{equation}"))
(add-to-list 'org-structure-template-alist '("eqa" "\\begin{equation}\n \\begin{array}{}\n?\n \\end{array}\n\\end{equation}"))


;;
;; ;;,---------
;; ;;| ORG GCAL
;;`---------
;; NOTE: ORG-GCAL Needs the occasional refresh, use org-gcal-refresh-token
(require-package 'org-gcal)
(el-get-bundle tkf/emacs-request)
(require 'org-gcal)
;; Client ID and Secret kept in seperate file: init-keys.el
;; (after-load 'org-gcal
;;   (setq org-gcal-client-id "long numbering thing here"
;;         org-gcal-client-secret "super secret key"))

;; (when (or (string-equal system-name "fennec.local")
;;           (string-equal system-name "fennec.lan")))
(setq org-gcal-file-alist '(("anton@ilyfa.cc" .  "~/Dropbox/ORGS/gcal.org")))
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
;; DISABLE: Use Seldom
;; (require-package 'ox-reveal)
;; (setq org-reveal-root "file:///Users/anton/git_repos/reveal.js")

;;,-----------------------
;;| APPT and Notifications
;;`-----------------------
;; DISABLE: in favour of iCal Notification
;; (require 'appt)
;; (setq appt-time-msg-list nil           ;; clear existing appt list
;;       appt-display-interval '3       ;; warn every 3 minutes from t - appt-message-warning-time
;;       appt-message-warning-time '10  ;; send first warning 10 minutes before appointment
;;       appt-display-mode-line nil      ;; don't show in the modeline
;;       appt-display-format 'window)  ;; pass warnings to the designated window function

;; (appt-activate 1) ;; activate appointment notification
;; (display-time)    ;; activate time display

;; ;; ;; Erase all reminders and rebuilt reminders for today from the agenda
;; (defun ype/org-agenda-to-appt ()
;;   (interactive)
;;   (setq appt-time-msg-list nil)
;;   (org-agenda-to-appt))

;; ;; Rebuild the reminders everytime the agenda is displayed
;; (add-hook 'org-finalize-agenda-hook 'ype/org-agenda-to-appt 'append)
;; (run-at-time "24:01" 3600 'ype/org-agenda-to-appt) ;; update appt list hourly

;; ;;,-------------------------------------
;; ;;| TERMINAL NOTIFIER
;; ;;`-------------------------------------
;; ;; set up the call to terminal-notifier
;; (defun my-appt-send-notification (title msg)
;;   (tn-notify msg title " "))

;; ;; designate the window function for my-appt-send-notification
;; (defun my-appt-display (min-to-app new-time msg)
;;   (my-appt-send-notification
;;    (format "'Appointment in %s minutes'" min-to-app) ;; passed to -title in terminal-notifier call
;;    (format "'%s'" msg))) ;; passed to -message in terminal-notifier call

;; (setq appt-disp-window-function (function my-appt-display))

;; ;; This is at the end of my .emacs - so appointments are set up when Emacs starts
;; (org-agenda-to-appt)

;;,-------------
;;| Org Mac Link
;;`-------------
(require-package 'org-mac-link)
(require 'org-mac-link)
(define-key ctrl-apos (kbd "a") 'org-mac-grab-link)

;;,----------------
;;| Org Box
;;| [[https://github.com/yasuhito/orgbox][yasuhito/orgbox]]
;;`----------------
(require-package 'orgbox)
(require 'orgbox)


;; ;;,---------
;; ;;| Org OPML
;; ;;`---------
;; ;; FIXME:
;; (add-to-list 'load-path (expand-file-name "submodules/org-opml" user-emacs-directory))
;; (load-library "org-opml")
;; (require 'ox-opml)

;;,----------
;;| Org Drill
;;`----------
(require 'org-drill)
(setq org-drill-use-visible-cloze-face-p t
      org-drill-maximum-items-per-session 20
      org-drill-maximum-duration 10
      org-drill-scope `("~/Dropbox/ORGS/drill.org")
      org-drill-save-buffers-after-drill-sessions-p nil
      org-drill-spaced-repetition-algorithm 'sm5
      org-drill-add-random-noise-to-intervals-p t
      org-drill-adjust-intervals-for-early-and-late-repetitions-p t)

(defun ype/ask-for-drill ()
  (interactive)
  (if (y-or-n-p "Drill me?")
      (progn
        (org-drill))
    (progn
      ;;FIXME: How do you set a named timer?
      (if (y-or-n-p "Want me to ask you again in 20 mins?")
          (progn
            (message "Okay! I'll ask you again in 20 minutes"))
        (progn
          (cancel-timer 'ype/ask-for-drill)))
      )))
;;(run-at-time "20 min" 1800 'ype/ask-for-drill)

;; Jump to Org Agenda when Idle
(defun jump-to-org-agenda ()
  (interactive)
  (let ((buf (get-buffer "*Org Agenda*"))
        wind)
    (if buf
        (if (setq wind (get-buffer-window buf))
            (select-window wind)
          (if (called-interactively-p 'any)
              (progn
                (select-window (display-buffer buf t t))
                (org-fit-window-to-buffer)
                ;; (org-agenda-redo)
                )
            (with-selected-window (display-buffer buf)
              (org-fit-window-to-buffer)
              ;; (org-agenda-redo)
              )))
      (call-interactively 'org-agenda-list))))

(run-with-idle-timer 300 t 'jump-to-org-agenda)

;; Send to bottom of list
(defun ype:org-send-to-bottom-of-list ()
  "Send the current line to the bottom of the list."
  (interactive)
  (beginning-of-line)
  (let ((kill-whole-line t))
    (save-excursion
      (kill-line 1)
      (org-end-of-item-list)
      (yank))))

;;Save all org buffers 1 minute before every hour
(run-at-time "00:59" 3600 'org-save-all-org-buffers)

;;Disable C-\' Key
(define-key org-mode-map (kbd "C-\'") nil)
(define-key global-map (kbd "H-a") 'org-agenda)
(define-key global-map (kbd "H-i") 'org-pomodoro)
(define-key global-map (kbd "H-I") 'org-clock-in)
(define-key global-map (kbd "H-o") 'org-clock-out)
(define-key global-map (kbd "H-f") 'org-refile)
(define-key global-map (kbd "H-c l") 'org-store-link)
(define-key global-map (kbd "H-c i") 'org-insert-link)
(define-key global-map (kbd "H-c a") 'org-archive-set-tag)
(define-key global-map (kbd "H-c o") 'org-refile-goto-last-stored)
(define-key global-map (kbd "H-c c") 'org-capture)
(define-key global-map (kbd "H-c p") 'ype/phone-call)
(define-key global-map (kbd "H-c d") 'ype/clock-in-default-task-as-default)
(define-key global-map (kbd "H-c m") 'ype/clock-in-default-email)
(define-key global-map (kbd "H-c E") 'ype/clock-in-default-elisp)
(define-key global-map (kbd "H-c s") 'ype/clock-in-default-school)
(define-key global-map (kbd "H-c t") 'org-clock-select-task)
(define-key global-map (kbd "H-c e") 'org-set-effort)

;; Experimental
;; [[https://github.com/tmarble/timesheet.el][tmarble/timesheet.el]]
(require-package 'timesheet)

;; [[https://github.com/tbanel/orgaggregate][tbanel/orgaggregate]]
(require-package 'orgtbl-aggregate)

;; Time Clocks
(el-get-bundle roman/clocker.el)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
