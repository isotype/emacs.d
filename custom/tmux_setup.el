;; -------------------------------------------------------------------- ;;
;; ; tmux_setup.el ---                                                  ;;
;;                                                                      ;;
;;  Filename: tmux_setup.el                                             ;;
;;  Description:                                                        ;;
;;  Author: y_pe                                                        ;;
;;  Maintainer:                                                         ;;
;;  Created: Fri Feb 14 08:27:23 2014 (+0000)                           ;;
;;  Version: 0.01                                                       ;;
;;  Package-Requires: ()                                                ;;
;;  Last-Updated: Fri Feb 14 08:31:39 2014 (+0000)
;;            By: anton
;;      Update #: 1                                                     ;;
;;  URL: isoty.pe                                                       ;;
;;  Doc URL: isoty.pe                                                   ;;
;;  Keywords: tmux, emacs, lldb, xcode, gdb                             ;;
;;  Tested on: Emacs 24.3.50.1                                          ;;
;;                                                                      ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                      ;;
;; ; Commentary:                                                        ;;
;;   Method for sending messages directly from emacs to tmux            ;;
;;                                                                      ;;
;;                                                                      ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                      ;;
;; ; Change Log:                                                        ;;
;;   1. initial commit                                                  ;;
;;                                                                      ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                      ;;
;;  This program is free software; you can redistribute it and/or       ;;
;;  modify it under the terms of the GNU General Public License as      ;;
;;  published by the Free Software Foundation; either version 3, or     ;;
;;  (at your option) any later version.                                 ;;
;;                                                                      ;;
;;  This program is distributed in the hope that it will be useful,     ;;
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of      ;;
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU   ;;
;;  General Public License for more details.                            ;;
;;                                                                      ;;
;;  You should have received a copy of the GNU General Public License   ;;
;;  along with this program; see the file COPYING.  If not, write to    ;;
;;  the Free Software Foundation, Inc., 51 Franklin Street, Fifth       ;;
;;  Floor, Boston, MA 02110-1301, USA.                                  ;;
;;                                                                      ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                      ;;
;; -------------------------------------------------------------------- ;;
;; Code:
(setq tmux-session-name 1)
(setq tmux-window-name 0)
(setq tmux-pane-number 0)

(setq lldb-session-name 'lldb)
(setq lldb-window-name 0)
(setq lldb-pane-number 0)

(defun y_pe/lldb-tmux-setup (lldb-window lldb-pane lldb-debug)
  "Setup global variables for tmux session: lldb - set window, pane, and debug path"
  (interactive "sEnter tmux LLDB window name: \nsEnter pane number: \nsEnter debug path: ")
  (setq lldb-window-name lldb-window)
  (setq lldb-pane-number lldb-pane)
  (setq lldb-debug-path lldb-debug)
  (message "LLDB Setup, session name: %s, window name: %s, pane number: %s, debug path: %s" lldb-session-name lldb-window-name lldb-pane-number lldb-debug-path))

(defun y_pe/lldb-tmux-start ()
  (interactive)
  (shell-command
   (format "tmux send-keys -t %s:%s.%s 'lldb %s' Enter" lldb-session-name lldb-window-name lldb-pane-number lldb-debug-path)))

(defun y_pelldb-tmux-run ()
  "Sends run command to tmux-lldb"
  (interactive)
  (shell-command
   (format "tmux send-keys -t %s:%s.%s 'C-c' Enter" lldb-session-name lldb-window-name lldb-pane-number))
  (sleep-for 2)
  (shell-command
   (format "tmux send-keys -t %s:%s.%s 'run' Enter" lldb-session-name lldb-window-name lldb-pane-number))
  (sleep-for 2)
  (shell-command
   (format "tmux send-keys -t %s:%s.%s 'Y' Enter" lldb-session-name lldb-window-name lldb-pane-number)))

(defun y_pe/lldb-tmux-send (lldb-command)
  (interactive "slldb: ")
  (shell-command
   (format "tmux send-keys -t %s:%s.%s '%s' Enter" lldb-session-name lldb-window-name lldb-pane-number lldb-command)))

(defun y_pe/tmux-setup (session window number)
  "Setup global variables for tmux session, window, and pane"
  (interactive "sEnter tmux session name: \nsEnter tmux window name: \nsEnter tmux pane number: ")
  (setq tmux-session-name session)
  (setq tmux-window-name window)
  (setq tmux-pane-number number)
  (message "Tmux Setup, session name: %s, window name: %s, pane number: %s" tmux-session-name tmux-window-name tmux-pane-number))

(defun y_pe/tmux-any-send (command)
  (interactive "srun: ")
  (shell-command
   (format "tmux send-keys -t %s:%s.%s '%s' Enter" tmux-session-name tmux-window-name tmux-pane-number command)))

(defun y_pe/xcodebuild ()
  (interactive)
  ;; (setq xcodebuild-command (concat "echo "
  ;; 				 "\"Time: $(date)\\n\" "
  ;; 				 "\> build_tail.log; "
  ;; 				 "xcodebuild -configuration Debug | egrep -A 5 "
  ;; 				 "\"(error|warning):\" "
  ;; 				 ">> build_tail.log"))
  (setq xcodebuild-command (concat "xcodebuild -configuration Debug"))
  (shell-command
   (format "tmux send-keys -t %s:%s.%s '%s' Enter" tmux-session-name tmux-window-name tmux-pane-number xcodebuild-command)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tmux_setup.el ends here
