;;; y_pe-w3m.el --- 
;; 
;; Filename: y_pe-w3m.el
;; Description: Emacs Setup for w3m
;; Author: y_pe
;; Maintainer: y_pe
;; Created: Fri Feb 21 09:22:22 2014 (+0000)
;; Version: 0.1
;; Package-Requires: (w3m)
;; Last-Updated: Wed Mar  5 09:37:25 2014 (+0000)
;;           By: anton
;;     Update #: 13
;; URL: isoty.pe
;; Doc URL: isoty.pe
;; Keywords: w3m, emacs
;; Compatibility: tested on Emacs 24.3.50.1, and w3m 0.5.3
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;;  This is a marginally changed version of beatofthegeek's
;;  "My Setup for Using Emacs as Web Browser" post. 
;;  
;;  To use w3m in Emacs you first must have w3m, which can be
;;  aquired via homebrew (OSX), apt-get (Debian), or yum (Fedora)
;;
;;  HOMEBREW: brew install w3m
;;  APT-GET: sudo apt-get install w3m
;;  YUM: sudo yum install w3m
;;  PACMAN: pacman -S w3m
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;;  - Added conkeror/0.9.3 user-agent
;;  - Added OSX and character bug fixes
;;  - Quick link ace-jump using "f" key
;;  - Quick open url
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:
(require 'w3m)

;;Open new URL in tab
(setq browse-url-browser-function 'w3m-goto-url-new-session)

;;Use Conkeror user agent
(setq w3m-user-agent "Mozilla/5.0 (X11; Linux x86_64; rv:6.0.1) Gecko/20110831 conkeror/0.9.3")

;; Mac OSX fix
(if (string= system-type "darwin")
     (setq process-connection-type nil))

;;Quick ace-jump links with "f" key
(w3m-lnum-mode +1)

;;Enable w3m cookies
(setq w3m-use-cookies t)

;; Fix word character problems
(setq w3m-coding-system 'utf-8
          w3m-file-coding-system 'utf-8
          w3m-file-name-coding-system 'utf-8
          w3m-input-coding-system 'utf-8
          w3m-output-coding-system 'utf-8
          w3m-terminal-coding-system 'utf-8)

;; Fix evil bullet character that display as \225
(standard-display-ascii ?\225 [?+])

;; Function needs work - haven't had time to finish
;; (defun y_pe/w3m-site-jump (picksite)
;;   "Incomplete, didn't have time to finish, more urgent matters
;;    like actual work for example...basically this is supposed to
;;    function that allows me to quickly select from a list of options
;;    using IDO websites that I regularly visit."
;;   (interactive)
;;   (setq list-websites '("http://news.ycombinator.com"
;; 			"http://github.com"
;; 			"http://www.quicklisp.org/beta/releases.html")))

;;Quick open site
(defun y_pe/qsite (site)
  "Opens site in new w3m session with 'http://' appended"
  (interactive
   (list (read-string "Enter website address(default: w3m-home):" nil nil w3m-home-page nil )))
  (w3m-goto-url-new-session
   (concat "http://" site)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; y_pe-w3m.el ends here
