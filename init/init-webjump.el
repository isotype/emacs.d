;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author: Anton Strilchuk <ype@env.sh>                             ;;
;; URL: http://ype.env.sh                                           ;;
;; Created: 30-06-2014                                              ;;
;; Last-Updated: 30-06-2014                                         ;;
;;  Update #: 2                                                     ;;
;;   By: Anton Strilchuk <ype@env.sh>                               ;;
;;                                                                  ;;
;; Filename: init-webjump                                           ;;
;; Version:                                                         ;;
;; Description:                                                     ;;
;;                                                                  ;;
;; Package Requires: ()                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Webjump
(require 'webjump)
(after-load 'webjump
  (setq webjump-sites
        '(("DuckDuckGo" .
           [simple-query
            "duckduckgo.com"
            "https://duckduckgo.com/?q=" ""])
          ("Google" .
           [simple-query
            "google.com"
            "https://google.co.uk/#q="
            ""])
          ("GitHub: Repo" .
           [simple-query
            "github.com"
            "https://github.com/search?q="
            "&ref=searchresults&type=Repositories"])
          ("GitHub: Code" .
           [simple-query
            "github.com"
            "https://github.com/search?q="
            "&ref=searchresults&type=Code"])
          ("GitHub: Issues" .
           [simple-query
            "github.com"
            "https://github.com/search?q="
            "&ref=searchresults&type=Issues"])
          ("GitHub: Users" .
           [simple-query
            "github.com"
            "https://github.com/search?q="
            "&ref=searchresults&type=Users"])
          )))

(global-set-key (kbd "s-j") 'webjump)

(provide 'init-webjump)
