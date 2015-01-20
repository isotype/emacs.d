;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author: Anton Strilchuk <anton@env.sh>                           ;;
;; URL: http://ype.env.sh                                           ;;
;; Created: 23-10-2014                                              ;;
;;; Last-Updated: 20-01-2015                                       ;;;
;;;  Update #: 17                                                  ;;;
;;;   By: Anton Strilchuk <anton@env.sh>                           ;;;
;;                                                                  ;;
;; Filename: init-feeds                                             ;;
;; Version:                                                         ;;
;; Description:                                                     ;;
;;                                                                  ;;
;; Package Requires: ()                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init-feeds)

(require-package 'elfeed)
(require-package 'elfeed-web)

(setq elfeed-feeds
      '(("https://news.ycombinator.com/rss" technews bloat)
        ("http://careers.stackoverflow.com/jobs/feed?type=any&location=SW129HW&range=20&distanceUnits=Miles" jobs)
        ("http://www.datatau.com/rss" datasci technews bloat)
        ("http://mix.chimpfeedr.com/ce98c-investopedia" inves)
        ("http://blog.echen.me/feeds/all.rss.xml" datasci technews)
        ("http://www.kdnuggets.com/tags/top-stories/feed" datasci dm ml technews bloat)
        ("http://simplystatistics.org/feed/" stats)
        ("http://freepythontips.wordpress.com/feed/" python technews)
        ("http://www.dataists.com/feed" datasci technews)
        ("http://feeds.feedburner.com/LifeIsShort-YouNeedPython" python progm)
        ("http://flowingdata.com/feed/" datasci technews)
        ("http://hunch.net/?feed=rss2" technews)
        ("http://christophe.rhodes.io/notes/blog/index.rss" blogs)
        ("http://feeds2.feedburner.com/stevelosh" blogs)
        ("http://feeds.feedburner.com/SanityInc" blogs)
        ("http://jim.sagepub.com/rss/recent.xml" pubs sci)
        ("http://jim.sagepub.com/rss/ahead.xml" pubs sci)
        ("http://www.plosone.org/article/feed/search?unformattedQuery=subject%3A%22Engineering+and+technology%22&sort=Date%2C+newest+first" pubs plos)
        ("http://www.plosone.org/article/feed/search?unformattedQuery=subject%3A%22Computer+and+information+sciences%22&sort=Date%2C+newest+first" pubs plos)
        ("http://www.plosone.org/article/feed/search?unformattedQuery=subject%3A%22Social+sciences%22&sort=Date%2C+newest+first" pubs plos)
        ("http://www.plosone.org/article/feed/search?unformattedQuery=subject%3A%22Medicine+and+health+sciences%22&sort=Date%2C+newest+first" pubs plos)
        ("http://www.plosone.org/article/feed/search?unformattedQuery=subject%3A%22Research+and+analysis+methods%22&sort=Date%2C+newest+first" pubs plos)
        ("http://blog.quicklisp.org/feeds/posts/default" lisp progm)
        ("http://geekhack.org/index.php?action=.xml;type=rss" kbd general)
        ("https://my.xilo.net/xilo/adslusagesummary.php?id=583b41008e8f72677e263ec7675670ff&type=rss" infofeeds)
        ("http://www.gavinocarroll.com/rss" blogs)
        ("http://links.laughingsquid.com/rss" technews bloat)
        ("http://news.nationalpost.com/category/news/canada/feed/" cdnnews bloat)
        ("http://www.fashioningtech.com/profiles/blog/feed?promoted=1&amp;xn_auth=no" etex technews)
        ("http://www.creativeapplications.net/feed/" technews bloat)
        ("http://reddit.com/r/emacs/.rss" emacs reddit progm)
        ("http://ergoemacs.org/emacs/blog.xml" emacs blogs progm)
        ("http://www.engadget.com/rss.xml" technews bloat)
        ("http://vimeo.com/channels/tclispers/videos/rss" video lisp progm)
        ("http://feeds.arstechnica.com/arstechnica/index" technews bloat)
        ("http://feeds.bbci.co.uk/news/rss.xml?edition=int" uknews)
        ("http://createdigitalmusic.com/feed/" music)
        ("http://feeds.gawker.com/gizmodo/full" general bloat)
        ("http://www.instructables.com/tag/type-id/rss.xml" tutorials bloat)
        ("http://feeds.gawker.com/lifehacker/vip" tutorials bloat)
        ("http://feeds.feedburner.com/linuxjournalcom?format=xml" linux progm)
        ("http://feeds.macrumors.com/MacRumors-All" mac general)
        ("http://feeds.feedburner.com/makezineonline" tutorials)
        ("http://planet.python.org/rss20.xml" python progm)
        ("http://reddit.com/r/programming/.rss" reddit progm R)
        ("http://feeds.feedburner.com/betalist?format=xml" nocat)
        ("http://www.synthtopia.com/feed/" music)
        ("http://techmeme.com/feed.xml" technews bloat)
        ("http://feeds.feedburner.com/uncrate" general bloat)
        ("http://feeds.gawker.com/valleywag/full" technews startups bloat)
        ("http://waxy.org/links/index.xml" technews bloat)
        ("http://feeds.wired.com/wired/index" technews bloat)
        ("https://www.google.co.uk/finance/company_news?q=CURRENCY:GBP&ei=fWOlVKuiDOvCwAPMxoDQCA&output=rss" currency GBP)
        ("https://www.google.co.uk/finance/company_news?q=CURRENCY:CAD&ei=iWSlVNn0FeXDwAOAmYHQCg&output=rss" currency CAD)
        ))

;; (add-hook 'elfeed-new-entry-hook
;;           (elfeed-make-tagger :before "2 weeks ago"
;;                               :remove 'unread))

(global-set-key (kbd "C-x w") 'elfeed)
