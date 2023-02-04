;;; module-extension.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


;; spotify (smudge) setting token in private c
(use-package smudge
:commands (smudge-controller-next-track smudge-controller-previous-track)
:general (leader "sn" 'smudge-controller-next-track
                 "sp" 'smudge-controller-previous-track
                 "ss" 'smudge-controller-toggle-play)
:config (setq smudge-transport 'connect)
)

;; slack config in private token setting

(use-package alert
:commands (alert)
:custom (alert-default-style 'notifier))

(use-package page-break-lines :defer t)

(use-package dashboard
    :functions dashboard-setup-startup-hook
    :custom (
        (dashboard-banner-logo-title "Happy Hacking")
        (dashboard-startup-banner "~/.emacs.d/image/emacs_icon.png") ;banner image change
        (initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
        (dashboard-set-heading-icons t)
        (dashboard-set-file-icons t)
        (dashboard-show-shortcuts nil)
        (dashboard-set-navigator t)
        (dashboard-week-agenda t)
        (dashboard-items '((recents   . 5)
                           (bookmarks . 5)
                           (projects  . 5)
                           (agenda    . 5)))
        (dashboard-set-init-info t))
    :config
        (dashboard-setup-startup-hook)
)

;; print file info
(use-package file-info
    :config
    (setq hydra-hint-display-type 'posframe)
    (setq hydra-posframe-show-params `(:poshandler posframe-poshandler-frame-center
                                             :internal-border-width 2
                                             :internal-border-color "#61AFEF"
                                             :left-fringe 16
                                             :right-fringe 16)))

(use-package elfeed
    :custom
    (elfeed-feeds '("https://sachachua.com/blog/category/emacs/feed/"
                    "http://feeds.feedburner.com/geeknews-feed"
                    "https://qiita.com/popular-items/feed"
                    "https://www.reddit.com/r/emacs.rss"))
    )

(use-package atomic-chrome
    :config (atomic-chrome-start-server)
    )

(provide 'module-extension)
;;; module-extension.el ends here
