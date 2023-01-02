;;; module-extension.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'use-package)

;; spotify controller
(use-package smudge :defer t
; in private/token.el
:general (leader "sn" 'smudge-controller-next-track
                 "hp" 'smudge-controller-previous-track)
:config  (setq smudge-transport 'connect)
)

; slack config in private token setting
(use-package alert
:commands (alert)
:custom (alert-default-style 'notifier))

(use-package page-break-lines :defer t)

(use-package dashboard
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

(provide 'module-extension)
;;; module-extension.el ends here
