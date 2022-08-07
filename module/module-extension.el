;;; module-extension.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; spotify controller
(use-package smudge :straight t :defer t
; in private/token.el
:general (leader "sn" 'smudge-controller-next-track
                 "hp" 'smudge-controller-previous-track)
:config  (setq smudge-transport 'connect)
)

; slack config in private token setting
(use-package alert
:commands (alert)
:init (setq alert-default-style 'notifier))

(use-package page-break-lines :straight t  :defer t)
(use-package dashboard :straight t 
:init (dashboard-setup-startup-hook)
:config
    (add-hook 'dashboard-mode-hook (lambda () (display-line-numbers-mode -1) ))
    (setq dashboard-banner-logo-title "Happy Hacking")
    (setq dashboard-startup-banner "~/.emacs.d/image/emacs_icon.png") ;banner image change
    (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
    (setq dashboard-set-heading-icons t)
    (setq dashboard-set-file-icons t)
    (setq dashboard-show-shortcuts nil)
    (setq dashboard-set-navigator t)
    (setq dashboard-week-agenda t)

    ;(setq dashboard-center-content t)
    (setq dashboard-set-init-info t)
    (setq dashboard-items '((recents   . 5)
                            (bookmarks . 5)
                            (projects  . 5)
                            (agenda    . 5)))
)

(provide 'module-extension)
;;; module-extension.el ends here
