;;; evil.el --- Initialization file  evil package
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs
;;; Code:

(use-package evil
    :ensure t
    :init
        (evil-mode t)
        (setq evil-want-C-u-scroll t)
    :config
        (evil-set-initial-state 'calender-mode    'emacs)
        (evil-set-initial-state 'calculater-mode  'emacs)
        (evil-set-initial-state 'git-rebase-mode  'emacs)
        (evil-set-initial-state 'magit-blame-mode 'emacs)
        (setq-default evil-symbol-word-search t)
)

(use-package evil-leader
    :ensure t
    :config
        (setq evil-leader/leader "<SPC>")
        (evil-leader/set-key
            "<SPC>" 'helm-M-x
            "er"    'eval-buffer
            "b"     'switch-to-buffer
            "f"     'find-file
            "t"     'eshell
            "ef"    (lambda ()(interactive)(find-file "~/.emacs.d/init.el"))
            "wh"    'shrink-window-horizontally
            "wj"    'enlarge-window
            "wk"    'shrink-window
            "wl"    'enlarge-window-horizontally
        )
        (global-evil-leader-mode t)
)
