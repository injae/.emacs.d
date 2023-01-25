;;; module-ui.el --- Summery
;;; -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:

(use-package all-the-icons)

(use-package all-the-icons-ibuffer
:after all-the-icons
:hook (ibuffer-mode . all-the-icons-ibuffer-mode)
)

(use-package all-the-icons-completion
:config (all-the-icons-completion-mode)
)

(use-package beacon
    :config (beacon-mode t))

(use-package git-gutter
:custom
    (git-gutter:lighter       " GG")
    (git-gutter:window-width  1)
    (git-gutter:modified-sign ".")
    (git-gutter:added-sign    "+")
    (git-gutter:deleted-sign  "-")
:config
    (global-git-gutter-mode t)
    (setq-default display-line-numbers-width 3)
    ;(add-hook 'prog-mode-hook 'display-line-numbers-mode)
    ;(add-hook 'org-mode-hook 'display-line-numbers-mode)
    (global-display-line-numbers-mode t)
    (global-hl-line-mode t)
    (set-face-foreground 'git-gutter:added    "#daefa3")
    (set-face-foreground 'git-gutter:deleted  "#FA8072")
    (set-face-foreground 'git-gutter:modified "#b18cce")
)
(use-package highlight-numbers :disabled
:hook (prog-mode . highlight-numbers-mode)
)

(setq custom-safe-themes t)

(use-package doom-themes
:config (load-theme   'doom-vibrant t)
        (doom-themes-org-config)
       ;(enable-theme 'doom-nord)
)

(use-package nano-theme :disabled
    :straight (:type git :host github :repo "rougier/nano-theme")
    :config (load-theme 'nano-dark t)
)

(use-package doom-modeline :after doom-themes
:hook   (after-init . doom-modeline-mode)
:init   (setq find-file-visit-truename t)
        (setq doom-modeline-buffer-file-name-style 'truncate-with-project)
        (setq inhibit-compacting-font-caches t)
        (setq doom-modeline-height 30)
        (setq doom-modeline-icon t) ; current version has error
        (setq doom-modeline-persp-name t)
        (setq doom-modeline-major-mode-icon t)
        (setq doom-modeline-enable-word-count t)
        (setq doom-modeline-lsp t)

        (setq doom-modeline-current-window t)
        (setq doom-modeline-env-version t)
        (setq doom-modeline-env-enable-python t)
        ;(setq doom-modeline-python-executable "pipenv")
        (setq doom-modeline-env-enable-ruby t)
        (setq doom-modeline-env-ruby-executable "ruby")
        (setq doom-modeline-env-enable-elixir t)
        (setq doom-modeline-env-elixir-executable "iex")
        (setq doom-modeline-env-enable-go t)
        (setq doom-modeline-env-go-executable "go")

        (setq doom-modeline-env-enable-perl t)
        (setq doom-modeline-env-perl-executable "perl")

        (setq doom-modeline-env-rust-executable "rustc")
        (setq doom-modeline-github t)
        ;(setq doom-modeline-iconer-state-icon t)
        ;(setq doom-modeline--battery-status t)
        (setq doom-modeline--flycheck-icon t)
        (setq doom-modeline-current-window t)
        (setq doom-modeline-major-mode-color-icon t)
:config (add-hook 'after-init-hook 'doom-modeline-mode)
        (with-eval-after-load 'lsp-treemacs (doom-themes-treemacs-config))
)

(use-package rainbow-mode :disabled
:hook   (prog-mode text-mode)
:config (rainbow-mode)
)

(use-package rainbow-delimiters
:hook ((prog-mode text-mode) . rainbow-delimiters-mode)
)

(use-package modern-fringes :defer t
:config (modern-fringes-invert-arrows)
        (modern-fringes-mode)
)

; 자동으로 Dark mode Light mode 변환
(use-package mac-dark-mode :no-require t :disabled
:if *is-mac*
:preface
(defun set-system-dark-mode ()
    (interactive)
    (if (string= (shell-command-to-string "printf %s \"$( osascript -e \'tell application \"System Events\" to tell appearance preferences to return dark mode\' )\"") "true")
        (load-theme 'doom-one t) ; dark-mode
        (load-theme 'doom-city-lights t)) ; light-mode
)
:config (run-with-idle-timer 60 t (lambda () (set-system-dark-mode)))  ; 1분마다, repeat
)

(use-package hide-mode-line
:after (neotree)
:hook  (neotree-mode . hide-mode-line-mode)
)

(use-package nyan-mode
;:after  (doom-modeline)
:config (setq nyan-wavy-trail t)
        (nyan-mode)
        (nyan-start-animation)
)

(use-package fancy-battery
:hook   (after-init . fancy-battery-mode)
:config (fancy-battery-default-mode-line)
        (setq fancy-battery-show-percentage t)
)

(use-package diminish :defer t
:init
    (diminish 'c++-mode "C++ Mode")
    (diminish 'c-mode   "C Mode")
)

(use-package neotree
:after (projectile all-the-icons)
:commands (neotree-toggle)
:general (leader "n" #'neotree-toggle)
:init
    (setq projectile-switch-project-action 'neotree-projectile-action)
    (setq-default neo-smart-open t)
:config
    (setq-default neo-window-width 30)
    (setq-default neo-dont-be-alone t)
    (add-hook 'neotree-mode-hook (lambda () (display-line-numbers-mode -1) ))
    (setq neo-force-change-root t)
    (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
    (setq neo-show-hidden-files t)
)

(use-package all-the-icons-dired
:after all-the-icons
:init  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(defun copy-file-name-to-clipboard ()
    "Copy the current buffer file name to the clipboard."
    (interactive)
    (let ((filename (if (equal major-mode 'dired-mode) default-directory (buffer-file-name))))
        (when filename
        (kill-new filename)
            (message "Copied buffer file name '%s' to the clipboard." filename)))
)
;; env.org[shell]
;; tab
(use-package centaur-tabs :after (s dashboard vterm)
:general (leader "th" 'centaur-tabs-backward
                 "tl" 'centaur-tabs-forward)
:hook   ((dashboard-mode . centaur-tabs-local-mode)
         (vterm-mode     . centaur-tabs-local-mode))
:custom (centaur-tabs-background-color (face-background 'default))
        (centaur-tabs-set-icons t)
        (centaur-tabs-set-bar 'over)
        (centaur-tabs-set-close-button t)
        (centaure-tabs-set-bar t)
        (centaur-tabs-style "chamfer")
:config (setq centaur-tabs-height 26)
        (setq centaur-tabs-cycle-scope 'tabs)
        (centaur-tabs-mode t)
        (centaur-tabs-headline-match)
        (centaur-tabs-group-by-projectile-project)
        (defun centaur-tabs-hide-tab (x)
            "Do no to show buffer X in tabs."
            (let ((name (format "%s" x)))
                (or ;; Current window is not dedicated window.
                    (window-dedicated-p (selected-window))
                    ;; Buffer name not match below blacklist.
                    (string-prefix-p "*epc" name)
                    (string-prefix-p "*helm" name)
                    (string-prefix-p "*Helm" name)
                    (string-prefix-p "*Compile-Log*" name)
                    (string-prefix-p "*lsp" name)
                    (string-prefix-p "*company" name)
                    (string-prefix-p "*Flycheck" name)
                    (string-prefix-p "*tramp" name)
                    (string-prefix-p " *Mini" name)
                    (string-prefix-p "*help" name)
                    (string-prefix-p "*straight" name)
                    (string-prefix-p "*temp" name)
                    (string-prefix-p "*Help" name)
                    (string-prefix-p "*pyright*" name)
                    (string-prefix-p "*pyright::stderr*" name)
                    (string-prefix-p "*Async-native-compile-log*" name)
                    (string-prefix-p "config.org[" name)
                    (s-matches? "^.*\\[.*\\]$" name)
                    ;; Is not magit buffer.
                    (and (string-prefix-p "magit" name)
                        (not (file-name-extension name)))
                    )))
)

(provide 'module-ui)
;;; module-ui.el ends here
