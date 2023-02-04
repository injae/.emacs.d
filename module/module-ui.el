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
:hook (emacs-startup . all-the-icons-completion-mode)
)

(use-package beacon :disabled
    :hook (emacs-startup . beacon-mode))

(use-package diff-hl
    :hook
    ((magit-pre-refresh . diff-hl-magit-pre-refresh)
     (magit-post-refresh . diff-hl-magit-post-refresh)
     (emacs-startup . global-diff-hl-mode))
    :init
    (setq-default display-line-numbers-width 3)
    (global-display-line-numbers-mode t)
    (global-hl-line-mode t)
    ;:custom (diff-hl-side 'right)
)

(use-package highlight-numbers
:hook (prog-mode . highlight-numbers-mode)
)

(setq custom-safe-themes t)

;; 현재 발견된 doom-theme 버그
;; emacs29 이상에서 아래 메세지 출력
;; Warning: setting attribute ‘:background’ of face ‘font-lock-comment-face’: nil value is invalid, use ‘unspecified’ instead.
(use-package doom-themes
:config (load-theme   'doom-vibrant t)
        (doom-themes-neotree-config)
        (doom-themes-org-config)
        ;(setq doom-themes-treemacs-theme "doom-atom")
        (doom-themes-treemacs-config)
        ;(enable-theme 'doom-nord)
)

(use-package nano-theme :disabled
    :straight (:type git :host github :repo "rougier/nano-theme")
    :config (load-theme 'nano-dark t)
)

(use-package doom-modeline :after doom-themes
    :hook (after-init . doom-modeline-mode)
    :custom
    (doom-modeline-buffer-file-name-style 'truncate-with-project)
    (doom-modeline-height 30)
    (doom-modeline-icon t) ; current version has error
    (doom-modeline-persp-name t)
    (doom-modeline-major-mode-icon t)
    (doom-modeline-enable-word-count t)
    (doom-modeline-lsp t)
    
    (doom-modeline-current-window t)
    (doom-modeline-env-version t)
    (doom-modeline-env-enable-python t)
    (doom-modeline-env-enable-ruby t)
    (doom-modeline-env-ruby-executable "ruby")
    (doom-modeline-env-enable-elixir t)
    (doom-modeline-env-elixir-executable "iex")
    (doom-modeline-env-enable-go t)
    (doom-modeline-env-go-executable "go")
    (doom-modeline-env-enable-perl t)
    (doom-modeline-env-perl-executable "perl")
    (doom-modeline-env-rust-executable "rustc")
    (doom-modeline-github t)
    (doom-modeline--flycheck-icon t)
    (doom-modeline-current-window t)
    (doom-modeline-major-mode-color-icon t)
    ;; (doom-modeline-iconer-state-icon t)
    ;; (doom-modeline--battery-status t)
    :init
    (setq find-file-visit-truename t)
    (setq inhibit-compacting-font-caches t)
    :config
    (add-hook 'after-init-hook 'doom-modeline-mode)
    (with-eval-after-load 'lsp-treemacs (doom-themes-treemacs-config))
)

(use-package rainbow-mode
:hook   (prog-mode text-mode)
:config (rainbow-mode)
)

(use-package rainbow-delimiters
:hook ((prog-mode text-mode) . rainbow-delimiters-mode)
)

(use-package modern-fringes
    :config
    (modern-fringes-mode)
    (modern-fringes-invert-arrows)
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

(use-package hide-mode-line :after neotree
    :hook  (neotree-mode . hide-mode-line-mode)
)

(use-package nyan-mode
    :custom (nyan-wavy-trail t)
    :config (nyan-mode)
            (nyan-start-animation)
)

(use-package fancy-battery
    :hook   (after-init . fancy-battery-mode)
    :custom (fancy-battery-show-percentage t)
    :config (fancy-battery-default-mode-line)
)

(use-package neotree :after (projectile all-the-icons)
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
                    ;(s-matches? "^.*\\[.*\\]$" name) ;; disable poly-mode buffer
                    ;; Is not magit buffer.
                    (and (string-prefix-p "magit" name)
                        (not (file-name-extension name)))
                    )))
)

(provide 'module-ui)
;;; module-ui.el ends here
