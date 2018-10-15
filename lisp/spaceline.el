;;; spaceline.el --- initialization emacs
;;; Commentry:
;;; space line setting
;;; Code:

(use-package spaceline :ensure t)
(use-package spaceline-config
    :ensure spaceline
    :init
    (custom-set-faces '(mode-line-buffer-id ((t nil)))) ;; blend well with tango-dark
    (setq powerline-default-separator 'arrow)   ;; bar arrow wave utf-8
    (spaceline-spacemacs-theme)
    (spaceline-toggle-buffer-id-on)
    (spaceline-toggle-input-method-on)
    (spaceline-toggle-buffer-modified-on)
    (spaceline-toggle-buffer-encoding-on)
    (spaceline-toggle-process-on)
    (spaceline-toggle-projectile-root-on)
    (spaceline-toggle-version-control-on)
    (spaceline-toggle-flycheck-error-on)
    (spaceline-toggle-flycheck-info-on)
    (spaceline-toggle-flycheck-warning-on)
    (spaceline-toggle-major-mode-on)
    (spaceline-toggle-minor-modes-on)
    (spaceline-toggle-line-column-on)
    (spaceline-toggle-window-number-on)
    (spaceline-toggle-buffer-encoding-on)
    (spaceline-toggle-evil-state-on)
    (spaceline-toggle-nyan-cat-on)
    (spaceline-helm-mode 1)
    :config
    (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
    (setq evil-normal-state-tag   (propertize "COMMAND "))
    (setq evil-emacs-state-tag    (propertize "EMACS   "))
    (setq evil-insert-state-tag   (propertize "INSERT  "))
    (setq evil-replace-state-tag  (propertize "REPLACE "))
    (setq evil-motion-state-tag   (propertize "MOTION  "))
    (setq evil-visual-state-tag   (propertize "VISUAL  "))
    (setq evil-operator-state-tag (propertize "OPERATE "))
)
(use-package all-the-icons :ensure t)

(use-package nyan-mode :ensure t
    :init
    (setq-default nyan-wavy-trail t)
    (nyan-mode)
    (nyan-start-animation)
    (nyan-refresh)
)
(when window-system
    (use-package mode-icons :ensure t
        :init (setq mode-icons-change-mode-name nil)
        :config (mode-icons-mode)
    )
)
(use-package fancy-battery :ensure t
    :init
    (setq fancy-battery-show-percentage t)
    (fancy-battery-mode)
)

;(use-package spaceline-all-the-icons :ensure t
;    :after spaceline
;    :config
;    ;(spaceline-all-the-icons-theme)
;    ;(spaceline-all-the-icons--setup-package-updates)
;    ;(spaceline-all-the-icons--setup-neotree)
;    ;(spaceline-all-the-icons--setup-git-ahead)
;    ;(spaceline-toggle-all-the-icons-eyebrowse-workspace-on)
;    ;(spaceline-toggle-all-the-icons-sunrise-on)
;    ;(spaceline-toggle-all-the-icons-sunset-on)
;    ;(spaceline-toggle-all-the-icons-time-on)
;    ;(spaceline-toggle-all-the-icons-weather-on)
;    ;(spaceline-toggle-all-the-icons-flycheck-status-on)
;    ;(spaceline-toggle-all-the-icons-flycheck-status-info-on)
;    ;(spaceline-toggle-all-the-icons-buffer-id-on)
;    ;(spaceline-toggle-all-the-icons-git-status-on)
;    ;(spaceline-toggle-all-the-icons-nyan-cat-on)
;    ;(spaceline-toggle-all-the-icons-narrowed-on)
;    ;(spaceline-toggle-all-the-icons-git-ahead-on)
;    ;(spaceline-toggle-all-the-icons-bookmark-on)
;    ;(spaceline-toggle-all-the-icons-projectile-on)
;    ;(spaceline-toggle-all-the-icons-window-number-on)
;    ;(spaceline-toggle-all-the-icons-mode-icon-on)
;    ;(spaceline-toggle-all-the-icons-battery-status-on)
;    ;(setq inhibit-compacting-font-caches t)
;)
;;
