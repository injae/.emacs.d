;;; spaceline.el --- initialization emacs
;;; Commentry:
;;; space line setting
;;; Code:

(use-package spaceline
    :ensure t
    :config
    (use-package nyan-mode
        :ensure t
        :init
            (setq-default nyan-wavy-trail t)
            (nyan-mode)
            (nyan-start-animation)
            (nyan-refresh)
    )
)

(use-package spaceline-config
    :ensure spaceline
    :init
        ;(custom-set-faces '(mode-line-buffer-id ((t nil)))) ;; blend well with tango-dark
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
        (spaceline-toggle-major-mode-off)
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
(use-package spaceline-all-the-icons
    :ensure t
    :after spaceline
    :config
        ;(spaceline-all-the-icons-theme)
        ;(spaceline-toggle-all-the-icons-buffer-id-on)
        ;(spaceline-toggle-all-the-icons-git-status-on)
        ;(spaceline-toggle-all-the-icons-nyan-cat)
        ;(spaceline-toggle-all-the-icons-flycheck-status)
        ;(spaceline-toggle-all-the-icons-narrowed)
        ;(spaceline-all-the-icons--setup-package-updates)
        ;(spaceline-all-the-icons--setup-git-ahead)
        ;(spaceline-all-the-icons--setup-neotree)
        (setq inhibit-compacting-font-caches t)
)
