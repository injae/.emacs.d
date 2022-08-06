;;;

(use-package projectile :straight t
    :init (projectile-mode)
    :config
    (setq projectile-require-project-root nil)
	(setq projectile-enable-caching t)
	(setq projectile-globally-ignored-directories (append '("*/.ccls-cache" "*/.git" "*/__pycache__" "~/.emacs.d/eln-cache" "~/.emacs.d/straight/")
            projectile-globally-ignored-directories))
	(setq projectile-current-project-on-switch t)
	(evil-ex-define-cmd "kp" 'projectile-kill-buffers)
)

;;;
