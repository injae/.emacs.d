;;; +project-manage.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package projectile :after evil
:config
    (setq projectile-require-project-root nil)
	(setq projectile-enable-caching t)
	(setq projectile-current-project-on-switch t)
	(setq projectile-globally-ignored-directories (append '("^\\eln-cache$" "^\\.emacs.d/straight$") projectile-globally-ignored-directories))
	(evil-ex-define-cmd "kp" 'projectile-kill-buffers)
    (projectile-mode)
)

; brew install coreutils fd poppler ffmpegthumbnailer mediainfo imagemagick
(use-package dirvish
    :ensure-system-package ((fd . "brew install fd"))
    :config (dirvish-override-dired-mode)
    )


(provide '+project-manage)
;;; +project-manage.el ends here
