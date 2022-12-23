;;; module-project-manage.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'use-package)

(use-package projectile
:config
    (setq projectile-require-project-root nil)
	(setq projectile-enable-caching t)
	(setq projectile-current-project-on-switch t)
	(setq projectile-globally-ignored-directories (append '("^\\__pycache__$" "^\\eln-cache$" "^\\.emacs.d/straight$") projectile-globally-ignored-directories))
	(evil-ex-define-cmd "kp" 'projectile-kill-buffers)
    (projectile-mode)
)

; brew install coreutils fd poppler ffmpegthumbnailer mediainfo imagemagick
(use-package dirvish
    :ensure-system-package ((fd . "brew install fd"))
    :config (dirvish-override-dired-mode)
    )


(provide 'module-project-manage)
;;; module-project-manage.el ends here
