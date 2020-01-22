(when (eval-when-compile (version< emacs-version "27"))
    (load "~/.emacs.d/early-init.el")
    (package-initialize))

    (setq use-package-compute-statistics t)
    (unless (and (package-installed-p 'delight)
                 (package-installed-p 'use-package))
        (package-refresh-contents)
        (package-install 'delight t)
        (package-install 'use-package t))
    ;(setq-default use-package-always-defer t)

(use-package auto-package-update :ensure t :pin melpa
:init (setq auto-package-update-delete-old-versions t)
      (setq auto-package-update-hide-results t)
      (auto-package-update-maybe)
)

(use-package use-package-ensure-system-package :ensure t :pin melpa)

;(use-package quelpa-use-package :ensure t :pin melpa)

(use-package org :ensure t :pin melpa
:init (org-babel-load-file (expand-file-name "config.org" user-emacs-directory))
:mode ("\\.org\\'" . org-mode)
)

;(use-package org-tanglesync :ensure t :pin melpa
;:hook ((org-mode . org-tanglesync-mode)
;       ((prog-mode text-mode) . org-tanglesync-watch-mode))
;;:custom (org-tanglesync-watch-files '("~/.emacs.d/config.org"))
;:bind (( "C-c M-i" . org-tanglesync-process-buffer-interactive)
;       ( "C-c M-a" . org-tanglesync-process-buffer-automatic))
;)

;(setq-default config-file "~/.emacs.d/config.el")
;(load "~/.emacs.d/config.el")

(setq-default custom-file "~/.emacs.d/custom-variable.el")
(when (file-exists-p custom-file) (load-file custom-file))

;(garbage-collect)
;(put 'narrow-to-region 'disabled nil)


