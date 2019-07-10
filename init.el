(require 'package)
    (add-to-list 'package-archives '("melpa"     . "http://melpa.milkbox.net/packages/")  t)
    (add-to-list 'package-archives '("elpa"      . "https://tromey.com/elpa/")            t)
    (add-to-list 'package-archives '("org"       . "http://orgmode.org/elpa/")            t)
    ;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
    (package-initialize)
    (setq use-package-compute-statistics t)
    (unless (and (package-installed-p 'delight)
                 (package-installed-p 'use-package))
        (package-refresh-contents)
        (package-install 'delight t)
        (package-install 'use-package t))
    ;(setq-default use-package-always-defer t)
(package-initialize)

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

(setq custom-file "~/.emacs.d/custom-variable.el")
(load-file custom-file)
;
;(garbage-collect)
;(put 'narrow-to-region 'disabled nil)


