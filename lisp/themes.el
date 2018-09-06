;;; themes.el --- initialization emacs
;;; Commentry:
;;; emacs themes load file
;;; Code:


;(use-package spacemacs-theme :defer t :init (load-theme 'spacemacs-dark t))
(use-package doom-themes
    :init
        (doom-themes-neotree-config)
        (doom-themes-org-config)
        (load-theme 'doom-one t)
)
