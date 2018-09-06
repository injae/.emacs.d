;;; paren.el --- initialization emacs
;;; Commentry:
;;; emacs init file 
;;; Code:

; paren brak ... color set

(use-package paren
    :ensure t 
    :init
      (show-paren-mode 1)
      (setq show-paren-delay 0)
)

(use-package rainbow-delimiters
    :ensure t
    :init
        (add-hook 'prog-mode-hook       'rainbow-delimiters-mode)
        (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
        (add-hook 'c++-mode-hook        'rainbow-delimiters-mode)
        ;(add-hook 'html-mode-hook       'rainbow-delimiters-mode)
        ;(add-hook 'css-mode-hook        'rainbow-delimiters-mode)
        ;(add-hook 'c-mode-hook          'rainbow-delimiters-mode)
        ;(add-hook 'lisp-mode-hook       'rainbow-delimiters-mode)
)

(use-package smartparens
    :ensure t
    :init (smartparens-global-mode)
    :config
    (use-package evil-smartparens
        :ensure t
        :init
           (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)
    )
)
