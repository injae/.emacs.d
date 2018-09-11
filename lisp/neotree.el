;;; neotree.el --- initialization emacs
;;; Commentry:
;;; neotree setting 
;;; Code:

(use-package neotree
    :ensure t
    :init
        (progn
            (setq-default neo-window-width 30)
            (setq-default neo-smart-open t)
            (setq-default neo-dont-be-alone t)
            (setq neo-force-change-root t)
            (setq projectile-switch-project-action 'neotree-projectile-action)
            (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
        )
        (add-hook 'neotree-mode-hook
            (lambda ()
                (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
                (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-quick-look)
                (define-key evil-normal-state-local-map (kbd "q")   'neotree-hide)
                (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
                (define-key evil-normal-state-local-map (kbd "g")   'neotree-refresh)
                (define-key evil-normal-state-local-map (kbd "n")   'neotree-next-line)
                (define-key evil-normal-state-local-map (kbd "p")   'neotree-previous-line)
                (define-key evil-normal-state-local-map (kbd "A")   'neotree-stretch-toggle)
                (define-key evil-normal-state-local-map (kbd "H")   'neotree-hidden-file-toggle)
            )
        )
        (add-hook 'neotree-mode-hook (lambda () (linum-mode nil)))
    :config
        (setq neo-show-hidden-files t)
        (evil-leader/set-key "n" 'neotree-toggle)
)

