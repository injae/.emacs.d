(use-package ssh-config-mode :straight t
:config (add-to-list 'auto-mode-alist '("/\\.ssh/config\\'" . ssh-config-mode))
)

(use-package ssh-deploy :straight t
:hook ((after-save . ssh-deploy-after-save)
       (find-file . ssh-deploy-find-file))
:config (ssh-deploy-line-mode)
        (ssh-deploy-add-menu)
)
