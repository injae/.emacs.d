;;; module-ssh.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'use-package)
(require 'straight)

(use-package ssh-config-mode 
:config (add-to-list 'auto-mode-alist '("/\\.ssh/config\\'" . ssh-config-mode))
)

(use-package ssh-deploy 
:hook ((after-save . ssh-deploy-after-save)
       (find-file . ssh-deploy-find-file))
:config (ssh-deploy-line-mode)
        (ssh-deploy-add-menu)
)

(provide 'module-ssh)
;;; module-ssh.el ends here
