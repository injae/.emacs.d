;;; module-copilot.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package editorconfig :straight t)
(use-package copilot :straight (:host github :repo "zerolfx/copilot.el") :disabled
    :after editorconfig 
    :config
        (add-hook 'prog-mode-hook 'copilot-mode)
        (customize-set-variable 'copilot-enable-predicates '(evil-insert-state-p))
        (defun my-copilot-complete ()
            (interactive)
            (or (copilot-accept-completion)
                (company-indent-or-complete-common nil)))
            ; modify company-mode behaviors
            (with-eval-after-load 'company
            ; disable inline previews
            (delq 'company-preview-if-just-one-frontend company-frontends)
                ; enable tab completion
                (define-key company-mode-map   (kbd "<C-tab>") 'my-copilot-complete)
                (define-key company-active-map (kbd "<C-return>") 'my-copilot-complete)
        )
    )

(provide 'module-copilot)
;;; module-copilot.el ends here
