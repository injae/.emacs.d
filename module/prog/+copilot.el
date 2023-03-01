;;; +copilot.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package copilot :elpaca (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
    :hook ((prog-mode . copilot-mode)
           (text-mode . copilot-mode))
    :preface
    (defun my/copilot-tab ()
        "Tab command that will complet with copilot if a completion is available.
         Otherwise will try company, yasnippet or normal tab-indent."
        (interactive)
        (or (copilot-accept-completion)
            ;; (company-yasnippet-or-completion)
            (indent-for-tab-command)))
    (defun my/copilot-quit ()
        "Run `copilot-clear-overlay' or `keyboard-quit'. If copilot is cleared,
         make sure the overlay doesn't come back too soon."
        (interactive)
        (condition-case err
            (when copilot--overlay
                (lexical-let ((pre-copilot-disable-predicates copilot-disable-predicates))
                    (setq copilot-disable-predicates (list (lambda () t)))
                    (copilot-clear-overlay)
                    (run-with-idle-timer
                        1.0
                        nil
                        (lambda ()
                            (setq copilot-disable-predicates pre-copilot-disable-predicates)))))
            (error handler)))
    :bind (:map copilot-mode-map
                ("TAB" . my/copilot-tab)
                ("C-<return>" . my/copilot-tab)
                ("C-n" . copilot-next)
                ("C-p" . copilot-previous)
                ("C-g" . copilot-clear-overlay)
              )
    ;; :config (advice-add 'keyboard-quit :before #'my/copilot-quit)
    )


;; you can utilize :map :hook and :config to customize copilot
(provide '+copilot)
;;; +copilot.el ends here
