;;; +copilot.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package copilot :elpaca (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
    :hook (prog-mode . copilot-mode)
    :preface
    (defun my/copilot-tab ()
        (interactive)
        (or (copilot-accept-completion)
            (indent-for-tab-command)))
    :bind (:map copilot-mode-map
                ("TAB" . my/copilot-tab)
                ("C-<return>" . my/copilot-tab)
                ("C-n" . copilot-next)
                ("C-p" . copilot-previous)
                ("C-g" . copilot-abort))
    )

(use-package chatgpt :after python
    :elpaca (:host github :repo "joshcho/ChatGPT.el" :files ("dist" "*.el"))
    :ensure-system-package((chatgpt . "pip install epc && pip install git+https://github.com/mmabrouk/chatgpt-wrapper && chatgpt install"))

    :init
    (setq chatgpt-repo-path "~/.emacs.d/straight/repos/ChatGPT.el/")
    :bind ("C-c q" . chatgpt-query))


;; you can utilize :map :hook and :config to customize copilot
(provide '+copilot)
;;; +copilot.el ends here

