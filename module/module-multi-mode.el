;;; module-multi-mode.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'use-package)
(require 'straight)

(use-package polymode
;:hook (polymode . centaur-tabs-mode-hook)
:init (add-hook 'polymode-init-inner-hook #'evil-normalize-keymaps)
:custom (polymode-display-process-buffers nil)
)

(use-package poly-jetbrain-lua :no-require t :after polymode :straight nil
:config
    ; jetbrain golang lua mode
    (define-hostmode poly-golang-lua-hostmode :mode 'go-mode)
    (define-innermode poly-golang-lua-innermode
        :mode 'lua-mode
        :head-matcher "// language=lua\n.*`$"
        :tail-matcher "^`$"
        ;:mode-matcher (cons "")
        :head-mode 'host
        :tail-mode 'host
        )
    (define-polymode poly-golang-lua-mode
        :hostmode   'poly-golang-lua-hostmode
        :innermodes '(poly-golang-lua-innermode))
)

(use-package poly-org
:hook (org-mode . poly-org-mode)
      ;(poly-org-mode . git-gutter-mode)
:init (evil-set-initial-state 'poly-org-mode 'normal)
)

(use-package poly-markdown  :disabled
;:after (markdown-mode polymode)
:hook (markdown-mode . poly-markdown-mode)
;:init (evil-set-initial-state 'poly-org-mode 'normal)
)

(provide 'module-multi-mode)
;;; module-multi-mode.el ends here
