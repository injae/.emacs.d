;;; module-highlight.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'use-package)
(require 'straight)

(use-package tree-sitter
    :config
    (global-tree-sitter-mode)
    (add-hook 'tree-sitter-after-on-hook 'tree-sitter-hl-mode)
)

(use-package tree-sitter-langs :after tree-sitter)
;(use-package tree-sitter-indent  :after tree-sitter)
;(use-package tsi :straight (:type git :host github :repo "orzechowskid/tsi.el") :after tree-sitter :disabled)

(provide 'module-highlight)
;;; module-highlight.el ends here
