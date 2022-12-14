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

(use-package ts-fold; :after tree-sitter
    :straight (ts-fold :type git :host github :repo "emacs-tree-sitter/ts-fold")
    :general (leader "<tab>" #'ts-fold-toggle)
    :custom ((ts-fold-indicators-fringe 'left-fringe)
             (ts-fold-summary-show t)
             (ts-fold-indicators-priority 100))
    :config
    (add-hook 'tree-sitter-after-on-hook 'ts-fold-mode)
    (add-hook 'tree-sitter-after-on-hook 'ts-fold-indicators-mode)
)

(provide 'module-highlight)
;;; module-highlight.el ends here
