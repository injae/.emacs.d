;;; +tree-sitter.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package tree-sitter
    :hook ((emacs-startup . global-tree-sitter-mode)
           (tree-sitter-after-on . tree-sitter-hl-mode))
)

(use-package tree-sitter-langs :after tree-sitter)
;(use-package tree-sitter-indent :after tree-sitter)
;(use-package tsi :elpaca (:type git :host github :repo "orzechowskid/tsi.el") :after tree-sitter :disabled)

(use-package ts-fold :disabled
    :elpaca (ts-fold :type git :host github :repo "emacs-tree-sitter/ts-fold")
    :general (leader "<tab>" #'ts-fold-toggle)
    :custom ((ts-fold-indicators-fringe 'left-fringe)
             (ts-fold-summary-show t)
             (ts-fold-indicators-priority 100))
    :config
    (add-hook 'tree-sitter-after-on-hook 'ts-fold-mode)
    (add-hook 'tree-sitter-after-on-hook 'ts-fold-indicators-mode)
)

(use-package treesit :elpaca nil :disabled
    :if (treesit-available-p))

(use-package treesit-auto :disabled
    :if (treesit-available-p)
    :hook (emacs-startup . global-treesit-auto-mode)
    :custom (treesit-auto-install 'prompt)
    )

(provide '+tree-sitter)
;;; +tree-sitter.el ends here
