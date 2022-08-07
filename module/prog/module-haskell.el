;;; module-git.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'use-package)
(require 'straight)

(use-package haskell-mode :straight t
:mode ("\\.hs\\'"    . haskell-mode)
)

(use-package lsp-haskell :straight t :after haskell-mode
:hook ((haskell-mode . (lambda () (lsp)))
       (haskell-literate-mode . (lambda () (lsp))))
)


(provide 'module-haskell)
;;; module-haskell.el ends here
