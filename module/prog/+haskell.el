;;; +git.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package haskell-mode
:mode ("\\.hs\\'"    . haskell-mode)
)

(use-package lsp-haskell  :after haskell-mode
:hook ((haskell-mode . (lambda () (lsp)))
       (haskell-literate-mode . (lambda () (lsp))))
)


(provide '+haskell)
;;; +haskell.el ends here
