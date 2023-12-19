;;; +csharp.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package csharp-mode :after lsp-mode :disabled
:mode (("\\.cs\\'" . csharp-mode))
       ;("\\.cs\\'" . csharp-tree-sitter-mode))
:hook (csharp-mode . lsp-deferred)
)

(provide '+csharp)
;;; +csharp.el ends here
