;;; module-csharp.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package csharp-mode
:mode (("\\.cs\\'" . csharp-mode))
       ;("\\.cs\\'" . csharp-tree-sitter-mode))
:hook (csharp-mode . lsp)
)

(provide 'module-csharp)
;;; module-csharp.el ends here
