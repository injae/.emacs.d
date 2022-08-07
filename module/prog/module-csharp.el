;;; module-csharp.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'use-package)
(require 'straight)

(use-package csharp-mode :straight t
:mode (("\\.cs\\'" . csharp-mode))
       ;("\\.cs\\'" . csharp-tree-sitter-mode))
:hook (csharp-mode . lsp)
)

(provide 'module-csharp)
;;; module-csharp.el ends here
