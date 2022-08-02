(use-package csharp-mode :straight t
:mode (("\\.cs\\'" . csharp-mode))
       ;("\\.cs\\'" . csharp-tree-sitter-mode))
:hook (csharp-mode . lsp)
)
