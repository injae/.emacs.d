;;; module-nix.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'use-package)

(use-package nix-mode
:mode "\\.nix\\'"
:hook (nix-mode . (lambda () (lsp)))

)

(provide 'module-nix)
;;; module-nix.el ends here
