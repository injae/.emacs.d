;;; +nix.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'use-package)

(use-package nix-mode
:mode "\\.nix\\'"
:hook (nix-mode . (lambda () (lsp)))

)

(provide '+nix)
;;; +nix.el ends here
