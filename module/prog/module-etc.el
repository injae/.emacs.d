;;; module-etc.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'use-package)

(use-package direnv  :config (direnv-mode))

(use-package gdscript-mode  :disabled
:hook   (gdscript-mode . lsp)
:custom (gdscript-godot-executable "/usr/local/Caskroom/godot/3.2.2/Godot.app/Contents/MacOS/Godot")
)
;(use-package company-godot-gdscript :quelpa (company-godot-gdscript :fetcher github :repo "francogarcia/company-godot-gdscript.el") :disabled
;:after (gdscript-mode company)
;:config (add-to-list 'company-backends 'company-godot-gdscript)
;)

(use-package protobuf-mode )

(use-package graphql-mode 
:mode ((".graphql\\'" . graphql-mode)
       (".prisma\\'"  . graphql-mode))
:hook (graphql-mode . (lambda () (require 'lsp-graphql) (lsp)))
)

(use-package lua-mode 
:mode ("\\.lua\\'" . lua-mode)
)

(provide 'module-etc)
;;; module-etc.el ends here
