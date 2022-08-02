
(use-package direnv :straight t :config (direnv-mode))

(use-package gdscript-mode :straight t :disabled
:hook   (gdscript-mode . lsp)
:custom (gdscript-godot-executable "/usr/local/Caskroom/godot/3.2.2/Godot.app/Contents/MacOS/Godot")
)
;(use-package company-godot-gdscript :quelpa (company-godot-gdscript :fetcher github :repo "francogarcia/company-godot-gdscript.el") :disabled
;:after (gdscript-mode company)
;:config (add-to-list 'company-backends 'company-godot-gdscript)
;)

(use-package protobuf-mode :straight t)

(use-package graphql-mode :straight t
:mode ((".graphql\\'" . graphql-mode)
       (".prisma\\'"  . graphql-mode))
:hook (graphql-mode . (lambda () (require 'lsp-graphql) (lsp)))
)

(use-package lua-mode :straight t
:mode ("\\.lua\\'" . lua-mode)
)
