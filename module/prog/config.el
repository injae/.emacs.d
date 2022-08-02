(use-package yaml-mode :straight t 
:mode (("\\.yaml\\'" . yaml-mode)
       ("\\.yml\\'"  . yaml-mode))
)

(use-package toml-mode :straight t 
:mode (("\\.toml\\'" . toml-mode)
       ("Pipfile\\'" . toml-mode))
)

(use-package cmake-mode :straight t 
:ensure-system-package (cmake-language-server . "pip3 install cmake-language-server")
:commands cmake-mode
:mode (("\\.cmake\\'"    . cmake-mode)
       ("CMakeLists.txt" . cmake-mode))
:hook (cmake-mode . (lambda () (require 'lsp-cmake) (lsp)))
:init (setq cmake-tab-width 4)
)

(use-package markdown-mode :straight t 
:after poly-markdown
:mode  (("\\README.md\\'" . gfm-mode)
        ("\\.md\\'"       . gfm-mode)
        ("\\.markdown\\'" . gfm-mode))
:general (leader "hm" '(:wk "Markdown"))
:config (setq markdown-command "multimarkdown")
        (poly-markdown-mode)
)

(use-package markdown-preview-mode :straight t  :defer t)
(use-package gh-md :straight t  :defer t
:general (leader "hmr" 'gh-md-render-buffer)
)

(use-package powershell :straight t)

(use-package json-mode :straight t 
:mode  (("\\.json\\'"       . json-mode)
        ("/Pipfile.lock\\'" . json-mode))
)

(use-package json-reformat :straight t 
:commands json-reformat-region
)

(use-package terraform-mode :straight t
    :ensure-system-package (terraform-ls . "go install github.com/hashicorp/terraform-ls@latest")
    :mode   ("\\.tf\\'" . terraform-mode)
    :config (setq terraform-indent-level 4)
            (setq lsp-terraform-enable-logging t)
            (lsp-register-client
                (make-lsp-client
                    :new-connection (lsp-stdio-connection '("~/go/bin/terraform-ls" "serve"))
                    :major-modes    '(terraform-mode)
                    :server-id      'terraform-ls)))
           (add-hook 'terraform-mode-hook 'lsp)
