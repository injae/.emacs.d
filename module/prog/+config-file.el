;;; +config-file.el --- Summery -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package yaml-mode
:ensure-system-package (yaml-language-server . "npm install -g yaml-language-server")
:hook (yaml-mode . (lambda () (lsp)))
:mode (("\\.yaml\\'" . yaml-mode)
       ("\\.yml\\'"  . yaml-mode))
)

(use-package toml-mode
:mode (("\\.toml\\'" . toml-mode)
       ("Pipfile\\'" . toml-mode))
)

(use-package cmake-mode
:ensure-system-package (cmake-language-server . "pip3 install cmake-language-server")
:commands cmake-mode
:mode (("\\.cmake\\'"    . cmake-mode)
       ("CMakeLists.txt" . cmake-mode))
:hook (cmake-mode . (lambda () (require 'lsp-cmake) (lsp)))
:init (setq cmake-tab-width 4)
)

(use-package markdown-mode
:after poly-markdown
:mode  (("\\README.md\\'" . gfm-mode)
        ("\\.md\\'"       . gfm-mode)
        ("\\.markdown\\'" . gfm-mode))
:general (leader "hm" '(:wk "Markdown"))
:config (setq markdown-command "multimarkdown")
        (poly-markdown-mode)
)

(use-package markdown-preview-mode  :defer t)
(use-package gh-md :defer t
:general (leader "hmr" 'gh-md-render-buffer)
)

(use-package powershell)

(use-package json-mode
    :mode  (("\\.json\\'"       . json-mode)
            ("/Pipfile.lock\\'" . json-mode))
)

(use-package jsonian :elpaca (:type git :host github :repo "iwahbe/jsonian")
    :after so-long
    :custom (jsonian-no-so-long-mode)
    :config
    (defun json-pretty (start end)
        (interactive "*r")
        (replace-string "\\\"" "\"" nil start end)
        (json-pretty-print-buffer)
    )

    (defun temp-json-pretty-buffer ()
        "json pretty print buffer"
        (interactive)
        (temp-buffer)
        (require 'jsonian-mode)
        (jsonian-mode)
        (yank-pop)
        (json-pretty (region-beginning) (region-end))
        )
    )

(use-package json-reformat
	:commands json-reformat-region)

(use-package jq-mode
	:commands jq-interactively)

(use-package jq-ts-mode :after jq-mode
	:config
	(add-to-list
		'treesit-language-source-alist
		'(jq "https://github.com/nverno/tree-sitter-jq" nil nil nil))
	)



(use-package terraform-mode :after exec-path-from-shell
    :ensure-system-package (terraform-ls . "go install github.com/hashicorp/terraform-ls@latest")
    :mode   ("\\.tf\\'" . terraform-mode)
    :hook (terraform-mode . (lambda () (lsp)))
    :custom
    (terraform-indent-level 2)
    (lsp-terraform-enable-logging t)
    (lsp-terraform-ls-enable-show-reference t)
    ;; (lsp-semantic-tokens-enable t)
    ;; (lsp-semantic-tokens-honor-refresh-requests t)
    (lsp-enable-links t)
    (lsp-terraform-ls-prefill-required-fields t)
    (lsp-terraform-ls-validate-on-save t)
    ;; (lsp-register-client
    ;;     (make-lsp-client
    ;;         :new-connection (lsp-stdio-connection '("~/go/bin/terraform-ls" "serve"))
    ;;         :major-modes    '(terraform-mode)
    ;;         :server-id      'terraform-ls))
    )
;;

(use-package dotenv-mode
    :mode (("\\.env\\..*\\'" . dotenv-mode)
           ("\\.envrc\\'" . dotenv-mode)))

(use-package protobuf-mode :mode "\\.proto\\'")

(use-package plantuml-mode :mode "\\.plantuml\\'")

(provide '+config-file)
;;; +config-file.el ends here
