
;(use-package lsp-java :straight t  :config (add-hook 'java-mode-hook 'lsp))
;(use-package dap-java :ensure nil)
(use-package gradle-mode :straight t  :config (add-hook 'java-mode-hook 'gradle-mode))
(use-package groovy-mode :straight t  
:mode (".gradle\\'" . groovy-mode)
)

(use-package kotlin-mode :straight t 
:config
    (setq lsp-clients-kotlin-server-executable "~/dev/tools/kotlin-language-server/server/build/install/server/bin/kotlin-language-server")
    (add-hook 'kotlin-mode-hook 'lsp)
)
