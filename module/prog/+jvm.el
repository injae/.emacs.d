;;; +jvm.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;(use-package lsp-java   :config (add-hook 'java-mode-hook 'lsp))
;(use-package dap-java :ensure nil)
(use-package gradle-mode :config (add-hook 'java-mode-hook 'gradle-mode))
(use-package groovy-mode
:mode (".gradle\\'" . groovy-mode)
)

(use-package kotlin-mode
:config
    (setq lsp-clients-kotlin-server-executable "~/dev/tools/kotlin-language-server/server/build/install/server/bin/kotlin-language-server")
    (add-hook 'kotlin-mode-hook 'lsp)
)


(provide '+jvm)
;;; +jvm.el ends here
