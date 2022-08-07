;;; module-web.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'use-package)
(require 'straight)

(use-package web-mode :straight t
;:ensure-system-package (nvm . "curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.1/install.sh | bash")
;:commands (web-mode)
    :mode (("\\.html?\\'"  . web-mode)
           ("\\.xhtml$\\'" . web-mode)
           ("\\.phtml\\'" . web-mode)
           ("\\.erb\\'" . web-mode)
           ("\\.tpl\\.php\\'" . web-mode)
           ("\\.[agj]sp\\'" . web-mode)
           ("\\.as[cp]x\\'" . web-mode)
           ("\\.mustache\\'" . web-mode)
           ("\\.djhtml\\'" . web-mode)
           ("\\.vue\\'" . web-mode)
           ("\\.tsx\\'" . web-mode))
    :custom
    (web-mode-enable-engine-detection t)
        ;(web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
        ;(add-hook 'web-mode-hook
        ;    (lambda ()
        ;        (when (string-equal "tsx" (file-name-extension buffer-file-name)
        ;                  (setup-tide-mode)))))
    (flycheck-add-mode 'typescript-tslint 'web-mode 'jsx-tide)
    :config
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-enable-auto-pairing t)
    (setq web-mode-enable-css-colorization t)
    (add-hook 'web-mode-hook #'lsp)
)

(use-package js2-mode :straight t
:mode (("\\.js\\'"  . js2-mode)
       ("\\.mjs\\'" . js2-mode))
:hook (js2-mode . (lambda () (lsp)))
)

(use-package xref-js2 :straight t
:after (js2-mode xref)
:config (add-hook 'js2-mode-hook (lambda () (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
)

(use-package skewer-mode :straight t
:after js2-mode
:hook  ((js2-mode  . skewer-mode)
        (css-mode  . skewer-css-mode)
        (html-mode . skewer-html-mode))
)

(use-package typescript-mode :straight t
    :after tree-sitter
    :mode  ("\\.ts\\'"  . typescript-mode)
    :hook (typescript-mode . (lambda () (lsp)))
)

;(use-package tsx-mode :straight (:type git :host github :repo "orzechowskid/tsx-mode.el") :disabled)

(use-package tide :straight t
:after (typescript-mode company flycheck)
:hook  ((typescript-mode . tide-setup)
        (typescript-mode . tide-hl-identifier-mode)
        (before-save . tide-format-before-save))
)

;(use-package vue-mode :straight t
;    ;; install lsp-volar-* and typescript (npm install -g typescript)
;    :mode "\\.vue\\'"
;    :hook (vue-mode . prettier-js-mode)
;    :config (add-hook 'vue-mode-hook #'lsp)
;            (setq prettier-js-mode '("--parser vue"))
;)

(use-package prettier-js :straight t
:hook (js2-mode . prettier-js-mode)
      (web-mode . prettier-js-mode)
)

(provide 'module-web)
;;; module-web.el ends here
