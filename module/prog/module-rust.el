;;; module-git.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'use-package)
(require 'straight)

(use-package rustic :straight t
:ensure-system-package (rustup . "curl https://sh.rustup.rs -sSf | sh")
:mode ("\\.rs\\'" . rustic-mode)
:general (leader "hrf" 'rust-format-buffer)
:init
(defun ij/rustic-mode-hook ()
    (when buffer-file-name (setq-local buffer-save-without-query t)))

:config
    (setq lsp-eldoc-hook nil)
    (setq lsp-enable-symbol-highlighting nil)
    (setq lsp-signature-auto-activate nil)
    (setq rustic-lsp-server 'rust-analyzer)
    (lsp-flycheck-add-mode 'rustic-mode)
    (setq lsp-rust-server 'rust-analyzer)
    ;(setq lsp-rust-analyzer-cargo-watch-enable nil) ;; large project에서 cargo crate를 check하는것을 방지
    ;(setq rustic-format-on-save t)
    (add-hook 'rustic-mode-hook 'ij/rustic-mode-hook)
)

;(use-package rust-mode :straight t
;:ensure-system-package (rustup . "curl https://sh.rustup.rs -sSf | sh")
;:mode ("\\.rs\\'" . rust-mode)
;:hook (rust-mode . lsp)
;:general (leader "hrf" 'rust-format-buffer)
;:config  (setq lsp-rust-rls-command '("rustup", "run", "nightly", "rls"))
;         (setq lsp-rust-server 'rust-analyzer)
;         (setq lsp-rust-analyzer-cargo-watch-enable nil) ;; large project에서 cargo crate를 check하는것을 방지
;         ;(lsp-rust-analyzer-inlay-hints-mode t) ; display type hint 
;         ;(setq rust-format-on-save t)
;         ;(add-hook 'rust-mode-hook (lambda () (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)))
;)

(use-package flycheck-rust :straight t 
:after  (flycheck rust-mode)
:config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
)

(use-package cargo :straight t 
:after  rust-mode
:hook (rust-mode . cargo-minor-mode)
:commands cargo-minor-mode
:general (leader "hrb" 'cargo-process-build
                 "hrr" 'cargo-process-run
                 "hrt" 'cargo-process-test)
)


(provide 'module-rust)
;;; module-rust.el ends here
