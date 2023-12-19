;;; +git.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


(use-package rustic :after exec-path-from-shell :disabled
    :ensure-system-package ((rustup . "curl https://sh.rustup.rs -sSf | sh")
                           (rust-analyzer . "rustup component add rust-analyzer"))
    :mode ("\\.rs\\'" . rustic-mode)
    ;:hook (rustic-mode . lsp-deferred)
    :hook (rust-mode . lsp-deferred)
    ;:hook (rust-mode . eglot-mode)
    :general (leader "hrf" 'rust-format-buffer)
    :init (defun ij/rustic-mode-hook ()
              (when buffer-file-name (setq-local buffer-save-without-query t)))
    :config
        (setq lsp-eldoc-hook nil)
        (setq lsp-enable-symbol-highlighting nil)
        (setq lsp-signature-auto-activate nil)
        (setq rustic-analyzer-command '("rustup run stable rust-analyzer"))
        (setq rustic-lsp-server 'rust-analyzer)
        (setq lsp-rust-server 'rust-analyzer)
        (lsp-flycheck-add-mode 'rustic-mode)
        ;(setq lsp-rust-analyzer-cargo-watch-enable nil) ;; large project에서 cargo crate를 check하는것을 방지
        ;(setq rustic-format-on-save t)
        (add-hook 'rustic-mode-hook 'ij/rustic-mode-hook)
)

(use-package rust-mode :after exec-path-from-shell
:ensure-system-package ((rustup . "curl https://sh.rustup.rs -sSf | sh")
                        (rust-analyzer . "rustup component add rust-analyzer"))
:mode ("\\.rs\\'" . rust-mode)
;:hook (rust-mode . lsp-deferred)
:general (leader "hrf" 'rust-format-buffer)
;:config ;(setq lsp-eldoc-hook nil)
        ;(setq lsp-enable-symbol-highlighting nil)
        ;(setq lsp-signature-auto-activate nil)
        ;(setq rustic-analyzer-command '("rustup run stable rust-analyzer"))
        ;(setq rustic-lsp-server 'rust-analyzer)
        ;(setq lsp-rust-server 'rust-analyzer)
        ;(lsp-flycheck-add-mode 'rustic-mode)
)

(use-package flycheck-rust :after (flycheck rustic)
:hook (flycheck-mode . flycheck-rust-setup)
)

(use-package cargo :after rust-mode
:hook (rust-mode . cargo-minor-mode)
:commands cargo-minor-mode
:general (leader "hrb" 'cargo-process-build
                 "hrr" 'cargo-process-run
                 "hrt" 'cargo-process-test)
)


(provide '+rust)
;;; +rust.el ends here
