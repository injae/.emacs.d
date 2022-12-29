;;; module-lsp.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'use-package)
(require 'straight)

(use-package lsp-mode ;:after exec-path-from-shell
:commands lsp
:hook ((lsp-completion-mode . my/lsp-mode-setup-completion)
       (lsp-mode  . lsp-enable-which-key-integration))
:general (leader "hh" '(lsp-execute-code-action         :wk "wizard")
                 "pp" '(xref-go-back                    :wk "lsp pop")
                 "fd" '(lsp-ui-peek-find-definitions    :wk "lsp define")
                 "fi" '(lsp-ui-peek-find-implementation :wk "lsp impl")
                 "fr" '(lsp-ui-peek-find-references     :wk "lsp ref"))
:custom (lsp-inhibit-message t)
        (lsp-message-project-root-warning t)
        (lsp-enable-file-watchers nil)
        (lsp-enable-completion-at-point t)
        (lsp-prefer-flymake nil)
        (lsp-file-watch-threshold nil)
        (lsp-response-timeout 25)
        (lsp-eldoc-render-all nil)
        (lsp-lens-enable t)
        (lsp-enable-snippet t)
        (lsp-rust-analyzer-server-display-inlay-hints nil)
        (lsp-completion-provider :none) ; with corfu

        ;(lsp-rust-analyzer-cargo-watch-command "clipy")
:init
    (defun my/lsp-mode-setup-completion ()
        (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
            '(orderless)))
:config
    ;(lsp-mode)
    ;corfu + lsp pause bugfix
    (advice-add #'lsp-completion-at-point :around #'cape-wrap-noninterruptible)
    (setq lsp-go-use-gofumpt t)
    (setq lsp-gopls-hover-kind "NoDocumentation")
    (lsp-register-custom-settings
        '(("gopls.staticcheck" t t)
          ("gopls.allExperiments" t t)
          ("gopls.usePlaceholders" t t)
          ("rust-analyzer.cargo.runBuildScript" t t)
          ;("pylsp.plugins.black.enabled" t t)
          ;("pylsp.plugins.ruff.enabled" t t)
          ;("pylsp.plugins.rope_autoimport.enabled" t t)
          ))

    (setq lsp-go-analyses
        '((unusedparams . t)
          (unreachable . t)
          (unusedwrite . t)
          (fieldalignment . t)
          (useany . t)))
    (add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
    (lsp-register-client
        (make-lsp-client
            :new-connection (lsp-stdio-connection '("rnix-lsp"))
            :major-modes '(nix-mode)
            :server-id 'nix))
   ;(setq lsp-go-gopls-placeholders nil)
)

(use-package lsp-ui
:commands lsp-ui-mode
:after  lsp-mode
:general (leader ;"ld"  #'lsp-ui-doc-focus-frame
                 "lpr" #'lsp-ui-peek-find-references
                 "lpd" #'lsp-ui-peek-find-definitions
                 "lpi" #'lsp-ui-peek-find-implementation)
         (:keymaps 'lsp-ui-peek-mode-map
                 "k"   #'lsp-ui-peek--select-prev
                 "j"   #'lsp-ui-peek--select-next)
:custom (scroll-margin 0)
        (lsp-headerline-breadcrumb-icons-enable t)
        (lsp-lens-enable nil)
        (lsp-ui-peek-enable t)
        (lsp-ui-flycheck-enable t)
        (lsp-ui-doc-enable t)
        (lsp-ui-doc-show-with-cursor t)
        (lsp-ui-sideline-enable t)
        (lsp-ui-sideline-show-hover nil)
        (lsp-ui-sideline-actions-icon nil)
        (lsp-ui-sideline-show-code-actions t)
        ;(lsp-ui-sideline-show-diagnostics t)
)

(use-package treemacs  :config (setq treemacs-resize-icons 22))
(use-package treemacs-evil  :after (treemacs evil))
(use-package treemacs-projectile  :after (treemacs projectile))

(use-package lsp-treemacs
:after (lsp-mode doom-modeline)
:config ;(setq lsp-metals-treeview-enable t)
        ;(setq lsp-metals-treeview-show-when-views-received t)
        (lsp-treemacs-sync-mode 1)
)

(use-package dap-mode
:after lsp-mode
:commands (dap-debug)
:general (leader "dd" 'dap-debug)
;:custom (dap-lldb-debug-program '("/Users/nieel/.vscode/extensions/lanza.lldb-vscode-0.2.2/bin/darwin/bin/lldb-vscode"))
:config
    (setq dap-auto-configure-features '(sessions locals controls tooltip))
    (add-hook 'dap-stopped-hook (lambda (arg) (call-interactively #'dap-hydra)))
    ;(require 'dap-gdb-lldb) ; gdb mode
    (require 'dap-go)
    (dap-mode 1)
)

(use-package dap-ui-setting :no-require t :straight nil
:after dap-mode
:preface
  (defun my/window-visible (b-name)
      "Return whether B-NAME is visible."
      (-> (-compose 'buffer-name 'window-buffer)
          (-map (window-list))
          (-contains? b-name)))

  (defun my/show-debug-windows (session)
      "Show debug windows."
      (let ((lsp--cur-workspace (dap--debug-session-workspace session)))
          (save-excursion
          ;; display locals
          (unless (my/window-visible dap-ui--locals-buffer)
              (dap-ui-locals))
          ;; display sessions
          (unless (my/window-visible dap-ui--sessions-buffer)
              (dap-ui-sessions)))))

  (defun my/hide-debug-windows (session)
      "Hide debug windows when all debug sessions are dead."
      (unless (-filter 'dap--session-running (dap--get-sessions))
          (and (get-buffer dap-ui--sessions-buffer)
              (kill-buffer dap-ui--sessions-buffer))
          (and (get-buffer dap-ui--locals-buffer)
              (kill-buffer dap-ui--locals-buffer))))
:config
    (add-hook 'dap-terminated-hook 'my/hide-debug-windows)
    (add-hook 'dap-stopped-hook 'my/show-debug-windows)
)

(use-package lsp-grammarly  :disabled
:hook (text-mode . (lambda () (require 'lsp-grammarly) (lsp)))
)

(use-package consult-lsp )

(provide 'module-lsp)
;;; module-lsp.el ends here
