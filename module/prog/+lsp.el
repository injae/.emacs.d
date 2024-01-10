;;; +lsp.el --- Summery -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package lsp-mode
;:after (exec-path-from-shell projectile)
:commands (lsp lsp-deferred)
:custom (lsp-inhibit-message t)
        (lsp-message-project-root-warning t)
        (lsp-enable-file-watchers nil)
        (lsp-file-watch-threshold 10000)
        (lsp-enable-completion-at-point t)
        (lsp-prefer-flymake nil)
        (lsp-auto-guess-root t)
        (lsp-response-timeout 25)
        (lsp-eldoc-render-all nil)
        (lsp-lens-enable t)
        (lsp-enable-snippet t)
        (lsp-idle-delay 0.500)
        (lsp-log-io t)
        (lsp-rust-analyzer-server-display-inlay-hints nil)
        (lsp-headerline-breadcrumb-enable-diagnostics nil)
        (lsp-completion-provider :none) ; with corfu
        ;(lsp-rust-analyzer-cargo-watch-command "clipy")
:init
    (defun my/lsp-mode-setup-completion ()
        (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
            '(orderless)))
:hook ((lsp-completion-mode . my/lsp-mode-setup-completion)
       (lsp-mode            . lsp-enable-which-key-integration))
:config
    ;(lsp-mode)
    ;;corfu + lsp pause bugfix
    ;; (advice-add #'lsp-completion-at-point :around #'cape-wrap-noninterruptible)
    (dolist (dir '("[/\\\\]\\.ccls-cache\\'"
                    "[/\\\\]\\.mypy_cache\\'"
                    "[/\\\\]\\.pytest_cache\\'"
                    "[/\\\\]\\.cache\\'"
                    "[/\\\\]\\.clwb\\'"
                    "[/\\\\]__pycache__\\'"
                    "[/\\\\]bazel-bin\\'"
                    "[/\\\\]bazel-code\\'"
                    "[/\\\\]bazel-genfiles\\'"
                    "[/\\\\]bazel-out\\'"
                    "[/\\\\]bazel-testlogs\\'"
                    "[/\\\\]third_party\\'"
                    "[/\\\\]third-party\\'"
                    "[/\\\\]buildtools\\'"
                    "[/\\\\]out\\'"
                    "[/\\\\]build\\'"
                    ))
        (push dir lsp-file-watch-ignored-directories))
    (setq lsp-pyright-multi-root nil)
    (setq lsp-go-use-gofumpt t)
    (setq lsp-gopls-hover-kind "NoDocumentation")
    (lsp-register-custom-settings
        '(("gopls.staticcheck" t t)
          ("gopls.allExperiments" t t)
          ;;("gopls.usePlaceholders" t t)
          ("rust-analyzer.cargo.runBuildScript" t t)
          ;;("pylsp.plugins.black.enabled" t t)
          ;;("pylsp.plugins.ruff.enabled" t t)
          ;;("pylsp.plugins.rope_autoimport.enabled" t t)
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
    (setq lsp-go-gopls-placeholders nil)
)

(use-package lsp-ui :after lsp-mode
:commands lsp-ui-mode
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

(use-package treemacs :disabled :config (setq treemacs-resize-icons 22))
(use-package treemacs-evil :after (treemacs evil))
(use-package treemacs-projectile :after (treemacs projectile))

(use-package lsp-treemacs :disabled
:after (lsp-mode doom-modeline)
:config ;(setq lsp-metals-treeview-enable t)
        ;(setq lsp-metals-treeview-show-when-views-received t)
        (lsp-treemacs-sync-mode 1)
)

(use-package dap-mode :disabled
:after lsp-mode
:commands (dap-debug)
:general (leader "dd" 'dap-debug)
;:custom (dap-lldb-debug-program '("/Users/nieel/.vscode/extensions/lanza.lldb-vscode-0.2.2/bin/darwin/bin/lldb-vscode"))
:config
    (setq dap-auto-configure-features '(sessions locals controls tooltip))
    (add-hook 'dap-stopped-hook (lambda (arg) (call-interactively #'dap-hydra)))
    (dap-mode)
)

(use-package dap-ui-setting :no-require t :elpaca nil
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

(use-package lsp-grammarly :disabled
:hook (text-mode . (lambda () (require 'lsp-grammarly) (lsp)))
)

(use-package consult-lsp :requires (lsp-mode consult)
    :after (lsp-mode consult)
    :bind (:map lsp-mode-map
            ([remap xref-find-apropos] . consult-lsp-symbols))
    :general (leader
                 "ls" 'consult-lsp-symbols
                 "lf" 'consult-lsp-file-symbols
                 "ld" 'consult-lsp-diagnostics)
    )

(use-package eglot :after (exec-path-from-shell projectile)
    :preface
    (defun my/eglot-ensure ()
        (exec-path-from-shell-initialize)
        (eglot-ensure))
    :hook (
          (rust-mode . my/eglot-ensure)
          (go-mode   . my/eglot-ensure)
          (python-base-mode . my/eglot-ensure)
        ;; (nix-mode . eglot-ensure)
        ;; (js-mode . eglot-ensure)
        ;; (js2-mode . eglot-ensure)
        ;; (typescript-mode . eglot-ensure)
        ;; (web-mode . eglot-ensure)
        ;; (css-mode . eglot-ensure)
        ;; (scss-mode . eglot-ensure)
        ;; (json-mode . eglot-ensure)
        ;; (yaml-mode . eglot-ensure)
        ;; (dockerfile-mode . eglot-ensure)
          )
    :config
    (setq-default eglot-workspace-configuration
        '(:pylsp
             (:plugins
                 (:mypy (:enabled t)
                  :ruff (:enabled t)
                  ;; :rope_autoimport (:enabled t (:code_actions: (:enabled: t)))
                 ))
          ;; :gopls
          ;;     (:usePlaceholders t)
        ))
    )

(use-package flycheck-eglot :after (flycheck eglot)
    :functions (flycheck-eglot-mode)
    :config (flycheck-eglot-mode)
    )

(use-package eldoc-box :after eglot
    :hook (eglot-managed-mode . eldoc-box-hover-at-point-mode)
          ;(eglot-managed-mode . eldoc-box-hover-mode)
    :custom (eldoc-box-clear-with-C-g t)
            (eldoc-box-offset 1)
    :config (add-to-list 'eldoc-box-frame-parameters '(alpha . 0.80))
    )

(use-package eglot-x :elpaca (:host github :repo "nemethf/eglot-x")
    :after eglot
    :config (eglot-x-setup)
    )

(use-package consult-eglot :after eglot)

(use-package lsp-key-map :no-require t :elpaca nil :after (:any lsp-mode eglot)
:preface
    (defun lsp-key-mapper (lsp-func eglot-func)
        (if (and (bound-and-true-p lsp-mode) (fboundp lsp-func))
            (call-interactively lsp-func)
            (if (and (bound-and-true-p eglot--managed-mode) (fboundp eglot-func))
                (call-interactively eglot-func)
            (message "No LSP client available for code actions"))))

    (defun lsp-key-map-code-action ()
        (interactive)
        (lsp-key-mapper 'lsp-execute-code-action 'eglot-code-actions)
        )

    (defun lsp-key-map-find-definition ()
        (interactive)
        (lsp-key-mapper 'lsp-find-definition 'eglot-find-declaration)
        )

    (defun lsp-key-map-find-implementation ()
        (interactive)
        (lsp-key-mapper 'lsp-find-implementation 'eglot-find-implementation)
        )

    (defun lsp-key-map-find-references ()
        (interactive)
        (lsp-key-mapper 'lsp-find-references 'xref-find-references)
        )

    (defun lsp-key-map-find-typeDefinition ()
        (interactive)
        (lsp-key-mapper 'lsp-find-typeDefinition 'eglot-find-typeDefinition)
        )

    (defun lsp-key-map-rename ()
        (interactive)
        (lsp-key-mapper 'lsp-rename 'eglot-rename)
        )
    
:general (leader "hh" '(lsp-key-map-code-action         :wk "wizard")
                 "pp" '(xref-go-back                    :wk "lsp pop")
                 "fd" '(lsp-key-map-find-definition     :wk "lsp define")
                 "fT" '(lsp-key-map-find-typeDefinition :wk "lsp type")
                 "fi" '(lsp-key-map-find-implementation :wk "lsp impl")
                 "fr" '(lsp-key-map-find-references     :wk "lsp ref")
                 "lr" '(lsp-key-map-rename              :wk "lsp rename")
        )
)


(provide '+lsp)
;;; +lsp.el ends here
