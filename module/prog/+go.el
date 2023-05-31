;;; +git.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package go-mode
    :ensure-system-package ((gopls . "go install golang.org/x/tools/gopls@latest")
                            (godef . "go install github.com/rogpeppe/godef@latest")
                            (gofumpt . "go install mvdan.cc/gofumpt@latest"))
    :mode ("\\.go\\''" . go-mode)
    :preface
    (defun go-formatting-hook () (setq format-all-formatters '("Go")))
    (defun lsp-go-install-save-hooks ()
        (add-hook 'before-save-hook #'lsp-format-buffer)
        (add-hook 'before-save-hook #'lsp-organize-imports))



    :hook ((go-mode . (lambda () (lsp-deferred)))
           (go-mode . lsp-go-install-save-hooks)
           (go-mode . go-formatting-hook)
           (go-ts-mode . (lambda () (lsp-deferred)))
           (go-ts-mode . lsp-go-install-save-hooks)
           (go-ts-mode . go-formatting-hook))
    :config
        (require 'dap-dlv-go)
        ;(add-hook 'before-save-hook 'gofmt-before-save)
)

(use-package dap-go :ensure dap-mode :after go-mode :disabled
:config (dap-go-setup)
)

;(defvar-local flycheck-local-checkers nil)
;(defun +flycheck-checker-get(fn checker property)
;    (or (alist-get property (alist-get checker flycheck-local-checkers))
;        (funcall fn checker property)))
;(advice-add 'flycheck-checker-get :around '+flycheck-checker-get)

(use-package flycheck-golangci-lint :after flycheck
    :ensure-system-package ((golangci-lint . "curl -sSfL https://raw.githubusercontent.com/golangci/golangci-lint/master/install.sh | sh -s -- -b $(go env GOPATH)/bin v1.52.0")
                            (gocritic . "go install github.com/go-critic/go-critic/cmd/gocritic@latest")
                            (revive . "go install github.com/mgechev/revive@latest")
                            (unparam . "go install mvdan.cc/unparam@latest")
                            (unused . "go install honnef.co/go/tools/cmd/unused@latest")
                            (ineffassign . "go install github.com/gordonklaus/ineffassign@latest")
                            (goconst . "go install github.com/jgautheron/goconst/cmd/goconst@latest")
                            (staticcheck . "go install honnef.co/go/tools/cmd/staticcheck@latest"))
    ;; :custom (flycheck-golangci-lint-enable-linters
    ;;             '("gocritic" "revive" "unparam" "unused" "stylecheck" "ineffassign" "goconst")) ; "misspell"
    ;;         (flycheck-golangci-lint-disable-linters '("structcheck" "goimports"))
    ;; :custom (flycheck-golangci-lint-config "~/.emacs.d/.golangci.yaml")
    :functions flycheck-golangci-lint-setup
    :preface
        (defvar-local flycheck-local-checkers nil)
        (defun +flycheck-checker-get(fn checker property)
            (or (alist-get property (alist-get checker flycheck-local-checkers))
                (funcall fn checker property)))
        (advice-add 'flycheck-checker-get :around '+flycheck-checker-get)
    :config
        (add-hook 'go-ts-mode-hook (lambda()
                                    (flycheck-golangci-lint-setup)
                                    (setq flycheck-local-checkers '((lsp . ((next-checkers . (golangci-lint))))))))
        (add-hook 'go-mode-hook (lambda()
                                    (flycheck-golangci-lint-setup)
                                    (setq flycheck-local-checkers '((lsp . ((next-checkers . (golangci-lint))))))))

)

;; :go-tag-add xml db
;; :go-tag-add json,omitempty
(use-package go-tag :after go-mode
:ensure-system-package (gomodifytags . "go install github.com/fatih/gomodifytags@latest")
)

(use-package go-impl :load-path "lisp/go-impl" :after go-mode
:ensure-system-package ((impl . "go install github.com/josharian/impl@latest")
                        (godoc . "go install golang.org/x/tools/cmd/godoc@latest"))
)

(use-package go-fill-struct :after go-mode
:ensure-system-package (fillstruct . "go install github.com/davidrjenni/reftools/cmd/fillstruct@latest")
)

(use-package go-gen-test :after go-mode
:ensure-system-package (gotests . "go install github.com/cweill/gotests/...@latest")
)

(use-package gotest :after go-mode)

(use-package go-errcheck :after go-mode
:ensure-system-package (errcheck . "go install github.com/kisielk/errcheck@latest")
)

(provide '+go)
;;; +go.el ends here
