;;; +python.el --- Summery -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package python-mode :after exec-path-from-shell
    :mode (("\\.py\\'" . python-ts-mode)
           ("\\.wsgi$" . python-ts-mode))
    :preface
    (defun python-formatting-hook ()
        (setq format-all-formatters '(("Python" black))))
    :hook (python-base-mode . python-formatting-hook)
    :ensure-system-package ((pylint . "pip install pylint pylint-strict-informational")
                            (mypy   . "pip install mypy")
                            (flake8 . "pip install flake8")
                            (isort . "pip install isort")
                            (black . "pip install black")
                               )
    :custom (python-indent-offset 4)
    ;; :config
    ;; (setq python-ts-mode-hook python-mode-hook)
    )

(use-package ruff-flycheck :elpaca nil :no-require t :after flycheck :disabled
    :config
    (flycheck-def-config-file-var flycheck-python-ruff-config python-ruff
                                '("pyproject.toml" "ruff.toml" ".ruff.toml"))

    (flycheck-define-checker python-ruff
        "A Python syntax and style checker using the ruff.
        To override the path to the ruff executable, set
        `flycheck-python-ruff-executable'.

        See URL `https://beta.ruff.rs/docs/'."
        :command ("ruff"
                    "check"
                    (config-file "--config" flycheck-python-ruff-config)
                    "--output-format=text"
                    "--stdin-filename" source-original
                    "-")
        :standard-input t
        :error-filter (lambda (errors)
                        (let ((errors (flycheck-sanitize-errors errors)))
                            (seq-map #'flycheck-flake8-fix-error-level errors)))
        :error-patterns
        ((warning line-start
                    (file-name) ":" line ":" (optional column ":") " "
                    (id (one-or-more (any alpha)) (one-or-more digit)) " "
                    (message (one-or-more not-newline))
                    line-end))
        :modes (python-mode python-ts-mode)
        :next-checkers ((warning . python-mypy)))

    (add-to-list 'flycheck-checkers 'python-ruff)
    ;; Python config: Use ruff + mypy.
    (defun python-flycheck-setup ()
        (progn
            (flycheck-select-checker 'python-ruff)
            (flycheck-add-next-checker 'python-ruff 'python-mypy)
            ))
    (add-hook 'python-mode-local-vars-hook #'python-flycheck-setup 'append)
)

(use-package python-pytest)

(use-package pyvenv :after python :disabled
    :functions (pyvenv-tracking-mode)
    :hook (python-base-mode . pyvenv-tracking-mode)
    :config
    (setq pyvenv-virtual-env-name '(".venv" ".virtualenv"))
    )


(use-package poetry :after python
    :functions (poetry-tracking-mode)
    :ensure-system-package ((poetry . "pip install poetry"))
    :hook (python-base-mode . poetry-tracking-mode)
    )

(use-package lsp-pyright :after python
    :hook (python-base-mode .
              (lambda ()
                  (require 'lsp-pyright)
                  (require 'lsp-ruff-lsp)
                  (lsp-deferred)))
    )

(use-package jinja2-mode)

(use-package jupyter)

(provide '+python)
;;; +python.el ends here
