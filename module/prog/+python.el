;;; +python.el --- Summery
;;; -*- lexical-binding: t; -*-
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
                            ;; (pylsp  . "pip install python-lsp-server[all] && pip install pylsp-mypy python-lsp-black pylsp-rope python-lsp-ruff")
                               )
    :custom (python-indent-offset 4)
    ;; :config
    ;; (setq python-ts-mode-hook python-mode-hook)
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

(use-package jupyter
    :init
    :config
    )


(provide '+python)
;;; +python.el ends here
