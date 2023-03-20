;;; +python.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package python-mode :after exec-path-from-shell
    :mode (("\\.py\\'" . python-mode)
           ("\\.wsgi$" . python-mode))
    :preface
    (defun python-formatting-hook ()
        (setq format-all-formatters '(("Python" isort black))))
    :hook (python-base-mode . python-formatting-hook)
    :config
    (setq python-indent-offset 4)
    (setq python-ts-mode-hook python-mode-hook)
    )


(use-package python-pytest)

(use-package poetry :after python
    :ensure-system-package ((poetry . "pip install poetry")
                            (pylint . "pip install pylint pylint-strict-informational")
                            (mypy   . "pip install mypy")
                            (flake8 . "pip install flake8")
                            (isort . "pip install isort")
                            (black . "pip install black")
                            ;; (pylsp  . "pip install python-lsp-server[all] && pip install pylsp-mypy python-lsp-black pylsp-rope python-lsp-ruff")
                               )
    :hook (python-base-mode . poetry-tracking-mode)
    )

(use-package lsp-pyright
    :hook (python-base-mode . (lambda () (require 'lsp-pyright) (lsp-deferred)))
    )

;; (use-package python-black :after python-mode
;;     :ensure-system-package ((black . "pip install black"))
;;     :hook (python-mode . python-black-on-save-mode)
;;     )
;;
;; (use-package python-isort :after python
;;     :ensure-system-package ((isort . "pip install isort"))
;;     :hook (python-mode . python-isort-on-save-mode)
;;     )


(provide '+python)
;;; +python.el ends here
