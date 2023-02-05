;;; +python.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package python-mode :straight nil
    :mode (("\\.py\\'" . python-mode)
           ("\\.wsgi$" . python-mode))
    :custom (python-indent-offset 4)
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
    :hook ((python-mode . poetry-tracking-mode))
)

(use-package lsp-pyright
    :hook (python-mode . (lambda () (require 'lsp-pyright) (lsp-deferred)))
)

;; (use-package lsp-mode
;;     :custom
;;         (lsp-pylsp-plugins-black-enabled t)
;;         ;(lsp-pylsp-plugins-yapf-enabled nil) black
;;         ;(lsp-pylsp-plugins-autopep8-enabled nil) black
;; 
;;         (lsp-pylsp-plugins-pydocstyle-enabled nil)
;;         ;(lsp-pylsp-plugins-pydocstyle-ignore ["D100" "D101" "D102" "D105"])
;; 
;;         (lsp-pylsp-plugins-flake8-enabled t)
;;        ;(lsp-pylsp-plugins-flake8-ignore [])
;;         (lsp-pylsp-plugins-pyflakes-enabled t)
;;         (lsp-pylsp-plugins-pylint-enabled nil)
;;         (lsp-pylsp-plugins-rope-completion-enabled t)
;;         (lsp-pylsp-plugins-pycodestyle-enabled t)
;; 

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
