;;; module-git.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package python-mode
:mode (("\\.py\\'" . python-mode)
       ("\\.wsgi$" . python-mode))
:interpreter (("python" . python-mode))
:ensure-system-package (;(pyenv . "")
                        (pipx . "python3 -m pip install --user pipx && python3 -m pipx ensurepath"))
:custom (python-indent-offset 4)
)

;(use-package pipenv
;:after (pyvenv-mode python-mode)
;:hook (python-mode . pipenv-mode)
;:config (setq pipenv-projectile-after-switch-function #'pipenv-projectile-after-switch-extended)
;)

(use-package python-pytest)

(use-package python-black :after python-mode
    :ensure-system-package ((black . "pip install black"))
    :hook (python-mode . python-black-on-save-mode)
    )

(use-package python-isort :after python
    :ensure-system-package ((isort . "pip install isort"))
    :hook (python-mode . python-isort-on-save-mode)
    )

(use-package poetry :after python
    :ensure-system-package ((poetry . "pipx install poetry")
                            (pylsp  . "pip install python-lsp-server[all] pylsp-mypy python-lsp-black pylsp-rope python-lsp-ruff pylint"))
    :hook ((python-mode . poetry-tracking-mode)
            ;(python-mode . (lambda () (require 'lsp-pylsp) (lsp)))
           ))

;(use-package lsp-mode
;    :custom
;        (lsp-pylsp-plugins-black-enabled t)
;        ;(lsp-pylsp-plugins-yapf-enabled nil) black
;        ;(lsp-pylsp-plugins-autopep8-enabled nil) black
;
;        (lsp-pylsp-plugins-pydocstyle-enabled nil)
;        ;(lsp-pylsp-plugins-pydocstyle-ignore ["D100" "D101" "D102" "D105"])
;
;        (lsp-pylsp-plugins-flake8-enabled t)
;       ;(lsp-pylsp-plugins-flake8-ignore [])
;        (lsp-pylsp-plugins-pyflakes-enabled t)
;        (lsp-pylsp-plugins-pylint-enabled nil)
;        (lsp-pylsp-plugins-rope-completion-enabled t)
;        (lsp-pylsp-plugins-pycodestyle-enabled t)
;

;(use-package pet
;  :ensure-system-package (dasel sqlite3)
;  :config
;  (add-hook 'python-mode-hook
;            (lambda () (setq-local python-shell-interpreter (pet-executable-find "python")
;                              python-shell-virtualenv-root (pet-virtualenv-root))
;              (pet-flycheck-setup)
;              (flycheck-mode 1)
;
;              ;(setq-local lsp-jedi-executable-command (pet-executable-find "jedi-language-server"))
;              (setq-local lsp-pyright-python-executable-cmd python-shell-interpreter lsp-pyright-venv-path python-shell-virtualenv-root)
;
;              (lsp)
;              (setq-local dap-python-executable python-shell-interpreter)
;
;              (setq-local python-pytest-executable (pet-executable-find "pytest"))
;
;              (when-let ((black-executable (pet-executable-find "black")))
;                (setq-local python-black-command black-executable)
;                (python-black-on-save-mode 1))
;
;              (when-let ((isort-executable (pet-executable-find "isort")))
;                (setq-local python-isort-command isort-executable)
;                (python-isort-on-save-mode 1)))))
;(use-package poetry  :after python
;:hook (python-mode . poetry-tracking-mode)
;)

(use-package lsp-pyright
    :hook (python-mode . (lambda () (require 'lsp-pyright) (lsp)))
)

(provide 'module-python)
;;; module-python.el ends here
