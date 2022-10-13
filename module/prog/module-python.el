;;; module-git.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'use-package)

(use-package python-mode
:mode (("\\.py\\'" . python-mode)
       ("\\.wsgi$" . python-mode))
:interpreter (("python" . python-mode))
:ensure-system-package (;(pyenv . "")
                        (pipenv . "pip install pipenv"))
:custom (python-indent-offset 4)
)

(use-package pipenv
:after (pyvenv-mode python-mode)
:hook (python-mode . pipenv-mode)
:config (setq pipenv-projectile-after-switch-function #'pipenv-projectile-after-switch-extended)
)

(use-package python-pytest)

(use-package python-black)

(use-package python-isort)

(use-package pet
  :ensure-system-package (dasel sqlite3)
  :config
  (add-hook 'python-mode-hook
            (lambda () (setq-local python-shell-interpreter (pet-executable-find "python")
                              python-shell-virtualenv-root (pet-virtualenv-root))
              (pet-flycheck-setup)
              (flycheck-mode 1)

              ;(setq-local lsp-jedi-executable-command (pet-executable-find "jedi-language-server"))
              (setq-local lsp-pyright-python-executable-cmd python-shell-interpreter lsp-pyright-venv-path python-shell-virtualenv-root)

              (lsp)
              (setq-local dap-python-executable python-shell-interpreter)

              (setq-local python-pytest-executable (pet-executable-find "pytest"))

              (when-let ((black-executable (pet-executable-find "black")))
                (setq-local python-black-command black-executable)
                (python-black-on-save-mode 1))

              (when-let ((isort-executable (pet-executable-find "isort")))
                (setq-local python-isort-command isort-executable)
                (python-isort-on-save-mode 1)))))

;(use-package poetry  :after python
;:hook (python-mode . poetry-tracking-mode)
;)

(use-package lsp-pyright
;:hook (python-mode . (lambda () (require 'lsp-pyright) (lsp)))
)

(provide 'module-python)
;;; module-python.el ends here
