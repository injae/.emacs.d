
(use-package python-mode :straight t
:mode (("\\.py\\'" . python-mode)
       ("\\.wsgi$" . python-mode))
:interpreter (("python" . python-mode))
:ensure-system-package (;(pyenv . "")
                        (pipenv . "pip install pipenv"))
:custom (python-indent-offset 4)
)

(use-package pipenv :straight t
:after (pyvenv-mode python-mode)
:hook (python-mode . pipenv-mode)
:config (setq pipenv-projectile-after-switch-function #'pipenv-projectile-after-switch-extended)
)

(use-package poetry :straight t :after python
:hook (python-mode . poetry-tracking-mode)
)

;(use-package lsp-pyright :straight t 
;:hook (python-mode . (lambda () (require 'lsp-pyright) (lsp)))
;)

(use-package lsp-python-ms :after python
:straight t
:init (setq lsp-python-ms-auto-install-server t)
:hook (python-mode . (lambda () (require 'lsp-python-ms) (lsp)))
)  ; or lsp-deferred
