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

(use-package poetry  :after python
:hook (python-mode . poetry-tracking-mode)
)

(use-package lsp-pyright  
:hook (python-mode . (lambda () (require 'lsp-pyright) (lsp)))
)

;(use-package lsp-python-ms :after python
;
;:init (setq lsp-python-ms-auto-install-server t)
;:hook (python-mode . (lambda () (require 'lsp-python-ms) (lsp)))
;)  ; or lsp-deferred

(provide 'module-python)
;;; module-python.el ends here
