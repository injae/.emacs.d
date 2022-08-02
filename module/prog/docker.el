(use-package docker :straight t  
:commands docker
:general (leader "hud" 'docker)
:custom (docker-image-run-arguments '("-i", "-t", "--rm"))
)

(use-package dockerfile-mode :straight t 
:mode ("Dockerfile\\'" . dockerfile-mode)
)

(use-package kubernetes :straight t :commands (kubernetes-overview))

;; If you want to pull in the Evil compatibility package.
(use-package kubernetes-evil :straight t :after kubernetes)

(use-package k8s-mode :straight t
:hook (k8s-mode . yas-minor-mode)
)

(use-package docker-compose-mode :straight t)
