;;; +docker.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package docker
:commands docker
:general (leader "hud" 'docker)
:custom (docker-image-run-arguments '("-i", "-t", "--rm"))
)

(use-package dockerfile-mode
:mode ("Dockerfile\\'" . dockerfile-mode)
)

(use-package docker-compose-mode)

(use-package kubernetes
    :commands (kubernetes-overview)
    :config
    (setq kubernetes-poll-frequency 3600)
    (setq kubernetes-redraw-frequency 3600)
    )

(use-package kubernetes-evil :after (kubernetes evil))

(use-package kubel :after vterm
    :commands (kubel)
    :config (kubel-vterm-setup)
    )
(use-package kubel-evil :after (kubel evil))



(use-package kubedoc)

(provide '+docker)
;;; +docker.el ends here
