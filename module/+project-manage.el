;;; +project-manage.el --- Summery -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package projectile :after (evil exec-path-from-shell)
:config
    ;(setq projectile-require-project-root nil)
	;(setq projectile-enable-caching t)
	;(setq projectile-current-project-on-switch t)
	(setq projectile-globally-ignored-directories
        (append '("^\\eln-cache$" "^\\.emacs.d/straight$") projectile-globally-ignored-directories))
	(evil-ex-define-cmd "kp" 'projectile-kill-buffers)
    (projectile-register-project-type 'bazel '("WORKSPACE")
                                    :project-file "WORKSPACE"
                                    :compile "bazel build"
                                    :test "bazel test"
                                    :run "bazel run")
    (add-hook 'projectile-before-switch-project-hook (lambda () (exec-path-from-shell-initialize)))
    (projectile-mode)
)

(use-package projection :after (evil exec-path-from-shell)
    :hook (after-init . global-projection-hook-mode)
    :config (with-eval-after-load 'project (require 'projection))
    )

(use-package projection-multi :after projection
    :init (define-key global-map "RET" #'projection-multi-compile)
    )

(use-package projection-multi-embark :after (projection-multi embark)
    :config (projection-multi-embark-setup-command-map)
    )

;; (use-package perspective :after projectile
;;     :bind (("C-x b" . persp-switch-to-buffers)
;;            ("C-x k" . persp-kill-buffer*))
;;     ;:general (leader "bs" '(persp-switch-to-buffer* :wk "switch buffer"))
;;     :custom
;;     (persp-mode-prefix-key (kbd "C-c M-p"))
;;     ;(persp-state-default-file (concat user-emacs-directory "var/persp-state"))
;;     :init
;;     (persp-mode)
;;     :config
;;     ;(add-hook 'kill-emacs-hook #'persp-state-save)
;;     ;(persp-state-load persp-state-default-file)
;;     )


; brew install coreutils fd poppler ffmpegthumbnailer mediainfo imagemagick
(use-package dirvish
    ;:ensure-system-package ((fd . "brew install fd"))
    :config (dirvish-override-dired-mode)
    )


(provide '+project-manage)
;;; +project-manage.el ends here
