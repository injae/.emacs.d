(use-package helm :ensure t :pin melpa :diminish helm-mode
;:bind ("M-x" . helm-M-x)
:init (helm-mode 1)
;; helm always bottom
(add-to-list 'display-buffer-alist
            `(,(rx bos "*helm" (* not-newline) "*" eos)
                    (display-buffer-in-side-window)
                    (inhibit-same-window . t)
                    (window-height . 0.4)))

(use-package helm-projectile :ensure t :pin melpa 
:after (helm projectile)
:init (helm-projectile-on)
))

(use-package helm-company :ensure t :pin melpa 
:after helm company
:init
    (define-key company-mode-map   (kbd "C-q") 'helm-company)
    (define-key company-active-map (kbd "C-q") 'helm-company)
)
(use-package helm-descbinds :ensure t :pin melpa 
:after helm
:init (helm-descbinds-mode)
)
(use-package helm-swoop :ensure t :pin melpa 
:after helm
:init (evil-leader/set-key "fw" 'helm-swoop)
)

(use-package helm-ag :ensure t :pin melpa 
:after helm
:init (evil-leader/set-key "fat" 'helm-do-ag-this-file
                            "fab" 'helm-do-ag-buffers
                            "far" 'helm-do-ag-project-root))

(use-package helm-system-packages :ensure t :pin melpa
:init (require 'em-tramp)
        (setq password-cache t)
        (setq password-cache-expiry 3600)
        (evil-leader/set-key "usp" 'helm-system-packages))

(use-package helm-dash :ensure t :pin melpa
:init (evil-leader/set-key "hDs" 'helm-dash
                            "hDi" 'helm-dash-install-user-docset)
)

;(use-package helm-rtags :ensure t :disabled
;:after (helm rtags)
;:config (setq rtags-display-result-backend 'helm))

(use-package helm-flyspell :ensure t :pin melpa :defer t
:after (helm flyspell)
:init (evil-leader/set-key "s" 'helm-flyspell-correct)
)

(use-package helm-smex :ensure t :pin melpa
:after (helm smex)
:bind  ("M-x" . #'helm-smex-major-mode-commands)
:init  (global-set-key [remap execute-extended-command] #'helm-smex)
       (evil-leader/set-key "fm" #'helm-smex-major-mode-commands))
