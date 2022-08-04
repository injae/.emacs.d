(use-package eshell 
:commands eshell
:config (setq eshell-buffer-maximum-lines 1000)
        ;(require 'xterm-color)
        (add-hook 'eshell-mode-hook (lambda () (setq pcomplete-cycle-completions     nil)))
        ;(add-hook 'eshell-mode-hook (lambda () (setq xterm-color-preserve-properties t) (setenv "TERM" "xterm-256color")))
        (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
        (setq eshell-output-filter-functions (remove 'eshell-handle-asni-color eshell-output-filter-functions))
        (setq eshell-cmpl-cycle-completions nil)
)

(use-package eshell-did-you-mean :straight t 
:after  eshell
:config (eshell-did-you-mean-setup)
)

(use-package esh-help :straight t 
:after (eshell eldoc)
:config (setup-esh-help-eldoc)
)

(use-package eshell-prompt-extras :straight (:build (:not compile))
:after eshell
:config
    (autoload 'epe-theme-lambda   "eshell-prompt-extras")
    (setq eshell-highlight-prompt nil)
    (setq eshell-prompt-function  'epe-theme-lambda)
)

(use-package fish-completion :straight t 
:after eshell
:config (when (and (executable-find "fish")
                   (require 'fish-completion nil t))
              (global-fish-completion-mode))
)

(use-package esh-autosuggest :straight t 
:after eshell
:hook (eshell-mode . esh-autosuggest-mode)
)

(use-package eshell-up :straight t :disabled
:after eshell
:config (add-hook 'eshell-mode-hook (lambda () (eshell/alias "up" "eshell-up $1")
                                          (eshell/alias "pk" "eshell-up-peek $1")))
)
