;;; linum.el --- initialization emacs
;;; Commentry:
;;; linum setting 
;;; Code:

;(add-to-list 'load-path "~/.emacs.d/lisp/linum/linum-highlight-current-line-number/")
;(require 'linum-highlight-current-line-number)
;(setq linum-format 'linum-highlight-current-line-number)
;(global-linum-mode t)

(use-package nlinum :ensure t 
    :init 
    (add-hook 'prog-mode-hook 'nlinum-mode)
    (add-hook 'text-mode-hook 'nlinum-mode)
    (setq nlinum-highlight-current-line t)
    (setq nlinum--width 5)
)
(use-package beacon :ensure t :init (beacon-mode t)) 
(use-package git-gutter-fringe :ensure t)

(use-package git-gutter :ensure t
  :init  
  (global-git-gutter-mode t)
  :config
    (setq git-gutter:lighter " gg")
    (setq git-gutter:window-width 1)
    (setq git-gutter:modified-sign ".")
    (setq git-gutter:added-sign    "+")
    (setq git-gutter:deleted-sign  "-")
    (set-face-foreground 'git-gutter:added    "#daefa3")
    (set-face-foreground 'git-gutter:deleted  "#FA8072")
    (set-face-foreground 'git-gutter:modified "#b18cce")
)
