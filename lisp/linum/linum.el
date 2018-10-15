;;; linum.el --- initialization emacs
;;; Commentry:
;;; linum setting 
;;; Code:

;(setq display-line-numbers-current-absolute t)
;(add-to-list 'load-path "~/.emacs.d/lisp/linum/linum-highlight-current-line-number/")
;(require 'linum-highlight-current-line-number)
;(setq linum-format 'linum-highlight-current-line-number)

(use-package beacon :ensure t :init (beacon-mode t)) 

(use-package git-gutter :ensure t
  :init  
  (global-git-gutter-mode t)
  (setq-default display-line-numbers-width 2)
  (global-display-line-numbers-mode t)
  (global-hl-line-mode t)
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

;;
