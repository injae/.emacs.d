;;; +edit.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package indent4-mode :elpaca nil :no-require t
:preface
    (defun my-set-indent (n)
        (setq-default tab-width n)
        ;(electric-indent-mode n)
        (setq-default c-basic-offset n)
        (setq lisp-indent-offset n)
        (setq indent-line-function 'insert-tab)
    )
    (defun un-indent-by-removing-4-spaces ()
        "back tab"
        (interactive)
        (save-excursion
        (save-match-data
        (beginning-of-line)
        ;; get rid of tabs at beginning of line
        (when (looking-at "^\\s-+")
        (untabify (match-beginning 0) (match-end 0)))
            (when (looking-at "^    ")
                (replace-match "")))
            )
    )
:config
    (global-set-key (kbd "<backtab>") 'un-indent-by-removing-4-spaces)
    (electric-indent-mode nil)
    (my-set-indent 4)
    (setq-default indent-tabs-mode nil)
)

(use-package paren :elpaca nil
:init   (show-paren-mode 0)
        (electric-pair-mode 0)
:config (setq show-paren-delay 0)
)

(use-package expand-region
:general (leader "tw" '(er/expand-region :wk "Text Wrap"))
)

(use-package smartparens
;:general (leader "pr " 'sp-rewrap-sexp
;                 "pll" 'sp-forward-slurp-sexp
;                 "phh" 'sp-backward-slurp-sexp
;                 "plh" 'sp-forward-barf-sexp
;                 "phl" 'sp-backward-barf-sexp)
:config (smartparens-global-mode)
        (show-smartparens-global-mode)
)
;elisp double quote problem fix setting
(use-package smartparens-config :after smartparens :elpaca nil)

;(use-package aggressive-indent   :disabled
;; https://github.com/Malabarba/aggressive-indent-mode
;:config (electric-indent-mode nil)
;;exclud mode
;;(add-to-list 'aggresive-indent-excluded-modes 'html-mode)
;)
;
;(use-package highlight-indentation  :disabled
;:hook (prog-mode . highlight-indentation-mode)
;)

(provide '+edit)
;;; +edit.el ends here
