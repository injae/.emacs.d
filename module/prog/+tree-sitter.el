;;; +tree-sitter.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package tree-sitter
  :hook ((emacs-startup . global-tree-sitter-mode)
         (tree-sitter-after-on . tree-sitter-hl-mode))
)

;; 
(use-package tree-sitter-langs :after tree-sitter)
(use-package tree-sitter-indent :after tree-sitter :disabled
  :hook (tree-sitter-after-on . tree-sitter-indent-mode)
  )
;(use-package tsi :elpaca (:type git :host github :repo "orzechowskid/tsi.el") :after tree-sitter :disabled)

(use-package evil-textobj-tree-sitter :after (evil tree-sitter)
  :config
  ;; bind `function.outer`(entire function block) to `f` for use in things like `vaf`, `yaf`
  (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
  ;; bind `function.inner`(function block without name and args) to `f` for use in things like `vif`, `yif`
  (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))

  ;; You can also bind multiple items and we will match the first one we can find
  (define-key evil-outer-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer")))
  )


;; (use-package ts-fold :disabled
;;     :elpaca (ts-fold :type git :host github :repo "emacs-tree-sitter/ts-fold")
;;     :general (leader "<tab>" #'ts-fold-toggle)
;;     :custom ((ts-fold-indicators-fringe 'left-fringe)
;;              (ts-fold-summary-show t)
;;              (ts-fold-indicators-priority 100))
;;     :config
;;     (add-hook 'tree-sitter-after-on-hook 'ts-fold-mode)
;;     (add-hook 'tree-sitter-after-on-hook 'ts-fold-indicators-mode)
;; )

(use-package treesit-auto :disabled
    ;:custom (treesit-auto-install 'prompt)
    :config
    ;; (treesit-auto-add-to-auto-mode-alist 'all)
    (global-treesit-auto-mode)
    )

(use-package treesit :elpaca nil
    :if (treesit-available-p)
    :config
    (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (make "https://github.com/alemuller/tree-sitter-make")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
    (setq treesit-font-lock-level 4)
)


;; (use-package treesit-langs :elpaca (:host github :repo "kiennq/treesit-langs");:files (:defaults "*")); "queries"))
;;     :after treesit
;;     :hook (prog-mode .
;;               (lambda () (ignore-errors (treesit-hl-toggle))))
;;     :custom
;;     (treesit-langs-grammar-dir (expand-file-name ".cache/tree-sitter/" user-emacs-directory))
;;     (treesit-langs-git-dir nil)
;;     :config (treesit-lang--setup)
;; )

(provide '+tree-sitter)
;;; +tree-sitter.el ends here
