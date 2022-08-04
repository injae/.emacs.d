(use-package cpp-mode ;:load-path "lisp/cpp-mode"
:no-require t
:straight nil
:mode (("\\.h\\'"   . c++-mode)
       ("\\.hpp\\'" . c++-mode))
;:commands cpp-mode
:general (leader "hc" '(:wk "C/C++"))
;:hook (c-mode-common . 'cpp-mode)
:init (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
;      (add-hook 'c++-mode-hook  'cpp-mode)
;      (add-hook 'c-mode-hook    'cpp-mode)
;      (add-hook 'objc-mode-hook 'cpp-mode)
)

(use-package ccls :straight t 
:hook  ((c-mode c++-mode objc-mode cuda-mode c-mode-common) . (lambda () (require 'ccls) (lsp)))
:config
    (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
    ;(setq ccls-sem-highlight-method 'font-lock)
    ;(ccls-use-default-rainbow-sem-highlight)
    (setq ccls-extra-init-params '(:client (:snippetSupport :json-false)))
    (setq ccls-executable "ccls")
    (setq ccls-initialization-options '(:compilationDatabaseDirectory "build/" ))
)

(use-package cppm :no-require t :straight nil
:after c++-mode
:general (leader "hcb" (lambda () (eshell-command "cppm build"))
                 "hcr" (lambda () (eshell-command "cppm run  ")))
)

(use-package clang-format :straight t 
:after  (c++-mode)
:init   (add-hook 'c++-mode-hook 'clang-format)
:general (leader "hccf" 'clang-format-regieon)
)

; only c/c++
(use-package disaster :straight t  :commands disaster)

(use-package quickrun :straight t
:general (leader "qr" #'quickrun)
:config
    (quickrun-add-command "c++/c1z"
        '((:command . "cppm")
          (:exec "%c build")
           :defualt "c++"))
)
