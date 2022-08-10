;;; module-snippet.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package yasnippet :straight t
;https://github.com/joaotavora/yasnippet
:custom (yas-snippet-dirs '("~/.emacs.d/yas/"))
:general (leader  "hy"  '(:wk "Yasnippet")
                  "hyl" 'consult-yasnippet)
:config (yas-global-mode t)
        (yas-reload-all t)
)

(use-package yasnippet-snippets :straight t  :after yasnippet)

(use-package auto-yasnippet :straight t
;https://github.com/abo-abo/auto-yasnippet
:after yasnippet
:general (leader "hyc" 'aya-create
                 "hye" 'aya-expand)
)

(use-package consult-yasnippet :straight t)

(provide 'module-snippet)
;;; module-snippet.el ends here