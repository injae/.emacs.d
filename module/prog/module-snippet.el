;;; module-snippet.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package yasnippet 
;https://github.com/joaotavora/yasnippet
:custom (yas-snippet-dirs '("~/.emacs.d/yas/"))
:general (leader  "hy"  '(:wk "Yasnippet")
                  "hyl" 'consult-yasnippet)
:config (yas-global-mode t)
        (yas-reload-all t)
)

(use-package yasnippet-snippets   :after yasnippet)

(use-package auto-yasnippet 
;https://github.com/abo-abo/auto-yasnippet
:after yasnippet
:general (leader "hyc" 'aya-create
                 "hye" 'aya-expand)
)

(use-package consult-yasnippet )

(provide 'module-snippet)
;;; module-snippet.el ends here
