;;; module-search.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package google-this  
:commands google-this
:general (leader "fw" '(google-this :wk "Search Word"))
:config  (google-this-mode 1)
)

;; google translation
(use-package go-translate 
:general (leader "ft" 'gts-do-translate)
:config
    (setq gts-translate-list '(("en" "ko") ("ko" "en") ("jp" "ko") ("ko" "jp")))
    (setq gts-default-translator
        (gts-translator
            :picker (gts-prompt-picker)
            :engines (list (gts-bing-engine) (gts-google-engine))
            :render (gts-buffer-render)))
)

(use-package avy  
:general (leader "jl" '(avy-goto-line :wk "Jump to line")
                 "jw" '(avy-goto-char :wk "Jump to word"))
)

(provide 'module-search)
;;; module-search.el ends here
