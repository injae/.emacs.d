;;; toggle.el --- initialization emacs
;;; Commentry:
;;; can use toogle key 
;;; Code:
; toggle key setting
(load-library "hideshow")
    (global-set-key (kbd "<C-right>") 'hs-show-block)
    (global-set-key (kbd "<C-left>")  'hs-hide-block)
    (add-hook 'c-mode-common-hook     'hs-minor-mode)
    (add-hook 'emacs-lisp-mode-hook   'hs-minor-mode)
    (add-hook 'java-mode-hook         'hs-minor-mode)
    (add-hook 'lisp-mode-hook         'hs-minor-mode)
    (add-hook 'perl-mode-hook         'hs-minor-mode)
    (add-hook 'sh-mode-hook           'hs-minor-mode)


