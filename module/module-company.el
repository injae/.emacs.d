;;; module-company.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

; 오직 company-complete-selection으로 만 해야지 snippet 자동완성이 작동됨
(use-package company
:init (global-company-mode 1)
:config
    (company-tng-mode t)
    (setq company-show-quick-access t)
    (setq company-idle-delay 0)
    (setq company--transform-candidates nil)
    (setq company-minimum-prefix-length 1)
    (setq company-tooltip-align-annotations nil)
    (setq company-dabbrev-downcase nil)
    ;(add-to-list 'company-backends #'company-capf)
    ;(add-to-list 'company-backends '(company-capf :with company-yasnippet))
)

(use-package company-statistics  
:after company
:config (company-statistics-mode)
)

;company-quickhelp speed up setting
(use-package company-posframe  
:after company
:config (company-posframe-mode 1)
)

(use-package company-suggest 
:config (setq company-suggest-complete-sentence t)
        (add-to-list 'company-backend 'company-suggest-google)
)


(use-package company-box  :diminish ""
:after company-mode
:hook   (company-mode . company-box-mode)
:custom (company-box-max-candidates 30)
:config (setq company-box-icons-unknown 'fa_question_circle)
        (setq company-box-color-icon t)
        (setq company-box-backends-colors nil)
        (setq company-box-icons-yasnippet 'fa_bookmark)
        (setq company-box-icons-lsp
            '((1  . fa_text_height) ;; Text
              (2  . (fa_tags :face font-lock-function-name-face)) ;; Method
              (3  . (fa_tag  :face font-lock-function-name-face)) ;; Function
              (4  . (fa_tag  :face font-lock-function-name-face)) ;; Constructor
              (5  . (fa_cog  :foreground "#FF9800")) ;; Field
              (6  . (fa_cog  :foreground "#FF9800")) ;; Variable
              (7  . (fa_cube :foreground "#7C4DFF")) ;; Class
              (8  . (fa_cube :foreground "#7C4DFF")) ;; Interface
              (9  . (fa_cube :foreground "#7C4DFF")) ;; Module
              (10 . (fa_cog  :foreground "#FF9800")) ;; Property
              (11 . md_settings_system_daydream) ;; Unit
              (12 . (fa_cog  :foreground "#FF9800")) ;; Value
              (13 . (md_storage :face font-lock-type-face)) ;; Enum
              (14 . (md_closed_caption :foreground "#009688")) ;; Keyword
              (15 . md_closed_caption) ;; Snippet
              (16 . (md_color_lens :face font-lock-doc-face)) ;; Color
              (17 . fa_file_text_o) ;; File
              (18 . md_refresh) ;; Reference
              (19 . fa_folder_open) ;; Folder
              (20 . (md_closed_caption :foreground "#009688")) ;; EnumMember
              (21 . (fa_square :face font-lock-constant-face)) ;; Constant
              (22 . (fa_cube :face font-lock-type-face)) ;; Struct
              (23 . fa_calendar) ;; Event
              (24 . fa_square_o) ;; Operator
              (25 . fa_arrows)) ;; TypeParameter
            )
        ;(company-box-show-single-candidate t)
        ;(setq company-box-icons-alist 'company-box-icons-all-the-icons)
        ;(company-box-doc-delay 0.5)
)

(use-package company-c-headers  
:after  (company c++-mode)
:config (add-to-list 'company-backends 'company-c-headers)
)

(use-package company-shell 
:after (company eshell)
:init (add-to-list 'company-backends '(company-shell company-shell-env company-fish-shell))
)

(use-package company-restclient  
:after  (company restclient)
:config (add-to-list 'company-backends 'company-restclient)
)

(provide 'module-company)
;;; module-company.el ends here
