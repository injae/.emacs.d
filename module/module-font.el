;;; module-font.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; font Setting
;; +------------+------------+
;; | 일이삼사오 | 일이삼사오 |
;; +------------+------------+
;; | ABCDEFGHIJ | ABCDEFGHIJ |
;; +------------+------------+
;; | 1234567890 | 1234567890 |
;; +------------+------------+
;; | 일이삼사오 | 일이삼사오 |
;; | abcdefghij | abcdefghij |
;; +------------+------------+
;; text utf-8 setting
;(setq utf-translate-cjk-mode nil)
(set-language-environment "Korean")
(prefer-coding-system 'utf-8)
(setq locale-coding-system   'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq system-time-locale "C")


; some font use mode speed up config (ex: org-superstar)
(setq inhibit-compacting-font-caches t)
; NanumGothicCoding Setting
(set-face-attribute   'default            nil       :family "Fira Code" :height 130)
;(set-face-attribute   'default           nil        :family "FiraCode Nerd Font Mono" :height 130)
(set-fontset-font nil 'hangul            (font-spec :family "NanumGothicCoding"  :pixelsize 17))
(set-fontset-font nil 'japanese-jisx0208 (font-spec :family "NanumGothicCoding"  :pixelsize 17))
(setq face-font-rescale-alist '(("NanumGothicCoding" . 1.2)))

; D2Coding Setting
;(set-face-attribute   'default            nil       :family "Fira Code" :height 120)
;(set-fontset-font nil 'hangul            (font-spec :family "D2Coding"  :pixelsize 18))
;(set-fontset-font nil 'japanese-jisx0208 (font-spec :family "D2Coding"  :pixelsize 18))
;(setq face-font-rescale-alist '(("D2coding" . 1.17)))

;(set-face-attribute   'default            nil       :family "FiraCode Nerd Font Mono" :height 120)
;(setq face-font-rescale-alist '(("D2coding" . 1.03877)))
;(setq face-font-rescale-alist '(("D2coding" . 0.85)))

(when *is-mac*
    (progn
        (require 'ucs-normalize)
        (set-file-name-coding-system 'utf-8-hfs)
        (setq default-process-coding-system '(utf-8-hfs . utf-8-hfs))
        (set-terminal-coding-system  'utf-8-hfs)
        ))

(setq-default line-spacing 3)

(global-font-lock-mode t)

;; 한글입력할때 완성전까지 안보이는 문제 해결을 위해 내장 한글입력기 사용
;; Linux 내장 한글입력기 사용법

; 터미널에 xrdb ~/.Xresources 하고 xrdb -merge ~/.Xresources 하고 이맥스 다시키면 됨
(setq default-input-method "korean-hangul")
(setq default-korean-keyboard 'korean-hangul)
;(global-set-key [S-SPC] 'toggle-input-method) ; Ivy모드를 사용하면 S-SPC를 ivy-minibuffer-map에서 remapping 해줘야 한다.
(global-set-key [?\S- ] 'toggle-input-method) ; Ivy모드를 사용하면 S-SPC를 ivy-minibuffer-map에서 remapping 해줘야 한다.
(global-set-key (kbd "S-SPC") 'toggle-input-method) ; Ivy모드를 사용하면 S-SPC를 ivy-minibuffer-map에서 remapping 해줘야 한다.
(global-set-key (kbd "<f17>") 'toggle-input-method) ; macos shift-space setting Karabiner를 사용해야된다.
(global-set-key (kbd "<Hangul>") 'toggle-input-method)

(use-package default-text-scale :straight t
:config (default-text-scale-mode)
        ;(if *is-wsl* (default-text-scale-increment 20))
        ;(if *is-wsl* (default-text-scale-increment 45))
)

; FiraCode같은 텍스트모드 활성 모드
(use-package ligature :straight (:host github :repo "mickeynp/ligature.el")
:config
; Enable the www ligature in every possible major mode
(ligature-set-ligatures 't '("www"))
(ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
; Enable ligatures in programming modes                                                           
(ligature-set-ligatures '(prog-mode org-mode)
        '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
          ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
          "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
          "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
          "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
          "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
          "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
          "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
          "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
          "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))
(global-ligature-mode t)
)

(use-package emojify :straight t 
:if window-system
:config 
        (setq emojify-display-style 'image)
        ;(setq emojify-emoji-styles  '(unicode))
        ;(setq emojify-emoji-set "emojione-v2.2.6")
        (global-emojify-mode 1)
)

;(use-package textsize :load-path "lisp/textsize")

(provide 'module-font)
;;; module-font.el ends here
