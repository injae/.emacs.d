;;; module-evil.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'use-package)
(require 'straight)

(use-package evil :straight t
:init (setq evil-want-keybinding nil)
      ;(setq evil-want-integration t)
:config (setq evil-want-C-u-scroll t)
        (setq evil-symbol-word-search t)
        (define-key evil-normal-state-map (kbd "q") 'nil) ; evil macro disable
        (define-key evil-visual-state-map (kbd "R") 'evil-visual-exchange-corners)
        (evil-ex-define-cmd "k" 'kill-this-buffer)
        (setq-default evil-kill-on-visual-paste nil)
        ;(fset 'evil-visual-update-x-selection 'ignore) ; visual mode 'p' command update clipboard problem fix
        (evil-mode 1)
)

(use-package evil-collection :straight t
:after evil
:custom (evil-collection-setup-minibuffer t)
:config
    (add-hook 'vterm-mode-hook #'evil-collection-vterm-escape-stay)
    (evil-collection-init)
    (evil-collection-consult-setup)
)

(use-package general :straight t
:after evil
:init (setq general-override-states '(insert emacs  hybrid   normal
                                      visual motion override operator replace))
:config
      (general-evil-setup :with-shortname-maps)
      (general-create-definer leader :keymaps '(global override) :states '(n v) :prefix "SPC")
      (leader "<SPC>" 'execute-extended-command
              "e"     '(:wk "Emacs")
              "b"     '(:wk "Buffer")
              "r"     '(repeat :wk "Repeat Before Command")
              "s"     '(:wk "Spell Check")
              "d"     '(:wk "Debug")
              "n"     '(:wk "File Manger")
              "f"     '(:wk "Find")
              "g"     '(:wk "Git")
              "o"     '(:wk "Org")
              "p"     '(:wk "Paren")
              "t"     '(:wk "Tabbar")
              "u"     '(:wk "Utils")
              "w"     '(:wk "Windows")
              "h"     '(:wk "Hacking")
              "l"     '(:wk "Lisp or LSP")
              "hr"    '(:wk "Rust")
              "er"    '(restart-emacs :wk "Restart")
              "el"    '(reload-emacs :wk "Reload")
              "ot"    '(org-babel-tangle :wk "tangle config.org" )
              "ff"    '(find-file :wk "Find File")
              "fu"    '(browse-url :wk "Browse url")
              "ep"    '(list-processes :wk "Process")
              "ef"    '((lambda ()(interactive) (find-file "~/.emacs.d/init.el")) :wk "configure file")
              "wf"    '(toggle-frame-fullscreen :wk "Full Screen")
              "wh"    '(shrink-window-horizontally :wk "Right size up")
              "wj"    '(enlarge-window :wk "Right size down")
              "wk"    '(shrink-window :wk "Bottom size up")
              "wl"    '(enlarge-window-horizontally :wk "Bootom size down"))
)

;(use-package drag-stuff :straight t
;:after evil
;:config
;    (drag-stuff-define-keys)
;    (drag-stuff-global-mode t)
;)
;
;(use-package move-text :straight t :after (evil drag-stuff)
;:bind (:map evil-visual-state-map
;          ("C-j" . drag-stuff-down)
;          ("C-k" . drag-stuff-up))
;:config (move-text-default-bindings)
;)

(use-package evil-visualstar :straight t
; vim visual mode에서 * #를 사용해서 같은 단어 검색가능
:after evil
:config (global-evil-visualstar-mode t)
)

(use-package evil-string-inflection :straight t
:config (define-key evil-normal-state-map "gR" 'evil-operator-string-inflection)
)

(use-package evil-surround :straight t
; @call-function
; visual mode S- or gS-
; normal mode ys- or yS-
; change surround cs-
; delete surround ds-
; @select area
; {call-function}- - ;현재부터 단어 끝까지
; {call-function}-i- ;현재 단어
; {call-function}-s- ;현재 줄
; @wrap function
; {select-area}-w
; ${target}( 바꾸고싶은거 ), ${change}(바뀔거)
; 감싸기:     => y-s-i-w-${change}( "(", "{", "[")
; 전부 감싸기 => y-s-s-${change}
; 바꾸기: => c-s-${target}( "(", "{", "["), ${change}
; 벗기기: => d-s-${target}( "(", "{", "[")
:after  evil
:config (global-evil-surround-mode 1)
)

(use-package evil-indent-plus :straight t 
:after evil
:config (evil-indent-plus-default-bindings)
)

;;; visual hint
(use-package evil-goggles :straight t :after evil
:config (setq evil-goggles-pulse t)
        (setq evil-goggles-duration 0.500)
        (evil-goggles-mode)
)

(use-package evil-traces :straight t  :after evil
; move: m +{n}, delete: +{n},+{n}d, join: .,+{n}j glboal: g/{target}/{change}
:config (evil-traces-use-diff-faces)
        (evil-traces-mode)
)

(use-package evil-nerd-commenter :straight t  :after evil
:general (leader "c" '(:wk "comment")
                 "ci" 'evilnc-comment-or-uncomment-lines
                 "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
                 "cc" 'evilnc-copy-and-comment-lines
                 "cp" 'evilnc-comment-or-uncomment-paragraphs
                 "cr" 'comment-or-uncomment-region
                 "cv" 'evilnc-toggle-invert-comment-line-by-line
                 "\\" 'evilnc-comment-operator)
)

(use-package evil-args :straight t  :after evil
; change argument: c-i-a, delete arguemnt: d-a-a
:config (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
        (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)
        (define-key evil-normal-state-map "L" 'evil-forward-arg)
        (define-key evil-normal-state-map "H" 'evil-backward-arg)
        (define-key evil-motion-state-map "L" 'evil-forward-arg)
        (define-key evil-motion-state-map "H" 'evil-backward-arg)
        (define-key evil-normal-state-map "K" 'evil-jump-out-args)
)

(use-package evil-multiedit :straight t :after evil)

(use-package evil-matchit :straight t 
:after evil
:config (global-evil-matchit-mode 1)
)

(use-package evil-lion :straight t 
; gl ${operator}
:config (evil-lion-mode)
)

(use-package evil-escape :straight t 
:config (setq-default evil-escape-key-sequence "jk")
)

(use-package evil-numbers :straight t 
;https://github.com/cofi/evil-numbers
:after evil
:general (leader "="     '(evil-numbers/inc-at-pt :wk "++")
                 "-"     '(evil-numbers/dec-at-pt :wk "--"))
         (nmap   "C-c +" '(evil-numbers/inc-at-pt :wk "++")
                 "C-c -" '(evil-numbers/dec-at-pt :wk "--"))
         (       "C-c +" '(evil-numbers/inc-at-pt :wk "++")
                 "C-c =" '(evil-numbers/inc-at-pt :wk "++")
                 "C-c -" '(evil-numbers/dec-at-pt :wk "--"))
)

(use-package evil-extra-operator :straight t
:after (evil fold-this)
:config (global-evil-extra-operator-mode 1)
)

(provide 'module-evil)
;;; module-evil.el ends here
