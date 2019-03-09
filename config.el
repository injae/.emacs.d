(add-to-list 'load-path "~/.emacs.d/lisp/")
(setq-default custom-file (expand-file-name ".config.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file))

(setq *is-mac*     (eq system-type 'darwin))
(setq *is-windows* (eq system-type 'windows-nt))
(setq *is-cygwin*  (eq system-type 'cygwin))
(setq *is-linux*   (or (eq system-type 'gnu/linux) (eq system-type 'linux)))
(setq *is-unix*    (or *is-linux* (eq system-type 'usg-unix-v) (eq system-type 'berkeley-unix)))

(use-package scroll-bar :ensure nil :no-require t
:if window-system
:init (scroll-bar-mode -1)
:config
    (setq scroll-step 1)
    (setq scroll-conservatively 10000)
)

(use-package tool-bar :ensure nil :no-require t
:if window-system
:init (tool-bar-mode -1)
)

(use-package menu-bar :ensure nil :no-require t
:if window-system
:init (menu-bar-mode -1)
)

(use-package tooltip-mode :ensure nil :no-require t
:if window-system
:init (tooltip-mode -1)
)

(use-package fringe-mode :ensure nil :no-require t
:if window-system
:init (fringe-mode -1)
)

(use-package mouse :ensure nil :no-require t
:if window-system
:init (xterm-mouse-mode)
)

(use-package backup-mode :no-require t
:init (setq backup-inhibited t)
      (setq auto-save-default nil)
      (setq make-backup-files nil) 
)

(set-frame-parameter nil 'alpha 0.95)
(setq compilation-window-height 15)
(set-variable 'cursor-type '(hbar . 10))

;; No popup frame
(setq pop-up-frames nil)
(setq ring-bell-function 'ignore)
; layout save setting
(winner-mode t)
;(desktop-save-mode 1)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-splash-screen t)
(setq echo-keystrokes 0.5)
(setq global-hl-line-mode +1)
(defalias 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode)

;; +------------+------------+
;; | 일이삼사오 | 일이삼사오 |
;; |------------+------------|
;; | 1234567890 | 1234567890 |
;; +------------+------------+
;; | abcdefghij | abcdefghij |
;; +------------+------------+
;; text utf-8 setting
(set-language-environment "Korean")
(prefer-coding-system 'utf-8)
(setq locale-coding-system   'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(set-face-attribute   'default            nil       :family "DejaVu Sans Mono" :height 110)
(set-fontset-font nil 'hangul            (font-spec :family "D2Coding" :pixelsize 18))
(set-fontset-font nil 'japanese-jisx0208 (font-spec :family "D2Coding" :pixelsize 18))
(setq face-font-rescale-alist '(("D2coding" . 1.2)))
(setq-default line-spacing 3)
(global-font-lock-mode t)

;; 한글입력할때 완성전까지 안보이는 문제 해결을 위해 내장 한글입력기 사용
; Linux 내장 한글입력기 사용법 
; ~/.Xresources 만들고 그안에 Emacs*useXIM: false 입력
; 터미널에 xrdb ~/.Xresources 하고 xrdb -merge ~/.Xresources 하고 이맥스 다시키면 됨
(setq default-korean-keyboard 'korean-hangul2)
(global-set-key [?\S- ] 'toggle-input-method)
;(global-set-key [kbd "<Hangul>"] 'toggle-input-method)

(use-package restart-emacs :ensure t :pin melpa :defer t)

(defun launch-separate-emacs-in-terminal ()
(suspend-emacs "fg ; emacs -nw"))

(defun launch-separate-emacs-under-x ()
(call-process "sh" nil nil nil "-c" "emacs &"))

(defun -restart-emacs ()
    (interactive)
    ;; We need the new emacs to be spawned after all kill-emacs-hooks
    ;; have been processed and there is nothing interesting left
    (let ((kill-emacs-hook (append kill-emacs-hook (list (if (display-graphic-p) #'launch-separate-emacs-under-x
                                                                                    #'launch-separate-emacs-in-terminal)))))
            (save-buffers-kill-emacs))
)

(defun -reload-emacs ()
    (interactive)
    (load-file (expand-file-name "~/.emacs.d/config.el"))
)

(use-package sudo-mode :no-require t
:after evil-leader
:preface
(defun sudo-find-file (file-name)
  "sudo open"
  (interactive "FSudo Find File: ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
    (find-file tramp-file-name)))
:init (evil-leader/set-key "fs" #'sudo-find-file))

(use-package paradox :ensure t :pin melpa :defer t
;https://github.com/Malabarba/paradox
:init (setq paradox-github-token "e1a1518b1f89990587ec97b601a1d0801c5a40c6")
)

(use-package drag-stuff :ensure t :pin melpa :defer t
:after evil
:init (drag-stuff-global-mode t)
      (drag-stuff-define-keys))

(use-package goto-last-change :ensure t :pin melpa :defer t
;https://github.com/camdez/goto-last-change.el
:after evil-leader
:init (evil-leader/set-key "fl" 'goto-last-change)
)

(use-package esup :ensure t :pin melpa :defer t)

(server-start)

;https://www.gnu.org/software/emacs/manual/html_node/elisp/Warning-Basics.html
  (setq warning-minimum-level :error)

(use-package buffer-zoom :no-require t
:after evil-leader
:config (evil-leader/set-key "tu" 'text-scale-increase
                             "td" 'text-scale-decrease)
)

(use-package hungry-delete :ensure t :pin melpa :defer t :disabled
; 공백 지울때 한꺼번에 다지워짐 
:init (global-hungry-delete-mode)
)

(use-package face-picker :no-require t
:preface
(defun what-face (pos)
     (interactive "d")
     (let ((face (or (get-char-property (pos) 'read-face-name)
                     (get-char-property (pos) 'face))))
          (if face (message "Face: %s" face) (message "No face at %d" pos))))
)

(use-package beacon :ensure t :pin melpa :defer t :init (beacon-mode t)) 
(use-package git-gutter :ensure t :pin melpa :defer t
:init 
    (setq-default display-line-numbers-width 3)
    (global-git-gutter-mode t)
    (global-display-line-numbers-mode t)
    (global-hl-line-mode t)
:config
    (setq git-gutter:lighter       " gg")
    (setq git-gutter:window-width  1)
    (setq git-gutter:modified-sign ".")
    (setq git-gutter:added-sign    "+")
    (setq git-gutter:deleted-sign  "-")
    (set-face-foreground 'git-gutter:added    "#daefa3")
    (set-face-foreground 'git-gutter:deleted  "#FA8072")
    (set-face-foreground 'git-gutter:modified "#b18cce")
)

(use-package doom-themes :ensure t :pin melpa
:init   (load-theme 'doom-one t)
:config (doom-themes-neotree-config)
        (doom-themes-org-config)
)

;(load-library "hideshow")
;    (global-set-key (kbd "<C-l>") 'hs-show-block)
;    (global-set-key (kbd "<C-h>") 'hs-hide-block)
;    (add-hook 'c-mode-common-hook     'hs-minor-mode)
;    (add-hook 'emacs-lisp-mode-hook   'hs-minor-mode)
;    (add-hook 'java-mode-hook         'hs-minor-mode)
;    (add-hook 'lisp-mode-hook         'hs-minor-mode)
;    (add-hook 'perl-mode-hook         'hs-minor-mode)
;    (add-hook 'sh-mode-hook           'hs-minor-mode)

;(use-package aggressive-indent :ensure t :pin melpa :defer t
;https://github.com/Malabarba/aggressive-indent-mode
;:init (global-aggressive-indent-mode)
      ;exclud mode
      ;(add-to-list 'aggresive-indent-excluded-modes 'html-mode)
;)

(use-package smart-tabs-mode :ensure t :pin melpa :defer t :disabled
:config (smart-tabs-insinuate 'c 'c++)
)

(use-package indent-guide :ensure t :disabled
; 문자로 표시하기 때문에 예쁘지 않음
:hook (prog-mode text-mode)
:config
    (setq indent-guide-char      " ")
    ;(setq indent-guide-recursive t)
    (setq indent-guide-delay     0.1)
    (set-face-background 'indent-guide-face "dimgray")
    (indent-guide-mode)
)

(use-package highlight-indentation :ensure t :pin melpa :disabled
:hook   (prog-mode text-mode)
:config ;(highlight-indentation-mode)
)


(use-package highlight-indent-guides :ensure t :disabled
:hook (prog-mode text-mode)
:config
    (highlight-indent-guides-mode)
    (setq highlight-indent-guides-delay 0)
    (setq highlight-indent-guides-auto-enabled nil)
    (set-face-background 'highlight-indent-guides-odd-face       "darkgray")
    (set-face-background 'highlight-indent-guides-even-face      "dimgray")
    (set-face-background 'highlight-indent-guides-character-face "dimgray")
    (setq highlight-indent-guides-method 'column)
)

(use-package indent4-mode :no-require t
:preface
    (defun my-set-indent (n)
        (setq-default tab-width n)
        ;(electric-indent-mode n)
        (setq c-basic-offset n)
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

(use-package paren :ensure t :pin melpa :defer t
:init   (show-paren-mode 0)
        (electric-pair-mode 0)
:config (setq show-paren-delay 0)
)    

(use-package rainbow-delimiters :ensure t :pin melpa
:hook ((prog-mode text-mode) . rainbow-delimiters-mode)
)

(use-package smartparens :ensure t :pin melpa
:init (smartparens-global-mode)
      (evil-leader/set-key "pr"  'sp-rewrap-sexp
                           "pu"  'sp-unwrap-sexp
                           "pll" 'sp-forward-slurp-sexp
                           "phh" 'sp-backward-slurp-sexp
                           "plh" 'sp-forward-barf-sexp
                           "phl" 'sp-backward-barf-sexp)
)

(use-package evil-smartparens :ensure t :pin melpa
:after (evil smartparens)
:init  (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))

(use-package parinfer :ensure t :pin melpa :defer t :disabled
:after (evil)
:bind ("C-," . parinfer-toggle-mode)
:init
(setq parinfer-extensions
    '(defaults
      pretty-parens
      evil
      lispy
      paredit
      smart-tab
      smart-yank))
(add-hook 'clojure-mode-hook     #'parinfer-mode)
(add-hook 'emacs-lisp-mode-hook  #'parinfer-mode)
(add-hook 'common-lisp-mode-hook #'parinfer-mode)
(add-hook 'scheme-mode-hook      #'parinfer-mode)
(add-hook 'lisp-mode-hook        #'parinfer-mode)
)

(use-package hydra :ensure t :pin melpa :defer t :disabled)

(use-package which-key :ensure t :pin melpa
:commands (which-key-mode)
:init     (which-key-mode t) 
:config   (which-key-enable-god-mode-support t))

(use-package evil :ensure t :pin melpa
:init (setq evil-want-integration t)
      (setq evil-want-keybinding nil)
      (setq evil-want-C-u-scroll t)
      (setq-default evil-symbol-word-search t)
      (evil-mode 1)
)

(use-package evil-surround :ensure t :pin melpa
;command is visual mode: y-s-i
:after  evil
:config (global-evil-surround-mode 1)
)

(use-package evil-mc :ensure t :pin melpa
:after evil
:preface
      (defun user-evil-mc-make-cursor-here () 
          (evil-mc-pause-cursors)
          (evil-mc-make-cursor-here))
:config 
      (evil-leader/set-key "ech" #'evil-mc-make-cursors-here
                           "ecp" #'evil-mc-pause-cursors
                           "ecr" #'evil-mc-resume-cursors
                           "ecu" #'evil-mc-undo-all-cursors)
      (global-evil-mc-mode 1)
)

(use-package evil-multiedit :ensure t :pin melpa :defer t :disabled)

(use-package evil-matchit :ensure t :pin melpa
:after evil
:config (global-evil-matchit-mode 1)
)

(use-package evil-escape :ensure t :pin melpa :disabled
:config (setq-default evil-escape-key-sequence "jk")
)

(use-package evil-numbers :ensure t :pin melpa
;https://github.com/cofi/evil-numbers
:after evil
:config
    (global-set-key (kbd "C-c +") 'evil-number/inc-at-pt)
    (global-set-key (kbd "C-c -") 'evil-number/dec-at-pt)
    (evil-leader/set-key "+" 'evil-number/inc-at-pt)
    (evil-leader/set-key "-" 'evil-number/dec-at-pt)
)

(use-package evil-leader :ensure t :pin melpa
:after (evil which-key)
:config 
     (global-evil-leader-mode t)
     (setq evil-leader/leader "<SPC>")
     (evil-leader/set-key
         ;"<SPC>" 'helm-smex
         "<SPC>" 'counsel-M-x
         "er"    'restart-emacs
         "el"    '-reload-emacs
         "ff"    'find-file
         "up"    'list-processes
         "ef"    (lambda ()(interactive)(find-file "~/.emacs.d/config.org"))
         "wf"    'toggle-frame-fullscreen
         "wh"    'shrink-window-horizontally
         "wj"    'enlarge-window
         "wk"    'shrink-window
         "wl"    'enlarge-window-horizontally
         )
     (which-key-declare-prefixes "SPC b  " "Buffer")
     (which-key-declare-prefixes "SPC d  " "Debug")
     (which-key-declare-prefixes "SPC e  " "Emacs")
     (which-key-declare-prefixes "SPC e f" "Emacs Config")
     (which-key-declare-prefixes "SPC e c" "Evil MultiEdit")
     (which-key-declare-prefixes "SPC f  " "Find")
     (which-key-declare-prefixes "SPC n  " "File Manager")
     (which-key-declare-prefixes "SPC g  " "Git")
     (which-key-declare-prefixes "SPC o  " "Org")
     (which-key-declare-prefixes "SPC p  " "Projectile")
     (which-key-declare-prefixes "SPC t  " "Tabbar")
     (which-key-declare-prefixes "SPC u  " "Utils")
     (which-key-declare-prefixes "SPC w  " "Windows")
     (which-key-declare-prefixes "SPC h  " "Hacking")
     (which-key-declare-prefixes "SPC h r" "Rust")
     (which-key-declare-prefixes "SPC h c" "C/C++")
     (which-key-declare-prefixes "SPC h y" "Yasnippet")
     (which-key-declare-prefixes "SPC h m" "Markdown")
     (which-key-declare-prefixes "SPC h d" "Definition Jump")
     (which-key-declare-prefixes "SPC f g" "Google")
     (which-key-declare-prefixes "SPC f a" "Agrep")
    )

(use-package evil-collection :ensure t :pin melpa
:after  (evil)
:init (setq evil-collection-setup-minibuffer t)
        (add-hook 'magit-mode-hook     (lambda () (evil-collection-magit-setup)     (evil-collection-init)))
        (add-hook 'neotree-mode-hook   (lambda () (evil-collection-neotree-setup)   (evil-collection-init)))
        (add-hook 'evil-mc-mode-hook   (lambda () (evil-collection-evil-mc-setup)   (evil-collection-init)))
        (add-hook 'which-key-mode-hook (lambda () (evil-collection-which-key-setup) (evil-collection-init)))
        (evil-collection-minibuffer-setup)
        (evil-collection-ivy-setup)
        (evil-collection-buff-menu-setup)
        (evil-collection-package-menu-setup)
        (evil-collection-eshell-setup)
:config
        (evil-collection-init)
)

(use-package all-the-icons :ensure t :pin melpa)
(use-package doom-modeline :ensure t :pin melpa
:hook (after-init . doom-modeline-init)
:init (setq doom-modeline-height 20)
      (setq doom-modeline-icon t)
      (setq doom-modeline-persp-name t)
      (setq doom-modeline-major-mode-icon t)
      (setq doom-modeline-lsp t)
      (setq doom-modeline-python-executable "python")
      (setq doom-modeline--flycheck-icon t)
      (setq doom-modeline-github t)
      (setq doom-modeline-current-window t)
)

(use-package hide-mode-line :ensure t :pin melpa
:after (neotree)
:hook (neotree-mode . hide-mode-line-mode)
)

(use-package nyan-mode :ensure t :pin melpa :defer t
:init   (nyan-mode)
:config (setq-default nyan-wavy-trail t)
        (nyan-start-animation)
        (nyan-refresh))
(use-package fancy-battery :ensure t :pin melpa :defer t
:init   (fancy-battery-mode)
:config (setq fancy-battery-show-percentage t))

(use-package diminish :ensure t :pin melpa :defer t
:init 
    (diminish 'c++-mode "C++ Mode")
    (diminish 'c-mode   "C Mode"  )
)

(use-package ivy :ensure t :pin melpa :defer t
:after evil-collection
:commands counsel-M-x
:bind ("M-x" . counsel-M-x)
:config (ivy-mode 1)
        (setq ivy-use-virtual-buffers t)
        (setq ivy-use-selectable-prompt t)
        (setq enable-recursive-minibuffers t)
        (setq ivy-height 20)
        (setq ivy-count-format "(%d/%d) ")
        (setq ivy-display-style 'fancy)
        (setq ivy-re-builders-alist '((counsel-M-x . ivy--regex-fuzzy)
                                      (t . ivy--regex-plus)))
        (setq ivy-format-function 'ivy-format-function-line)
        (setq ivy-initial-inputs-alist nil)
)

(use-package counsel 
:after ivy
:config (counsel-mode)
)

(use-package swiper :ensure t :pin melpa
:after ivy
:bind ("C-s"   . swiper)
      ("C-S-s" . swiper-all)
:config
    (setq swiper-action-recenter t)
    (setq swiper-goto-start-of-match t)
    (setq swiper-stay-on-quit t)
)

(use-package ivy-yasnippet :ensure t :pin melpa
:after (ivy yasnippet)
:bind  ("C-c C-y" . ivy-yasnippet)
:config (advice-add #'ivy-yasnippet--preview :override #'ignore)
)

(use-package historian :ensure t :pin melpa
:after  (ivy)
:config (historian-mode)
)

(use-package ivy-historian :ensure t :pin melpa
:after  (ivy historian)
:config (ivy-historian-mode)
)

(use-package ivy-xref :ensure t :pin melpa :disabled
:after (ivy xref)
:config (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)
)

(use-package flyspell-correct-ivy :ensure t :pin melpa
:after (ivy flyspell)
:bind  (:map flyspell-mode-map
             ([remap flyspell-correct-word-before-point] . flyspell-correct-previous-word-generic))
)

(use-package counsel-projectile :ensure t :pin melpa
:after  (counsel projectile)
:config (setq projectile-completion-system 'ivy)
        (counsel-projectile-mode 1)
        (evil-leader/set-key "epf" 'counsel-projectile-find-file
                             "epg" 'counsel-projectile-rg
                             "ept" 'counsel-projectile-transformer)
)

(use-package counsel-world-clock :ensure t :pin melpa
:after (counsel)
;:bind (:map counsel-mode-map ("C-c c k" . counsel-world-clock))
)

(use-package counsel-tramp :ensure t :pin melpa
:after counsel
:bind ("C-c s" . 'counsel-tramp)
:init (setq tramp-default-method "ssh")
)

(use-package counsel-org-clock :ensure t :pin melpa 
:after (counsel org)
)

(use-package ivy-rich :ensure t :pin melpa
:after ivy
:defines   (all-the-icons-mode-icon-alist all-the-icons-dir-icon-alist bookmark-alist)
:functions (all-the-icons-icon-family
            all-the-icons-match-to-alist
            all-the-icons-auto-mode-match?
            all-the-icons-octicon
            all-the-icons-dir-is-submodule)
:hook (ivy-rich-mode . (lambda ()
                         (setq ivy-virtual-abbreviate
                               (or (and ivy-rich-mode 'abbreviate) 'name))))
:preface
(with-eval-after-load 'all-the-icons
  (add-to-list 'all-the-icons-mode-icon-alist
               '(gfm-mode  all-the-icons-octicon "markdown" :v-adjust 0.0 :face all-the-icons-lblue)))

(defun ivy-rich-bookmark-name (candidate)
  (car (assoc candidate bookmark-alist)))

(defun ivy-rich-buffer-icon (candidate)
  "Display buffer icons in `ivy-rich'."
  (when (display-graphic-p)
    (when-let* ((buffer (get-buffer candidate))
                (major-mode (buffer-local-value 'major-mode buffer))
                (icon (if (and (buffer-file-name buffer)
                               (all-the-icons-auto-mode-match? candidate))
                          (all-the-icons-icon-for-file candidate)
                        (all-the-icons-icon-for-mode major-mode))))
      (if (symbolp icon)
          (setq icon (all-the-icons-icon-for-mode 'fundamental-mode)))
      (unless (symbolp icon)
        (propertize icon
                    'face `(
                            :height 1.1
                            :family ,(all-the-icons-icon-family icon)
                            ))))))

(defun ivy-rich-file-icon (candidate)
  "Display file icons in `ivy-rich'."
  (when (display-graphic-p)
    (let ((icon (if (file-directory-p candidate)
                    (cond
                     ((and (fboundp 'tramp-tramp-file-p)
                           (tramp-tramp-file-p default-directory))
                      (all-the-icons-octicon "file-directory"))
                     ((file-symlink-p candidate)
                      (all-the-icons-octicon "file-symlink-directory"))
                     ((all-the-icons-dir-is-submodule candidate)
                      (all-the-icons-octicon "file-submodule"))
                     ((file-exists-p (format "%s/.git" candidate))
                      (all-the-icons-octicon "repo"))
                     (t (let ((matcher (all-the-icons-match-to-alist candidate all-the-icons-dir-icon-alist)))
                          (apply (car matcher) (list (cadr matcher))))))
                  (all-the-icons-icon-for-file candidate))))
      (unless (symbolp icon)
        (propertize icon
                    'face `(
                            :height 1.1
                            :family ,(all-the-icons-icon-family icon)
                            ))))))

(setq ivy-rich--display-transformers-list
      '(ivy-switch-buffer
        (:columns
         ((ivy-rich-buffer-icon (:width 1))
          (ivy-rich-candidate (:width 30))
          (ivy-rich-switch-buffer-size (:width 7))
          (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
          (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
          (ivy-rich-switch-buffer-project (:width 15 :face success))
          (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
         :predicate
         (lambda (cand) (get-buffer cand)))
        ivy-switch-buffer-other-window
        (:columns
         ((ivy-rich-buffer-icon)
          (ivy-rich-candidate (:width 30))
          (ivy-rich-switch-buffer-size (:width 7))
          (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
          (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
          (ivy-rich-switch-buffer-project (:width 15 :face success))
          (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
         :predicate
         (lambda (cand) (get-buffer cand)))
        counsel-M-x
        (:columns
         ((counsel-M-x-transformer (:width 50))
          (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
        counsel-describe-function
        (:columns
         ((counsel-describe-function-transformer (:width 50))
          (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
        counsel-describe-variable
        (:columns
         ((counsel-describe-variable-transformer (:width 50))
          (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))
        counsel-find-file
        (:columns
         ((ivy-rich-file-icon)
          (ivy-rich-candidate (:width 30))))
        counsel-file-jump
        (:columns
         ((ivy-rich-file-icon)
          (ivy-rich-candidate (:width 30))))
        counsel-dired-jump
        (:columns
         ((ivy-rich-file-icon)
          (ivy-rich-candidate (:width 30))))
        counsel-git
        (:columns
         ((ivy-rich-file-icon)
          (ivy-rich-candidate (:width 30))))
        counsel-projectile-find-file
        (:columns
         ((ivy-rich-file-icon)
          (ivy-rich-candidate (:width 30))))
        counsel-projectile-find-dir
        (:columns
         ((ivy-rich-file-icon)
          (ivy-rich-candidate (:width 30))))
        counsel-recentf
        (:columns
         ((ivy-rich-file-icon)
          (ivy-rich-candidate (:width 90))
          (ivy-rich-file-last-modified-time (:face font-lock-comment-face))))
        counsel-bookmark
        (:columns
         ((ivy-rich-bookmark-type)
          (ivy-rich-bookmark-name (:width 40))
          (ivy-rich-bookmark-info)))
        ))
:config
(setq ivy-rich-parse-remote-buffer nil)
(ivy-rich-mode 1))

(use-package smex :ensure t :pin melpa :disabled
:init (smex-initialize)
      (global-set-key [remap execute-extended-command] #'helm-smex)
      (evil-leader/set-key "fm" #'smex-major-mode-commands)
)
(use-package helm-smex :ensure t :pin melpa :disabled
:after (helm smex)
:bind ("M-x" . #'helm-smex-major-mode-commands)
:init (global-set-key [remap execute-extended-command] #'helm-smex)
      (evil-leader/set-key "fm" #'helm-smex-major-mode-commands)
)

(use-package projectile :ensure t :pin melpa :defer t
:init (projectile-mode t)
)

(use-package neotree :ensure t :pin melpa
:after (projectile)
:commands (neotree-toggle)
:init 
    (setq projectile-switch-project-action 'neotree-projectile-action)
    (setq-default neo-smart-open t)
    (evil-leader/set-key "n" #'neotree-toggle)
:config
    (setq-default neo-window-width 30)
    (setq-default neo-dont-be-alone t)
    (setq-local display-line-numbers 0)
    (setq neo-force-change-root t)
    (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
    (setq neo-show-hidden-files t)
)

(use-package ace-window :ensure t :pin melpa
:commands (ace-window)
:init   (evil-leader/set-key "wo" 'ace-window)
:config (setq aw-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8))
)

(use-package eyebrowse :ensure t :pin melpa :defer t
:init (eyebrowse-mode t)
:config 
    (evil-leader/set-key
        "w;" 'eyebrowse-last-window-config
        "w0" 'eyebrowse-close-window-config
        "w1" 'eyebrowse-switch-to-window-config-1
        "w2" 'eyebrowse-switch-to-window-config-2
        "w3" 'eyebrowse-switch-to-window-config-3
        "w4" 'eyebrowse-switch-to-window-config-4
        "w5" 'eyebrowse-switch-to-window-config-5
        "w6" 'eyebrowse-switch-to-window-config-6
        "w7" 'eyebrowse-switch-to-window-config-7
    )
)

(use-package exwm :ensure t :pin melpa :disabled
:if window-system
:commands (exwm-init)
:config
    (use-package exwm-config 
    :init (exwm-config-default))
    (setq exwm-workspace-number 0)
    (exwm-input-set-key (kbd "s-h") 'windmove-left)
    (exwm-input-set-key (kbd "s-j") 'windmove-down)
    (exwm-input-set-key (kbd "s-k") 'windmove-up)
    (exwm-input-set-key (kbd "s-l") 'windmove-right)
    (exwm-input-set-key (kbd "s-s") 'split-window-right)
    (exwm-input-set-key (kbd "s-v") 'split-window-vertically)
    (exwm-input-set-key (kbd "s-d") 'delete-window)
    (exwm-input-set-key (kbd "s-q") '(lambda () (interactive) (kill-buffer (current-buffer))))
    (exwm-input-set-key (kbd "s-e") 'exwm-exit)
    (advice-add 'split-window-right :after 'windmove-right)
    (advice-add 'split-window-vertically :after 'windmove-down)

    ;; 's-N': Switch to certain workspace
    (dotimes (i 10)
        (exwm-input-set-key (kbd (format "s-%d" i))
                            `(lambda ()
                            (interactive)
                            (exwm-workspace-switch-create ,i))))
    ;; 's-r': Launch application
    (exwm-input-set-key (kbd "s-r")
                        (lambda (command)
                            (interactive (list (read-shell-command "$ ")))
                            (start-process-shell-command command nil command)))
)

(use-package magit :ensure t :pin melpa 
:commands magit-status
:init   (evil-leader/set-key "gs" 'magit-status)
:config (setq vc-handled-backends nil)
)

(use-package evil-magit :ensure t :pin melpa
:after (evil magit)
:config  (evil-magit-init)
)

(use-package magithub :ensure t :pin melpa :disabled
:after magit
:init (magithub-feature-autoinject t)
      (evil-leader/set-key "gd" 'magithub-dashboard)
      (setq magithub-clone-default-directory "~/github")   
)

(use-package evil-ediff :ensure t :pin melpa 
:after evil
:config (evil-ediff-init)
)

(use-package undo-tree :ensure t :pin melpa :defer t :diminish undo-tree-mode
:init
    ;(global-set-key (kbd "C-u") #'undo-tree-undo)
    ;(global-set-key (kbd "C-r") #'undo-tree-redo)
    (evil-leader/set-key "uu"    'undo-tree-undo)
    (evil-leader/set-key "ur"    'undo-tree-undo)
    (defalias 'redo 'undo-tree-redo)
    (defalias 'undo 'undo-tree-undo)
    (global-undo-tree-mode)
)

(use-package org
:init (setq org-directory            (expand-file-name "~/Dropbox/org"))
      (setq org-default-notes-file   (concat org-directory "/notes/notes.org"))
      (evil-leader/set-key
          "oa" 'org-agenda
          "ob" 'org-iswitchb
          "oc" 'org-capture
          "oe" 'org-edit-src-code
          "ok" 'org-edit-src-exit
          "ol" 'org-store-link
      )
)

(use-package org-bullets :ensure t :pin melpa
:after org
:init ;(setq org-bullets-bullet-list '("◉" "◎" "<img draggable="false" class="emoji" alt="⚫" src="https://s0.wp.com/wp-content/mu-plugins/wpcom-smileys/twemoji/2/svg/26ab.svg">" "○" "►" "◇"))
      (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
)

(use-package org-journal :ensure t :pin melpa
:after org
:preface
(defun org-journal-find-location () (org-journal-new-entry t) (goto-char (point-min)))
:config
      (setq org-journal-dir (expand-file-name "~/Dropbox/org/journal")
            org-journal-file-format "%Y-%m-%d.org"
            org-journal-date-format "%Y-%m-%d (%A)")
      (add-to-list 'org-agenda-files (expand-file-name "~/Dropbox/org/journal"))
      (setq org-journal-enable-agenda-integration t
            org-icalendar-store-UID t
            org-icalendar-include0tidi "all"
            org-icalendar-conbined-agenda-file "~/calendar/org-journal.ics")
      (org-journal-update-org-agenda-files)
      (org-icalendar-combine-agenda-files)
)


(use-package org-capture
:after org
:config (setq org-reverse-note-order t)
      (add-to-list 'org-agenda-files (expand-file-name "~/Dropbox/org/notes"))
      (setq org-capture-templates
          '(("t" "Todo" entry (file+headline "~/Dropbox/org/notes/notes.org" "Todos")
             "* TODO %?\nAdded: %U\n" :prepend t :kill-buffer t)
            ("l" "Link" entry (file+headline "~/Dropbox/org/notes/notes.org" "Links")
             "* TODO %?\nAdded: %U\n" :prepend t :kill-buffer t)
            ("j" "Journal" entry (function org-journal-find-location)
             "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?")
            ("a" "Appointment" entry (file "~/Dropbox/org/agenda/gcal.org")
             "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")
           )
      )
)

(use-package org-agenda 
:after org
:config (use-package evil-org :ensure t :pin melpa
        :after (org evil)
        :init (add-hook 'org-mode-hook 'evil-org-mode)
              (add-hook 'evil-org-mode-hook (lambda () (evil-org-set-key-theme)))
              (setq org-agenda-files '("~/Dropbox/org/agenda"))
              (require 'evil-org-agenda)
              (evil-org-agenda-set-keys)
        )
)
(use-package org-pomodoro :ensure t :pin melpa
:after org-agenda
:custom
    (org-pomodoro-ask-upon-killing t)
    (org-pomodoro-format "%s")
    (org-pomodoro-short-break-format "%s")
    (org-pomodoro-long-break-format  "%s")
:custom-face
    (org-pomodoro-mode-line         ((t (:foreground "#ff5555"))))
    (org-pomodoro-mode-line-break   ((t (:foreground "#50fa7b"))))
:hook
    (org-pomodoro-started  . (lambda () (notifications-notify
        :title "org-pomodoro"
        :body "Let's focus for 25 minutes!"
        :app-icon "~/.emacs.d/img/001-food-and-restaurant.png")))
    (org-pomodoro-finished . (lambda () (notifications-notify
        :title "org-pomodoro"
        :body "Well done! Take a break."
        :app-icon "~/.emacs.d/img/004-beer.png")))
:bind (:map org-agenda-mode-map ("p" . org-pomodoro))
)

(use-package org-gcal :ensure t :pin melpa
:after  org-agenda
:config (setq org-gcal-client-id "354752650679-2rrgv1qctk75ceg0r9vtaghi4is7iad4.apps.googleusercontent.com"
              org-gcal-client-secret "j3UUjHX4L0huIxNGp_Kw3Aj4"
              org-gcal-file-alist '(("8687lee@gmail.com" . "~/Dropbox/org/agenda/gcal.org")))
        (add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync)))
        (add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync)))
)

(use-package orgtbl-aggregate :ensure t :pin melpa :defer t
; https://github.com/tbanel/orgaggregate
:after org
)

;(use-package calfw :ensure t :pin melpa :defer t 
;:commands cfw:open-calendar-buffer
;:config (use-package calfw-org
;        :config (setq cfw:org-agenda-schedule-args '(:deadline :timestamp :sexp))
;        )
;)
;(use-package calfw-gcal :ensure t :pin melpa :defer t
;:init (require 'calfw-gcal))

(use-package org-babel :no-require t
:after org
:config (org-babel-do-load-languages
          'org-babel-load-languages
          '((emacs-lisp . t)
            (python     . t)
            (org        . t)
            (shell      . t)
            (C          . t)))
)
;; 스펠체크 넘어가는 부분 설정
(add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))

(use-package olivetti :ensure t :pin melpa
:commands (olivetti-mode)
:config (setq olivetti-body-width 120))
(use-package typo :ensure t :pin melpa
:commands (type-mode))
(use-package poet-theme :ensure t :pin melpa :defer t)
(define-minor-mode writer-mode
    "poet use writer mode"
    :lighter " writer"
    (if writer-mode 
       (progn
           (olivetti-mode 1)
           (typo-mode 1)
           (display-line-numbers-mode 0))
       (olivetti-mode 0)
       (typo-mode 0)
       (beacon-mode 0)
       (display-line-numbers-mode 1)))

(use-package mu4e :ensure t :pin melpa :disabled
:commands (mu4e)
)

(use-package rainbow-mode :ensure t :pin melpa
:hook   (prog-mode text-mode)
:config (rainbow-mode)
)

(use-package docker :ensure t :pin melpa :disabled
:init (evil-leader/set-key "hud" 'docker)) 

(use-package dockerfile-mode :ensure t :pin melpa 
:init (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(use-package shell-pop :ensure t :pin melpa
:init (setq shell-pop-shell-type '("eshell" "* eshell *" (lambda () (eshell))))
      (evil-leader/set-key "ut" 'shell-pop)
      ;(global-set-key (kbd "<C-t>") 'shell-pop)
)

(use-package eshell
:commands eshell-mode
:config  (setq eshell-buffer-maximum-lines 1000)
         (add-hook 'eshell-mode-hook (lambda () (setq pcomplete-cycle-completions nil)))
         (setq eshell-cmpl-cycle-completions nil)
)

(use-package exec-path-from-shell :ensure t :pin melpa
:after eshell
:config ;(exec-path-from-shell-copy-env "PATH")
      (when (memq window-system '(mac ns x)) (exec-path-from-shell-initialize))
)

(use-package esh-help :ensure t :pin melpa
:after eshell
:config (setup-esh-help-eldoc)
)

(use-package eshell-prompt-extras :ensure t :pin melpa
:after eshell
:config
    (autoload 'epe-theme-lambda "eshell-prompt-extras")
    (setq eshell-highlight-prompt nil
          eshell-prompt-function 'epe-theme-lambda)
)
(use-package virtualenvwrapper :ensure t :pin melpa
:after eshell-prompt-extras
:init (venv-initialize-eshell))

(use-package fish-completion :ensure t :pin melpa
:after eshell
:config (when (and (executable-find "fish")
                   (require 'fish-completion nil t))
              (global-fish-completion-mode))
)

(use-package esh-autosuggest :ensure t :pin melpa
:after eshell
:hook (eshell-mode . esh-autosuggest-mode)
)

(use-package eshell-up :ensure t :pin melpa
:after eshell
:config (add-hook 'eshell-mode-hook (lambda () (eshell/alias "up" "eshell-up $1")
                                          (eshell/alias "pk" "eshell-up-peek $1")))
)

(use-package execute-shell :no-require t
:after eshell
:preface
(defun background-shell-command (command)
    "run shell commmand background"
    (interactive "sShell Command : ")
    (call-process-shell-command "command" nil 0))
:config (add-to-list 'display-buffer-alist
        (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))
)

(use-package command-log-mode :ensure t :pin melpa :defer t)

(use-package emojify :ensure t :pin melpa :defer t
:if window-system
:init   (global-emojify-mode 1)
:config (setq emojify-display-style 'image)
        (setq emojify-emoji-styles  '(unicode))
        (setq emojify-emoji-set "emojione-v2.2.6")
)

(use-package buffer-move :ensure t :pin melpa :defer t
:init
    (evil-leader/set-key
        "b s" 'ibuffer
        "b r" 'eval-buffer
        "b h" 'buf-move-left
        "b j" 'buf-move-down
        "b k" 'buf-move-up
        "b l" 'buf-move-right
        "b m" 'switch-to-buffer
        "b n" 'next-buffer
        "b p" 'previous-buffer
    )
    (global-set-key (kbd "C-x C-b") 'ibuffer)
)

(setq ibuffer-saved-filter-groups
    '(("home"
          ("emacs-config" (or (filename . ".emacs.d")
                              (filename . "emacs-config")))
          ("org-mode"     (or (mode . org-mode)
                              (filename ."OrgMode")))
          ("code"         (or (filename . "~/dev")
                              (mode . prog-mode)
                              (mode . c++-mode)
                              (mode . c-mode)
                              (mode . yaml-mode)
                              (mode . toml-mode)
                              (mode . lisp-mode)
                              (mode . emacs-lisp-mode)))
          ("magit"        (or (name . "\*magit")))
          ("Help"         (or (name . "\*Help\*")
                              (name . "\*Apropos\*")
                              (name . "\*info\*")))
     ))
)
(add-hook 'ibuffer-mode-hook '(lambda () (ibuffer-switch-to-saved-filter-groups "home")))

(use-package ibuffer-projectile :ensure t :pin melpa :disabled
:after (projectile)
:init  (add-hook 'ibuffer-hook (lambda () (ibuffer-projectile-set-filter-groups)
                                     (unless (eq ibuffer-sorting-mode 'alphabetic)
                                             (ibuffer-do-sort-by-alphabetic))))
)

(use-package dash :ensure t :pin melpa :defer t
:init (dash-enable-font-lock)
)
(use-package dash-functional :ensure t :pin melpa
:after dash
)

(use-package ialign :ensure t :pin melpa :defer t
:init (evil-leader/set-key "ta" 'ialign))

(use-package page-break-lines :ensure t :pin melpa :defer t)
(use-package dashboard :ensure t :pin melpa :defer t
:init (dashboard-setup-startup-hook)
:config 
    (setq dashboard-banner-logo-title "Happy Hacking")
    ;(setq dashboard-startup-banner "~/.emacs.d/image/emacs_icon.png") ;banner image change
    (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
    (setq show-week-agenda-p t)
    (setq dashboard-items '((recents   . 5)
                            (bookmarks . 5)
                            (projects  . 5)
                            (agenda    . 5)))
)

(use-package tabbar :ensure t :pin melpa
:after (powerline evil-leader)
:commands (tabbar-mode)
:preface
(defvar my/tabbar-left  "/"  "Separator on left side of tab")
(defvar my/tabbar-right "\\" "Separator on right side of tab")
(defun my/tabbar-tab-label-function (tab)
    (powerline-render (list my/tabbar-left (format " %s  " (car tab)) my/tabbar-right)))
:init (tabbar-mode 1)
:config
      (require 'tabbar)
      (setq my/tabbar-left  (powerline-wave-right 'tabbar-default nil 24))
      (setq my/tabbar-right (powerline-wave-left  nil 'tabbar-default 24))
      (setq tabbar-tab-label-function 'my/tabbar-tab-label-function)
      (setq tabbar-use-images nil)
      (setq tabbar-scroll-left-button  nil)
      (setq tabbar-scroll-right-button nil)
      (setq tabbar-home-button nil)
      (evil-leader/set-key "th" 'tabbar-forward-tab)
      (evil-leader/set-key "tl" 'tabbar-backward-tab)
)

(use-package symon :ensure t :pin melpa :defer t)

(use-package google-this :ensure t :pin melpa
:commands google-this
:init    (evil-leader/set-key "fgs" 'google-this)
:config  (google-this-mode 1)
)
(evil-leader/set-key "fgu" 'browse-url)

(use-package google-translate :ensure t :pin melpa
:commands (google-translate-smooth-translate)
:init (evil-leader/set-key "fgt" 'google-translate-smooth-translate)
:config (require 'google-translate-smooth-ui)
       ;(require 'google-translate-default-ui)
       ;(evil-leader/set-key "ft" 'google-translate-at-point)
       ;(evil-leader/set-key "fT" 'google-translate-query-translate)
       (setq google-translate-translation-directions-alist
           '(("en" . "ko")
             ("ko" . "en")
             ("jp" . "ko")
             ("ko" . "jp")))
)

(use-package esup :ensure t :pin melpa :defer t)

(use-package flyspell :ensure t :pin melpa :defer t
:init
    (add-hook 'prog-mode-hook 'flyspell-prog-mode)
    (add-hook 'text-mode-hook 'flyspell-mode)
    (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
)

(use-package wgrep :ensure t :pin melpa :defer t
:config (setq wgrep-auto-save buffer t)
       ;(setq wgrep-enable-key "r")
)

(use-package iedit :ensure t :pin melpa :defer t
:init (evil-leader/set-key "fi" 'iedit-mode)
)

(use-package try :ensure t :pin melpa :defer t)

(use-package org-use-package :no-require t
:after (evil org)
:preface
(defun org-use-package-install ()
    "org babel emacs config evaluate"
    (interactive)
    (org-babel-execute-maybe)
    (undo-tree-undo))
:config (evil-leader/set-key "oi" 'org-use-package-install)
)

(use-package helm :defer t :ensure t :pin melpa :diminish helm-mode :disabled
;:bind ("M-x" . helm-M-x)
:init (helm-mode 1)
;; helm always bottom
(add-to-list 'display-buffer-alist
            `(,(rx bos "*helm" (* not-newline) "*" eos)
                    (display-buffer-in-side-window)
                    (inhibit-same-window . t)
                    (window-height . 0.4)))

(use-package helm-projectile :ensure t :pin melpa :disabled
:after projectile
:init (helm-projectile-on)
))
(use-package helm-company :ensure t :pin melpa :disabled
:after helm company
:init
    (define-key company-mode-map   (kbd "C-q") 'helm-company)
    (define-key company-active-map (kbd "C-q") 'helm-company)
)
(use-package helm-descbinds :ensure t :pin melpa :disabled
:after helm
:init (helm-descbinds-mode)
)
(use-package helm-swoop :ensure t :pin melpa :defer t :disabled
:after helm
:init (evil-leader/set-key "fw" 'helm-swoop)
)

(use-package helm-ag :ensure t :pin melpa :defer t :disabled
:after helm
:init (evil-leader/set-key "fat" 'helm-do-ag-this-file
                           "fab" 'helm-do-ag-buffers
                           "far" 'helm-do-ag-project-root))

(use-package helm-system-packages :ensure t :pin melpa :defer t :disabled
:init (require 'em-tramp)
      (setq password-cache t)
      (setq password-cache-expiry 3600)
      (evil-leader/set-key "usp" 'helm-system-packages))

(use-package helm-dash :ensure t :pin melpa :defer t :disabled
:init (evil-leader/set-key "hDs" 'helm-dash
                           "hDi" 'helm-dash-install-user-docset)
)

;(use-package helm-rtags :ensure t :disabled
;:after (helm rtags)
;:config (setq rtags-display-result-backend 'helm))

(use-package helm-flyspell :ensure t :pin melpa :defer t :disabled
:after (helm flyspell)
:init (evil-leader/set-key "s" 'helm-flyspell-correct)
)

(use-package company :ensure t :pin melpa
:init (global-company-mode 1)
:config 
    (setq company-idle-delay 0)
    (setq company-minimum-prefix-length 1)
    (setq company-show-numbers t)
    (define-key company-active-map (kbd "M-n") 0)
    (define-key company-active-map (kbd "M-p") 0)
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-active-map (kbd "C-p") 'company-select-previous)
)

(use-package company-quickhelp :ensure t :pin melpa
:after  company
:config (company-quickhelp-mode)
       ;(evil-leader/set-key "hch" 'company-quickhelp-manual-begin)
)

(use-package company-statistics :ensure t :pin melpa
:after  company
:config (company-statistics-mode)
)

(use-package company-tabnine :ensure t :pin melpa
:after  company
:config (add-to-list 'company-backend #'company-tabnine)
        (company-tng-configure-default)
        (setq company-frontends '(company-tng-frontend
                                  company-pseudo-tooltip-frontend
                                  company-echo-metadata-frontend))
)

(use-package lsp-mode :ensure t :pin melpa
:commands (lsp-prog-major-mode-enable)
:hook (prog-major-mode . lsp-prog-major-mode-enable)
:config (setq lsp-inhibit-message t)
      (setq lsp-message-project-root-warning t)
      (setq create-lockfiles nil)
)

(use-package lsp-ui :ensure t :pin melpa
:after  lsp-mode
:hook   (lsp-mode . lsp-ui-mode)
:config (setq scroll-margin 0)
        (require 'lsp-clients)
)

(use-package company-lsp :ensure t :pin melpa
:after   (lsp-mode company)
:init    (add-to-list 'company-backends #'company-lsp)
)

(use-package flycheck :ensure t :pin melpa
:hook   company
:config (setq flycheck-clang-language-standard "c++17")
        (global-flycheck-mode t)
)
(use-package flycheck-pos-tip :ensure t :pin melpa
:after   flycheck
:config (flycheck-pos-tip-mode))

(use-package flycheck-inline :ensure t :pin melpa
:after flycheck
:config
    (setq flycheck-inline-display-function
        (lambda (msg pos)
            (let* ((ov (quick-peek-overlay-ensure-at pos))
                (contents (quick-peek-overlay-contents ov)))
            (setf (quick-peek-overlay-contents ov)
                    (concat contents (when contents "\n") msg))
            (quick-peek-update ov)))
        flycheck-inline-clear-function #'quick-peek-hide)
    (global-flycheck-inline-mode)
)

(use-package yasnippet :ensure t :pin melpa
;https://github.com/joaotavora/yasnippet
:after (company)
:config
  (use-package yasnippet-snippets :ensure t :pin melpa :after yansippet)
  (evil-leader/set-key "hyl" 'company-yasnippet)
  (setq yas-snippet-dirs '("~/.emacs.d/yas/"))
  (yas-global-mode t)
  (yas-reload-all t)
)
(use-package auto-yasnippet :ensure t :pin melpa
;https://github.com/abo-abo/auto-yasnippet
:after yasnippet
:config (evil-leader/set-key "hyc" 'aya-create)
        (evil-leader/set-key "hye" 'aya-expand)
)

(use-package cc-mode
:config (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
)

(use-package company-c-headers :ensure t :pin melpa
:after (company cc-mode)
:config (add-to-list 'company-backends 'company-c-headers)
)
(use-package clang-format :ensure t :pin melpa
:after (cc-mode)
:config (evil-leader/set-key "hcf" 'clang-format-regieon)
)
(use-package rtags :ensure t :pin melpa
:after (cc-mode)
:config
    (setq rtags-autostart-diagnostics t)
    (rtags-diagnostics)
    (setq rtags-completions-enabled t) (rtags-enable-standard-keybindings)
    (evil-leader/set-key "hcs" 'rtags-find-symbol
                         "hcr" 'rtags-find-references) 
)

(use-package ivy-rtags :ensure t :pin melpa
:after  (ivy rtags)
:config (setq rtags-display-result-backend 'ivy)
)

(use-package company-rtags :ensure t :pin melpa 
:after  (company rtags)
:config (add-to-list 'company-backend 'company-rtags))
(use-package flycheck-rtags :ensure t :pin melpa
:preface
    (defun my-flycheck-rtags-setup ()
        (flycheck-select-checker 'rtags)
        (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
        (setq-local flycheck-check-syntax-automatically nil))
:config
    (add-hook 'c-mode-hook    #'my-flycheck-rtags-setup)
    (add-hook 'c++-mode-hook  #'my-flycheck-rtags-setup)
    (add-hook 'objc-mode-hook #'my-flycheck-rtags-setup)
    (add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard   "c++17")))
    (add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++17")))
)

(use-package cmake-ide :ensure t :pin melpa
:after cc-mode
:config
    (require 'subr-x)
    (cmake-ide-setup)
    (setq cmake-ide-flags-c++ (append '("-std=c++17")))
    (defadvice cmake-ide--run-cmake-impl
      (after copy-compile-commands-to-project-dir activate)
      (if (file-exists-p (concat project-dir "/compile_commands.json"))
      (progn 
      (cmake-ide--message "[advice] found compile_commands.json" )
      (copy-file (concat project-dir "compile_commands.json") cmake-dir)
      (cmake-ide--message "[advice] copying compile_commands.json to %s" cmake-dir))
      (cmake-ide--message "[advice] couldn't find compile_commands.json" )))
)

(use-package irony :ensure t :pin melpa :diminish irony-mode
:after cc-mode
:config
    (setq irony-additional-clang-options '("-std=c++17"))
    (setq irony-cdb-search-directory-list (quote ("." "build" "bin")))
    (add-hook 'c++-mode-hook   'irony-mode)
    (add-hook 'c-mode-hook     'irony-mode)
    (add-hook 'objc-mode-hook  'irony-mode)
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
)

(use-package irony-eldoc :ensure t :pin melpa
:after (irony eldoc)
:hook irony-mode
)
(use-package company-irony :ensure t :pin melpa
:after  (company irony)
:config (add-to-list 'company-backends 'company-irony)
)
(use-package flycheck-irony :ensure t :pin melpa
:after  (flycheck irony)
:config (flycheck-irony-setup)
)
(use-package company-irony-c-headers :ensure t :pin melpa
:after  (company-c-headers irony)
:config (add-to-list 'company-backends 'company-irony-c-headers)
)

(use-package dap-mode :ensure t :pin melpa :defer t
:init   (evil-leader/set-key "dd" 'dap-debug)
:config (require 'dap-lldb)
)

(use-package gdb-mi 
:load-path "lisp/emacs-gdb"
:commands gdb-executable
:init   (evil-leader/set-key "de" 'gdb-executable)
:config (setq-default gdb-show-main t)
        (setq-default gdb-many-windows t)
        (fmakunbound 'gdb)
        (fmakunbound 'gdb-enable-debug)
        (evil-leader/set-key "dn" 'gdb-next)
        (evil-leader/set-key "di" 'gdb-step)
        (evil-leader/set-key "df" 'gdb-finish)
      ;(evil-leader/set-key "dt" '(lambda () (call-interactively 'gub-tbreak) (call-interactively 'gud-cont)))
)

(use-package eldoc :ensure t :pin melpa :diminish eldoc-mode)
(use-package eldoc-rtags
:after (eldoc-rtags)
:preface
    (defun fontify-string (str mode)
        "Return STR fontified according to MODE."
        (with-temp-buffer
            (insert str)
            (delay-mode-hooks (funcall mode))
            (font-lock-default-function mode)
            (font-lock-default-fontify-region
            (point-min) (point-max) nil)
            (buffer-string)
        )
    )

    (defun rtags-eldoc-function ()
        (let ((summary (rtags-get-summary-text)))
            (and summary
                (fontify-string
                (replace-regexp-in-string
                "{[^}]*$" ""
                (mapconcat
                    (lambda (str) (if (= 0 (length str)) "//" (string-trim str)))
                    (split-string summary "\r?\n")
                    " "))
                major-mode))))

    (defun rtags-eldoc-mode ()
        "rtags eldoc extensions"
        (interactive)
        (setq-local eldoc-documentation-function #'rtags-eldoc-function)
        (eldoc-mode 1)
    )
:config
    (add-hook 'c-mode-hook   'rtags-eldoc-mode)
    (add-hook 'c++-mode-hook 'rtags-eldoc-mode)
)

(use-package slime :ensure t :pin melpa :disabled
:commands slime
:init
    (setq inferior-lisp-program (or (executable-find "sbcl")
                                    (executable-find "/usr/bin/sbcl")
                                    (executable-find "/usr/sbin/sbcl"
                                    "sbcl")))
:config
    (require 'slime-autoloads)
    (slime-setup '(slime-fancy))
)
(use-package elisp-slime-nav :ensure t :pin melpa :diminish elisp-slime-nav-mode
:after slime
:hook ((emacs-lisp-mode ielm-mode) . elisp-slime-nav-mode)
)

(use-package prettify-symbol :no-require t
:init (add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)
      (add-hook 'lisp-mode-hook       'prettify-symbols-mode)
      (add-hook 'org-mode-hook        'prettify-symbols-mode)
)

(use-package paredit :ensure t :pin melpa :disabled
:init
(add-hook 'emacs-lisp-mode-hook #'paredit-mode)
;; enable in the *scratch* buffer
(add-hook 'lisp-interaction-mode-hook #'paredit-mode)
(add-hook 'ielm-mode-hook #'paredit-mode)
(add-hook 'lisp-mode-hook #'paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)
(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode t)))
)

(defun racer-install ()
    "Racer install-linux" 
    (interactive)
    (eshell-command "rustup toolchain add nightly")
    (eshell-command "rustup component add rust-src")
    (eshell-command "cargo +nightly install racer")
)

(defun rust-install ()
    "Rust and Racer install-linux" 
    (interactive)
    (eshell-command "curl https://sh.rustup.rs -sSf | sh")
    (racer-install)
)

(use-package rust-mode :ensure t :pin melpa
:commands rust-mode
:mode (("\\.rs\\'" . rust-mode))
:config (evil-leader/set-key "hrf" 'rust-format-buffer)
;:config (setq rust-format-on-save t)
;(add-hook 'rust-mode-hook (lambda () (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)))
)

(use-package flycheck-rust :ensure t :pin melpa 
:after  (flycheck rust-mode)
:config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
)

(use-package racer :ensure t :pin melpa
:after  rust-mode
:config (add-hook 'rust-mode-hook  #'racer-mode)
        (add-hook 'racer-mode-hook #'company-mode) 
        (add-hook 'racer-mode-hook #'eldoc-mode) 
)

(use-package company-racer :ensure t :pin melpa
:after (company racer)
:config (add-to-list 'company-backends 'company-racer)
)

(use-package cargo :ensure t :pin melpa
:after  rust-mode
:config (add-hook 'rust-mode-hook 'cargo-minor-mode)
        (evil-leader/set-key "hrb" 'cargo-process-build
                             "hrr" 'cargo-process-run
                             "hrt" 'cargo-process-test)
)

(use-package haskell-mode :ensure t :pin melpa :defer t)

(use-package yaml-mode :ensure t :pin melpa
:commands yaml-mode
:mode (("\\.yaml\\'" . yaml-mode)
       ("\\.yml\\'"  . yaml-mode))
)

(use-package toml-mode :ensure t :pin melpa
:commands toml-mode
:mode ("\\.toml\\'" . toml-mode))

(use-package cmake-mode :ensure t :pin melpa
:commands cmake-mode
:mode (("\\.cmake\\'"    . cmake-mode)
       ("CMakeLists.txt" . cmake-mode))
:init (setq cmake-tab-width 4)      
)

(use-package markdown-mode :ensure t :pin melpa
:commands (markdown-mode gfm-mode)
:mode   (("\\README.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
:config (setq markdown-command "multimarkdown")
)

(use-package markdown-preview-mode :ensure t :pin melpa :defer t)
(use-package gh-md :ensure t :pin melpa :defer t
:init (evil-leader/set-key "hmr" 'gh-md-render-buffer)
)

(use-package easy-jekyll :ensure t :pin melpa
:commands easy-jekyll
:config (setq easy-jekyll-basedir "~/dev/blog/")
        (setq easy-jekyll-url "https://injae.github.io")
        (setq easy-jekyll-sshdomain "blogdomain")
        (setq easy-jekyll-root "/")
        (setq easy-jekyll-previewtime "300")
)

(use-package python-mode
:after python-mode
:interpreter ("python" . python-mode)
:mode   ("\\.py\\'" . python-mode)
        ("\\.wsgi$" . python-mode)
:init   (setq-default indent-tabs-mode nil)
:config (setq python-indent-offset 4)
)

(use-package pyenv-mode :ensure t :pin melpa
:after python-mode
:preface
    (defun projectile-pyenv-mode-set ()
        "Set pyenv version matching project name."
        (let ((project (projectile-project-name)))
            (if (member project (pyenv-mode-versions))
                (pyenv-mode-set project)
                (pyenv-mode-unset)
            )
        )
    )
:config
    (add-hook 'projectile-switch-project-hook 'projectile-pyenv-mode-set)
    (add-hook 'python-mode-hook 'pyenv-mode)
)
(use-package pyenv-mode-auto :ensure t :pin melpa :after python-mode)

(use-package anaconda-mode :ensure t :pin melpa
:after  python-mode
:config (add-hook 'python-mode-hook 'anaconda-mode)
        (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

(use-package company-anaconda :ensure t :pin melpa
:after  (company-mode anaconda-mode)
:config (add-to-list 'company-backends '(company-anaconda :with company-capf)))

(use-package company-jedi :ensure t :pin melpa
:after  (company python-mode)
:config (add-hook 'python-mode 'jedi:setup)
        (add-to-list 'company-backends 'company-jedi)
;:config (jedi:complete-on-dot t)
)

(use-package i3wm :ensure t :pin melpa :defer t :disabled)

(use-package company-shell :ensure t :pin melpa :defer t
:after (company eshell)
:init (add-to-list 'company-backends '(company-shell company-shell-env company-fish-shell))
)

(use-package lsp-go :ensure t :pin melpa
:after  (lsp-mode go-mode)
:hook   (go-mode . lsp-go-enable)
:custom (lsp-go-language-server-flags '("-gocodecompletion"
                                        "-diagnostics"
                                        "-lint-tool=golint"))
:commands lsp-go-enable
)

(use-package dumb-jump :ensure t :pin melpa
:after  company
:init   (evil-leader/set-key "hdo" 'dumb-jump-go-other-window)
        (evil-leader/set-key "hdj" 'dumb-jump-go)
        (evil-leader/set-key "hdi" 'dumb-jump-go-prompt)
        (evil-leader/set-key "hdx" 'dumb-jump-go-prefer-external)
        (evil-leader/set-key "hdz" 'dumb-jump-go-prefer-external-other-window)
:config (setq dumb-jump-selector 'ivy)
        (setq dumb-jump-force-searcher 'rg)
)

(use-package web-mode :ensure t :pin melpa
:commands (web-mode)
:mode     (("\\.html?\\'"  . web-mode)
           ("\\.xhtml$\\'" . web-mode)
           ("\\.vue\\'"    . web-mode))
:config   (setq web-mode-enable-engine-detection t)
)

(use-package json-mode :ensure t :pin melpa
:after web-mode
:commands json-mode
:mode (("\\.json\\'"       . json-mode)
       ("/Pipfile.lock\\'" . json-mode))
)
