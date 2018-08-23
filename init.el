;;; Commentry: --- my init.el file"
;;;###Code:


(if (display-graphic-p)
    (progn
      (setq initial-frame-alist
            '(
              (tool-bar-lines . 0)
              (width . 200) ; chars
              (height . 60) ; lines
              (left . 100)
              (top . 60)))
      (setq default-frame-alist
            '(
              (tool-bar-lines . 0)
              (width . 200)
              (height . 60)
              (left . 100)
              (top . 60))))
  (progn
    (setq initial-frame-alist '( (tool-bar-lines . 0)))
      (setq default-frame-alist '( (tool-bar-lines . 0)))))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq make-backup-files nil)


;(set-language-enviroment "Korean")
(prefer-coding-system 'utf-8)
(global-font-lock-mode nil)

(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

(setq echo-keystrokes 0.5)
(setq global-hl-line-mode +1)

(defalias 'yes-or-no-p 'y-or-n-p)

;(setq multi-term-program "/bin/zsh")
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; tab setting
(setq-default indent-tabs-mode nil)
(defun my-set-indent (n)
    (setq-default tab-width n)
    (electric-indent-mode -1)
    (setq c-basic-offset n)
    (setq lisp-indent-offset n)
    (setq indent-line-function 'insert-tab)

)
(my-set-indent 4)
(global-set-key (kbd "<backtab>") 'un-indent-by-removing-4-spaces)
(defun un-indent-by-removing-4-spaces ()
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

;;other module and user script
;(concat user-emacs-directory (convert-standard-filename "lisp/"))

;;auto install packages
(require 'package)
    (add-to-list 'package-archives '("elpa"      . "https://tromey.com/elpa/")            t)
    (add-to-list 'package-archives '("org"       . "http://orgmode.org/elpa/")            t)
    (add-to-list 'package-archives '("melpa"     . "http://melpa.milkbox.net/packages/")  t)
    (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
    (package-initialize)
    (unless (package-installed-p 'use-package)
        (package-refresh-contents)
        (package-install 'use-package)
    )

;; vim key map road
(use-package evil
    :ensure t
    :init
        (evil-mode t)
        (setq evil-want-C-u-scroll t)
    :config
        (evil-set-initial-state 'calender-mode    'emacs)
        (evil-set-initial-state 'calculater-mode  'emacs)
        (evil-set-initial-state 'git-rebase-mode  'emacs)
        (evil-set-initial-state 'magit-blame-mode 'emacs)
        (setq-default evil-symbol-word-search t)
)

(use-package evil-leader
    :ensure t
    :config
        (setq evil-leader/leader "<SPC>")
        (global-evil-leader-mode t)
        (evil-leader/set-key "e r"   'eval-buffer)
        (evil-leader/set-key "<SPC>" 'helm-M-x)
        (evil-leader/set-key "f"     'find-file)
        (evil-leader/set-key "e f"   (lambda ()(interactive)(find-file "~/.emacs.d/init.el")))
)

(add-to-list 'load-path "~/.emacs.d/lisp/linum-highlight-current-line-number/")
(require 'linum-highlight-current-line-number)
(setq linum-format 'linum-highlight-current-line-number)
(global-linum-mode t)

(use-package doom-themes
    :init
        (doom-themes-neotree-config)
        (doom-themes-org-config)
        (load-theme 'doom-one t)
)

(use-package all-the-icons :ensure t)
(use-package neotree
    :ensure t
    :init
    (progn
        (setq-default neo-window-width 30)
        (setq-default neo-smart-open t)
        (setq-default neo-dont-be-alone t)
        (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
    )
        (add-hook 'neotree-mode-hook
            (lambda ()
                (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
                (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-quick-look)
                (define-key evil-normal-state-local-map (kbd "q")   'neotree-hide)
                (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
                (define-key evil-normal-state-local-map (kbd "g")   'neotree-refresh)
                (define-key evil-normal-state-local-map (kbd "n")   'neotree-next-line)
                (define-key evil-normal-state-local-map (kbd "p")   'neotree-previous-line)
                (define-key evil-normal-state-local-map (kbd "A")   'neotree-stretch-toggle)
                (define-key evil-normal-state-local-map (kbd "H")   'neotree-hidden-file-toggle)
            )
        )
    (neotree-show)
:config
        (setq neo-show-hidden-files t)
        (evil-leader/set-key "n" 'neotree-toggle)
        (neotree-show)
        (add-hook 'neotree-mode-hook (lambda () (linum-mode nil)))
)


;; theme load
;(use-package spacemacs-theme :defer t :init (load-theme 'spacemacs-dark t))

; parn brak ... color set
(use-package rainbow-delimiters
    :ensure t
    :init
        (rainbow-delimiters-mode-enable)
    :config
        (add-hook 'html-mode-hook       'rainbow-delimiters-mode)
        (add-hook 'css-mode-hook        'rainbow-delimiters-mode)
        (add-hook 'c++-mode-hook        'rainbow-delimiters-mode)
        (add-hook 'c-mode-hook          'rainbow-delimiters-mode)
        (add-hook 'lisp-mode-hook       'rainbow-delimiters-mode)
        (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
)

(use-package spaceline
    :ensure t
    :config
    (use-package nyan-mode
        :ensure t
        :init
            (setq-default nyan-wavy-trail t)
            (nyan-mode)
            (nyan-start-animation)
            (nyan-refresh)
    )
)

(use-package spaceline-config
    :ensure spaceline
    :init
        (custom-set-faces '(mode-line-buffer-id ((t nil)))) ;; blend well with tango-dark
        (spaceline-helm-mode 1)
        (spaceline-toggle-buffer-encoding-on)
        (spaceline-toggle-line-column-on)
        (spaceline-toggle-flycheck-info-on)
        (spaceline-spacemacs-theme)
        (spaceline-toggle-buffer-encoding-on)
        (spaceline-toggle-evil-state-on)
        (spaceline-toggle-nyan-cat-on)
    :config
        (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
        (setq evil-normal-state-tag   (propertize "COMMAND "))
        (setq evil-emacs-state-tag    (propertize "EMACS   "))
        (setq evil-insert-state-tag   (propertize "INSERT  "))
        (setq evil-replace-state-tag  (propertize "REPLACE "))
        (setq evil-motion-state-tag   (propertize "MOTION  "))
        (setq evil-visual-state-tag   (propertize "VISUAL  "))
        (setq evil-operator-state-tag (propertize "OPERATE "))
)

(use-package all-the-icons :ensure t)
(use-package spaceline-all-the-icons
    :ensure t
    :after spaceline
    :config
        ;(spaceline-all-the-icons-theme)
        ;(spaceline-toggle-all-the-icons-buffer-id-on)
        ;(spaceline-toggle-all-the-icons-git-status-on)
        ;(spaceline-toggle-all-the-icons-nyan-cat)
        ;(spaceline-toggle-all-the-icons-flycheck-status)
        ;(spaceline-toggle-all-the-icons-narrowed)
        ;(spaceline-all-the-icons--setup-package-updates)
        ;(spaceline-all-the-icons--setup-git-ahead)
        ;(spaceline-all-the-icons--setup-neotree)
        (setq inhibit-compacting-font-caches t)
)

(use-package smartparens
    :ensure t
    :init (smartparens-global-mode)
    :config
    (require 'evil-smartparens)
        (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)
)

;(use-package whitespace
;    :ensure t
;    :init
;        (setq whitespace-style '(tab-mark space))
;        (global-whitespace-mode)
;)

(use-package magit
    :ensure t
    :config
        (evil-leader/set-key "g s" 'magit-status)
)


(use-package which-key
    :ensure t
    :init (which-key-mode t)
)

; color code background color set
(use-package rainbow-mode
    :ensure t
    :init
        (add-hook 'html-mode-hook       'rainbow-mode)
        (add-hook 'css-mode-hook        'rainbow-mode)
        (add-hook 'c++-mode-hook        'rainbow-mode)
        (add-hook 'c-mode-hook          'rainbow-mode)
        (add-hook 'lisp-mode-hook       'rainbow-mode)
        (add-hook 'emacs-lisp-mode-hook 'rainbow-mode)
)


(use-package projectile
    :ensure t
    :init
        (projectile-mode t)
    :config
        (evil-leader/set-key "p" 'projectile-command-map)
)

;; auto complite mode road
(use-package company
    :ensure t
    :init (add-hook 'after-init-hook 'global-company-mode)
    :config
        (setq company-idle-delay   0)
        (setq company-minimum-prefix-length 1)
        (setq company-show-numbers t)
        (define-key company-active-map (kbd "M-n") 0)
        (define-key company-active-map (kbd "M-p") 0)
        (define-key company-active-map (kbd "C-n") 'company-select-next)
        (define-key company-active-map (kbd "C-p") 'company-select-previous)
        (use-package company-c-headers
            :ensure t
            :init (add-to-list 'company-backends 'company-c-headers)
        )
)

(use-package irony
    :ensure t
    :init
        (add-hook    'c++-mode-hook        'irony-mode)
        (add-hook    'c-mode-hook          'irony-mode)

    :config
    (progn
        (use-package company-irony
            :ensure t
            :config
                (require 'company)
                (add-to-list 'company-backends     'company-irony)
                (add-hook    'irony-mode-hook      'irony-cdb-autosetup-compile-options)
        )
        (use-package flycheck-irony
            :ensure t
            :config (eval-after-load 'flycheck #'flycheck-irony-setup)
        )
        (use-package company-irony-c-headers
            :ensure t
            :init
                (eval-after-load 'company '(add-to-list 'company-backends '(company-irony-c-headers)))
        )
        (use-package clang-format
            :ensure t
            :config (global-set-key [C-M-tab] 'clang-format-regieon)
        )
    )
)

(with-eval-after-load 'company
    (add-hook 'c++-mode-hook 'company-mode)
    (add-hook 'emacs-lisp-mode-hook 'company-mode)
    (add-hook 'lisp-mode-hook 'company-mode)
)

(use-package helm
    :ensure t
    :bind ("M-x" . helm-M-x)
    :config
        (helm-mode 1)
        ;; helm always bottom
        (add-to-list 'display-buffer-alist
                    `(,(rx bos "*helm" (* not-newline) "*" eos)
                         (display-buffer-in-side-window)
                         (inhibit-same-window . t)
                         (window-height . 0.4)))
    :diminish helm-mode
)

(use-package helm-projectile
    :ensure t
    ;:bind("M-t" . helm-projectile-find-file)
    :config (helm-projectile-on)
)

(use-package helm-company
    :ensure t
    :config
        (eval-after-load 'company
            '(progn
                (define-key company-mode-map (kbd "C-q") 'helm-company)
                (define-key company-active-map (kbd "C-q") 'helm-company)
            ))
)

(use-package helm-descbinds
    :ensure t
    :config (helm-descbinds-mode)
)

(use-package cmake-mode   :ensure t)
(use-package yaml-mode    :ensure t)
(use-package haskell-mode :ensure t)

(use-package elisp-slime-nav
    :ensure t
    :init
    (dolist (i '(emacs-lisp-mode-hook ielm-mode-hook))
        (add-hook i 'elisp-slime-nav-mode)
    )
    :diminish elisp-slime-nav-mode
)

(use-package flycheck
    :ensure t
    :init   (global-flycheck-mode t)
)


(use-package flycheck-pos-tip
    :ensure t
    :config
    (with-eval-after-load 'flycheck (flycheck-pos-tip-mode))
)

(use-package flyspell
    :ensure t
    :init
    (progn
        (add-hook 'prog-mode-hook       'flyspell-prog-mode)
        (add-hook 'text-mode-hook       'flyspell-mode-hook)
        (add-hook 'c-mode-hook          'flyspell-mode-hook)
        (add-hook 'yaml-mode-hook       'flyspell-mode-hook)
        (add-hook 'lisp-mode-hook       'flyspell-mode-hook)
        (add-hook 'emacs-lisp-mode-hook 'flyspell-mode-hook)
    )
    
    :config
        (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-corrent-word)
        (define-key flyspell-mode-map  (kbd "<C-tab>")  #'flyspell-correct-word)
)

(use-package undo-tree
    :ensure t
    :bind (("C-u"   . undo-tree-undo)
           ("C-r"   . undo-tree-redo)
    )
    :init
        (global-undo-tree-mode)
        (defalias 'redo 'undo-tree-redo)
        (defalias 'undo 'undo-tree-undo)
    :diminish undo-tree-mode
)


(use-package yasnippet
    :ensure t
    :config
        (yas-global-mode t)
        (setq yas-indent-line nil)
    :diminish yas-minor-mode
)

(require 'server)
(server-start)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(irony-cdb-search-directory-list (quote ("." "build" "bin")))
 '(package-selected-packages
        (quote
            (hlinum-mode linum-highlight-current-line-number doom-themes doom-modeline neotree nyan-mode boxquote evil-smartparens smartparens spaceline-all-the-icons nlinum clang-format flycheck-irony company-c-headers yaml-mode zenburn-theme company-irony-c-headers powerline flycheck-pos-tip magit discover-my-major spacemacs-theme helm-descbinds use-package helm-company flycheck evil company-irony cmake-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line-buffer-id ((t nil))))
