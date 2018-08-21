
;;(setq custom-file "~/.eamcs.d/custon.el")
;;(load custom-file)

(if (display-graphic-p)
    (progn
      (setq initial-frame-alist
            '(
              (tool-bar-lines . 0)
              (width . 150) ; chars
              (height . 60) ; lines
              (left . 50)
              (top . 50)))
      (setq default-frame-alist
            '(
              (tool-bar-lines . 0)
              (width . 150)
              (height . 60)
              (left . 50)
              (top . 50))))
  (progn
    (setq initial-frame-alist '( (tool-bar-lines . 0)))
      (setq default-frame-alist '( (tool-bar-lines . 0)))))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-linum-mode t)

;(set-language-enviroment "Korean")
(prefer-coding-system 'utf-8)
(global-font-lock-mode nil)


(defalias 'yes-or-no-p 'y-or-n-p)

;; tab setting
(setq-default indent-tabs-mode nil)
(defun my-set-indent (n)
  (setq-default tab-width n)
  (setq c-basic-offset n)
  (setq lisp-indent-offset n)
  (setq indent-line-function 'insert-tab)
)
(my-set-indent 4)
;; shift tab settgin
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

;; toggle key setting
(load-library "hideshow")
    (global-set-key (kbd "<C-right>") 'hs-show-block)
    (global-set-key (kbd "<C-left>")  'hs-hide-block)
    (add-hook 'c-mode-common-hook     'hs-minor-mode)
    (add-hook 'emacs-lisp-mode-hook   'hs-minor-mode)
    (add-hook 'java-mode-hook         'hs-minor-mode)
    (add-hook 'lisp-mode-hook         'hs-minor-mode)
    (add-hook 'perl-mode-hook         'hs-minor-mode)
    (add-hook 'sh-mode-hook           'hs-minor-mode)

(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)


(require 'package)
    (add-to-list 'package-archives '("org"       . "http://orgmode.org/elpa/")            t)
    (add-to-list 'package-archives '("melpa"     . "http://melpa.milkbox.net/packages/")  t)
    (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
    (package-initialize)
    (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

;; theme load
(use-package spacemacs-theme
    :defer t
    :init (load-theme 'spacemacs-dark t)
)

(use-package which-key
    :ensure t
    :init
    (which-key-mode) 
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

; parn brak ... color set
(use-package rainbow-delimiters
    :ensure
    :init
        (add-hook 'html-mode-hook       'rainbow-delimiters-mode)
        (add-hook 'css-mode-hook        'rainbow-delimiters-mode)
        (add-hook 'c++-mode-hook        'rainbow-delimiters-mode)
        (add-hook 'c-mode-hook          'rainbow-delimiters-mode)
        (add-hook 'lisp-mode-hook       'rainbow-delimiters-mode)
        (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
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
    (evil-leader/set-key "r" 'eval-buffer)
    (evil-leader/set-key "<SPC>" 'execute-extended-command)

)

(use-package vimish-fold
    :ensure t
    :config
        (vimish-fold-global-mode t)
        (global-key-binding (kbd "<C-rigth>") 'vimish-fold)
        (global-key-binding (kbd "<C-left>" ) 'vimish-fold-delete)
)

(use-package projectile
    :ensure t
    :config
        (projectile-global-mode t)
        (evil-leader/set-key "p" 'projectile-command-map)
        ;(setq projectile-enable-caching t)
)

;; auto complite mode road
(use-package company
    :ensure t
    :init (add-hook 'after-init-hook 'global-company-mode)
    :config
        (setq company-idle-delay   0)
        (setq company-show-numbers t)
)

(use-package helm
    :ensure t
    :bind ("M-x" . helm-M-x)
    :config
        (helm-mode 1)
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
                (define-key company-mode-map (kbd "C-:") 'helm-company)
                (define-key company-active-map (kbd "C-:") 'helm-company)
            )
        )
)

(use-package helm-descbinds
    :ensure t
    :config (helm-descbinds-mode)
)

(use-package irony
    :ensure t
    :config
    (progn
        (use-package company-irony
            :ensure t
            :config
                (add-to-list 'company-backends 'company-irony)
                (add-hook 'irony-mode-hook 'electric-pair-mode)
                (add-hook 'c++-mode-hook 'irony-mode)
                (add-hook 'c-mode-hook 'irony-mode)
                (add-hook 'irony-mode-hook 'my-irony-mode-hook)
                (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
                (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
        )
    )
)

(use-package cmake-mode   :ensure t)

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
    :defer t
    :config
        (with-eval-after-load 'flycheck (flycheck-pos-tip-mode))
)

(require 'server)
(server-start)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
    '(package-selected-packages
         (quote
             (discover-my-major spacemacs-theme helm-descbinds use-package helm-company flycheck evil company-irony cmake-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
