(add-to-list 'load-path "~/.emacs.d/lisp/")
(setq-default custom-file (expand-file-name ".config.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file))

(setq *is-mac*     (eq system-type 'darwin))
(setq *is-windows* (eq system-type 'windows-nt))
(setq *is-cygwin*  (eq system-type 'cygwin))
(setq *is-linux*   (or (eq system-type 'gnu/linux) (eq system-type 'linux)))
(setq *is-unix*    (or *is-linux* (eq system-type 'usg-unix-v) (eq system-type 'berkeley-unix)))

(when window-system
    (tool-bar-mode -1)
    (menu-bar-mode -1)
    (if (boundp 'fringe-mode)     (fringe-mode -1))
    (if (boundp 'scroll-bar-mode) (scroll-bar-mode -1))
    (tooltip-mode -1)
    ;; 마우스 사용가능
    (xterm-mouse-mode)
    ;; default window size options
    ;(if (display-graphic-p)
    ;    (progn
    ;    (setq initial-frame-alist
    ;            '(
    ;            (tool-bar-lines . 0)
    ;            (width . 200) ; chars
    ;            (height . 60) ; lines
    ;            (left . 100)
    ;            (top . 60)))
    ;    (setq default-frame-alist
    ;            '(
    ;            (tool-bar-lines . 0)
    ;            (width . 200)
    ;            (height . 60)
    ;            (left . 100)
    ;            (top . 60))))
    ;(progn
    ;    (setq initial-frame-alist '( (tool-bar-lines . 0)))
    ;    (setq default-frame-alist '( (tool-bar-lines . 0)))))
)

(setq scroll-step 1)
(setq scroll-conservatively 10000)

(setq backup-inhibited t)
(setq auto-save-default nil)
(setq make-backup-files nil)

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

;; +------------+------------+
;; | 일이삼사오 | 일이삼사오 |
;; |------------+------------|
;; | 1234567890 | 1234567890 |
;; +------------+------------+
;; text utf-8 setting
(set-language-environment "Korean")
(prefer-coding-system 'utf-8)
(setq locale-coding-system   'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(set-face-attribute   'default nil       :family "DejaVu Sans Mono" :height 110)
(set-fontset-font nil 'hangul (font-spec :family "D2Coding" :pixelsize 18))
(setq face-font-rescale-alist '(("D2coding" . 1.2)))
(setq-default line-spacing 3)
(global-font-lock-mode t)

;(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(defun launch-separate-emacs-in-terminal ()
(suspend-emacs "fg ; emacs -nw"))

(defun launch-separate-emacs-under-x ()
(call-process "sh" nil nil nil "-c" "emacs &"))

(defun restart-emacs ()
    (interactive)
    ;; We need the new emacs to be spawned after all kill-emacs-hooks
    ;; have been processed and there is nothing interesting left
    (let ((kill-emacs-hook (append kill-emacs-hook (list (if (display-graphic-p) #'launch-separate-emacs-under-x
                                                                                 #'launch-separate-emacs-in-terminal)))))
         (save-buffers-kill-emacs))
)



(use-package beacon :ensure t :init (beacon-mode t)) 
(use-package git-gutter :ensure t
:init 
    (setq-default display-line-numbers-width 2)
    (global-git-gutter-mode t)
    (global-display-line-numbers-mode t)
    (global-hl-line-mode t)
:config
    (setq git-gutter:lighter " gg")
    (setq git-gutter:window-width 1)
    (setq git-gutter:modified-sign ".")
    (setq git-gutter:added-sign    "+")
    (setq git-gutter:deleted-sign  "-")
    (set-face-foreground 'git-gutter:added    "#daefa3")
    (set-face-foreground 'git-gutter:deleted  "#FA8072")
    (set-face-foreground 'git-gutter:modified "#b18cce")
)

(use-package doom-themes
:init (load-theme 'doom-one t)
:config
    (doom-themes-neotree-config)
    (doom-themes-org-config)
)

(load-library "hideshow")
    (global-set-key (kbd "<C-l>") 'hs-show-block)
    (global-set-key (kbd "<C-h>")  'hs-hide-block)
    (add-hook 'c-mode-common-hook     'hs-minor-mode)
    (add-hook 'emacs-lisp-mode-hook   'hs-minor-mode)
    (add-hook 'java-mode-hook         'hs-minor-mode)
    (add-hook 'lisp-mode-hook         'hs-minor-mode)
    (add-hook 'perl-mode-hook         'hs-minor-mode)
    (add-hook 'sh-mode-hook           'hs-minor-mode)

;(use-package aggressive-indent :ensure t :pin melpa
;:init (global-aggressive-indent-mode)
;)

(use-package indent-guide :ensure t
:init ;(indent-guide-global-mode)
:config
    (setq indent-guide-char      "|")
    (setq indent-guide-recursive t)
    ;(set-face-background 'indent-guide-face "dimgray")
    ;(setq indent-guide-delay     0.1)
)
(defun my-set-indent (n)
    (setq-default tab-width n)
    ;(electric-indent-mode t)
    (setq c-basic-offset n)
    (setq lisp-indent-offset n)
    (setq indent-line-function 'insert-tab)
)
(my-set-indent 4)
(setq-default indent-tabs-mode nil)

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
(global-set-key (kbd "<backtab>") 'un-indent-by-removing-4-spaces)
;(use-package highlight-indent-guides :ensure t
;    :init (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
;    :config 
;       (setq highlight-indent-guides-method 'character)
;       ;(set-face-background 'highlight-indent-guides-odd-face       "darkgray")
;       ;(set-face-background 'highlight-indent-guides-even-face      "dimgray" )
;       ;(set-face-background 'highlight-indent-guides-character-face "dimgray" )
;)

(use-package paren :ensure t 
:init   (show-paren-mode 1)
:config (setq show-paren-delay 0)
)

(use-package rainbow-delimiters :ensure t
:hook ((prog-mode text-mode) . rainbow-delimiters-mode)
)

(use-package smartparens :ensure t :pin melpa
:init (smartparens-global-mode)
:config 
    (use-package evil-smartparens :ensure t :pin melpa
    :init (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))
)

(use-package which-key :ensure t 
:init   (which-key-mode t) 
:config (which-key-enable-god-mode-support t))

(use-package evil :ensure t :pin melpa
:init
    (setq evil-want-integration t)
    (setq evil-want-keybinding nil)
    (setq evil-want-C-u-scroll t)
    (setq-default evil-symbol-word-search t)
    (evil-mode t)
)
(use-package evil-collection :ensure t :pin melpa
:after evil
:init 
    (setq evil-collection-setup-minibuffer t)
    (evil-collection-helm-setup)
    (evil-collection-magit-setup)
    (evil-collection-neotree-setup)
    (evil-collection-which-key-setup)
    (evil-collection-buff-menu-setup)
    (evil-collection-package-menu-setup)
    (evil-collection-init)
)
(use-package evil-leader :ensure t :defer t :pin melpa
:init (global-evil-leader-mode t)
:config
    (setq evil-leader/leader "<SPC>")
    (evil-leader/set-key
        "<SPC>" 'helm-M-x
        "er"    'restart-emacs
        "ff"    'find-file
        "pl"    'list-processes
        "ef"    (lambda ()(interactive)(find-file "~/.emacs.d/config.org"))
        "wf"    'toggle-frame-fullscreen
        "wh"    'shrink-window-horizontally
        "wj"    'enlarge-window
        "wk"    'shrink-window
        "wl"    'enlarge-window-horizontally
        )
    )

(use-package all-the-icons :ensure t)
(use-package spaceline :ensure t)
(use-package spaceline-config :ensure spaceline
:init (spaceline-spacemacs-theme)
:config
    (custom-set-faces '(mode-line-buffer-id ((t nil)))) ;; blend well with tango-dark
    (setq powerline-default-separator 'arrow)   ;; bar arrow wave utf-8
    (spaceline-toggle-buffer-id-on)
    (spaceline-toggle-input-method-on)
    (spaceline-toggle-buffer-modified-on)
    (spaceline-toggle-buffer-encoding-on)
    (spaceline-toggle-process-on)
    (spaceline-toggle-projectile-root-on)
    (spaceline-toggle-version-control-on)
    (spaceline-toggle-flycheck-error-on)
    (spaceline-toggle-flycheck-info-on)
    (spaceline-toggle-flycheck-warning-on)
    (spaceline-toggle-major-mode-on)
    (spaceline-toggle-minor-modes-off)
    (spaceline-toggle-line-column-on)
    (spaceline-toggle-window-number-on)
    (spaceline-toggle-buffer-encoding-on)
    (spaceline-toggle-evil-state-on)
    (spaceline-toggle-nyan-cat-on)
    (spaceline-helm-mode 1)
    (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
    (setq evil-normal-state-tag   (propertize "COMMAND "))
    (setq evil-emacs-state-tag    (propertize "EMACS   "))
    (setq evil-insert-state-tag   (propertize "INSERT  "))
    (setq evil-replace-state-tag  (propertize "REPLACE "))
    (setq evil-motion-state-tag   (propertize "MOTION  "))
    (setq evil-visual-state-tag   (propertize "VISUAL  "))
    (setq evil-operator-state-tag (propertize "OPERATE "))
)

;(use-package spaceline-all-the-icons :ensure t
;    :after spaceline
;    :config
;    ;(spaceline-all-the-icons-theme)
;    ;(spaceline-all-the-icons--setup-package-updates)
;    ;(spaceline-all-the-icons--setup-neotree)
;    ;(spaceline-all-the-icons--setup-git-ahead)
;    ;(spaceline-toggle-all-the-icons-eyebrowse-workspace-on)
;    ;(spaceline-toggle-all-the-icons-sunrise-on)
;    ;(spaceline-toggle-all-the-icons-sunset-on)
;    ;(spaceline-toggle-all-the-icons-time-on)
;    ;(spaceline-toggle-all-the-icons-weather-on)
;    ;(spaceline-toggle-all-the-icons-flycheck-status-on)
;    ;(spaceline-toggle-all-the-icons-flycheck-status-info-on)
;    ;(spaceline-toggle-all-the-icons-buffer-id-on)
;    ;(spaceline-toggle-all-the-icons-git-status-on)
;    ;(spaceline-toggle-all-the-icons-nyan-cat-on)
;    ;(spaceline-toggle-all-the-icons-narrowed-on)
;    ;(spaceline-toggle-all-the-icons-git-ahead-on)
;    ;(spaceline-toggle-all-the-icons-bookmark-on)
;    ;(spaceline-toggle-all-the-icons-projectile-on)
;    ;(spaceline-toggle-all-the-icons-window-number-on)
;    ;(spaceline-toggle-all-the-icons-mode-icon-on)
;    ;(spaceline-toggle-all-the-icons-battery-status-on)
;    ;(setq inhibit-compacting-font-caches t)
;)

(use-package nyan-mode :ensure t
    :init (nyan-mode)
    :config
    (setq-default nyan-wavy-trail t)
    (nyan-start-animation)
    (nyan-refresh))
(when window-system
    (use-package mode-icons :ensure t
        :init  
        (setq mode-icons-desaturate-active t)
        (mode-icons-mode)))
(use-package fancy-battery :ensure t
    :init   (fancy-battery-mode)
    :config (setq fancy-battery-show-percentage t))

(use-package helm :defer t :ensure t :diminish helm-mode
:bind ("M-x" . helm-M-x)
:init (helm-mode 1)
;; helm always bottom
(add-to-list 'display-buffer-alist
            `(,(rx bos "*helm" (* not-newline) "*" eos)
                    (display-buffer-in-side-window)
                    (inhibit-same-window . t)
                    (window-height . 0.4)))

(use-package helm-projectile :ensure t 
:after projectile
:init (helm-projectile-on)
))
(use-package helm-company :ensure t
:after helm company
:init
    (define-key company-mode-map   (kbd "C-q") 'helm-company)
    (define-key company-active-map (kbd "C-q") 'helm-company)
)
(use-package helm-descbinds :ensure t 
:after helm
:init (helm-descbinds-mode)
)
(use-package helm-swoop :ensure t :pin melpa
:after helm
:init (evil-leader/set-key "fw" 'helm-swoop)
)

(use-package projectile :defer t :ensure t
:init (projectile-mode t)
:config (evil-leader/set-key "p" 'projectile-command-map)
)

(use-package neotree :ensure t
:init 
    (setq projectile-switch-project-action 'neotree-projectile-action)
    (evil-leader/set-key "n" #'neotree-toggle)
:config
    (progn
        (setq-default neo-window-width 30)
        (setq-default neo-smart-open t)
        (setq-default neo-dont-be-alone t)
        (setq neo-force-change-root t)
        (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
    )
    (setq neo-show-hidden-files t)
)

(use-package ace-window :ensure t
:init   (evil-leader/set-key "wo" 'ace-window)
:config (setq aw-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8))
)

(use-package eyebrowse :ensure t
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

(setq gdb-show-main t)
(evil-leader/set-key "db" 'gud-break)
(evil-leader/set-key "dn" 'gud-next)
(evil-leader/set-key "di" 'gud-step)
(evil-leader/set-key "df" 'gud-finish)
(evil-leader/set-key "dt" '(lambda () (call-interactively 'gud-tbreak)
                                      (call-interactively 'gud-cont  )))

(use-package magit :ensure t  :pin melpa
:init   (evil-leader/set-key "gs" 'magit-status)
:config (setq vc-handled-backends nil)
)
(use-package evil-magit :ensure t :pin melpa
:after (evil magit)
:init  (evil-magit-init)
)
;(use-package magithub :ensure t :pin melpa
;:after magit
;:init (magithub-feature-autoinject t)
;      (setq magithub-clone-default-directory "~/github")   
;)

(use-package evil-ediff :ensure t :pin melpa
    :init (evil-ediff-init)
)

(use-package undo-tree :ensure t :diminish undo-tree-mode
:init
    (global-set-key (kbd "C-u") #'undo-tree-undo)
    [global-set-key (kbd "C-r") #'undo-tree-redo]
    (evil-leader/set-key "uu"    'undo-tree-undo)
    (evil-leader/set-key "ur"    'undo-tree-undo)
    (defalias 'redo 'undo-tree-redo)
    (defalias 'undo 'undo-tree-undo)
    (global-undo-tree-mode)
)

(evil-leader/set-key "oe" 'org-edit-src-code)    
(evil-leader/set-key "ok" 'org-edit-src-exit)

(use-package rainbow-mode :ensure t
    :hook (prog-mode
           text-mode
           html-mode
           css-mode
           c++-mode
           c-mode
           lisp-mode
           emacs-lisp-mode)
    :init (rainbow-mode)
)

(use-package docker          :ensure t :init (evil-leader/set-key "ud" 'docker)) 
(use-package dockerfile-mode :ensure t 
    :init (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(use-package buffer-move :ensure t
:init
    (evil-leader/set-key
        "bs" 'switch-to-buffer
        "bh" 'buf-move-left
        "bj" 'buf-move-down
        "bk" 'buf-move-up
        "bl" 'buf-move-right
    )
)

(use-package dash :ensure t :pin melpa
:init (dash-enable-font-lock)
)
(use-package dash-functional :ensure t :pin melpa
:after dash
)
;; if you want use helm-dash you use this command helm-dash-install-docset
(use-package helm-dash :ensure t :pin melpa
:after helm dash
)

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(defvar my-eshell " *BOTTOM-TERMINAL*" "my shell name,use eshell.")
(defun get-current-directory (&optional buffer)
  "get current directory."
  (if (not buffer)
      (file-name-directory (or (buffer-file-name) default-directory))
      (with-current-buffer buffer
      (file-name-directory (or (buffer-file-name) default-directory)))))

  (defun get-parent-dir (name)
  "Get the parent name dir."
  (locate-dominating-file default-directory name))

  (defun get-project-root-directory (buffer)
  "find current project root,for git or gradle."
      (with-current-buffer buffer
      (if (setq parent (cl-some #'get-parent-dir pop-find-parent-directory))
      parent
      (get-current-directory))))

  (defun eshell-pop-bottom()
  "pop eshell at bottom"
  (interactive)
  (let ((pos-buffer (current-buffer))
      (tmp-eshell (get-buffer my-eshell))
      (dir (get-current-directory)))
      ;;check if my-eshell exist,if not create one.
      (unless tmp-eshell
      (setq tmp-eshell (eshell 100))
      (with-current-buffer tmp-eshell
      (eshell/clear-scrollback)
      (rename-buffer my-eshell)
      (switch-to-buffer pos-buffer)))
      (setq window
      (select-window
      (display-buffer-in-side-window tmp-eshell '((side . bottom))) t))
      (set-window-dedicated-p window t)
      (when (not (equal pre-path dir))
      (eshell/cd dir)
      (eshell-send-input)
      (setq pre-path dir)))
      )
   (evil-leader/set-key "ut" 'eshell-pop-bottom)



(use-package company :ensure t
;:init (global-company-mode 1)
:init (company-mode)
:config 
    (setq company-idle-delay 0)
    (setq company-minimum-prefix-length 1)
    (setq company-show-numbers t)
    (define-key company-active-map (kbd "M-n") 0)
    (define-key company-active-map (kbd "M-p") 0)
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-active-map (kbd "C-p") 'company-select-previous)
)
(with-eval-after-load 'company
    (add-hook 'c++-mode-hook        'company-mode)
    (add-hook 'c-mode-hook          'company-mode)
    (add-hook 'racer-mode-hook      'company-mode)
    (add-hook 'lisp-mode-hook       'company-mode)
    (add-hook 'emacs-lisp-mode-hook 'company-mode)
)
;(use-package company-quickhelp :ensure t :pin melpa
;:init
;    ;(evil-leader/set-key "c h" 'company-quickhelp-manual-begin)
;    (company-quickhelp-mode)
;)

(use-package flycheck :ensure t :pin melpa
:init (global-flycheck-mode t)
      (setq flycheck-clang-language-standard "c++17")
)
(use-package flycheck-pos-tip :ensure t 
:after flycheck
:init (flycheck-pos-tip-mode))

(use-package yasnippet :ensure t
:init
(use-package yasnippet-snippets :ensure t)
(setq yas-snippet-dirs '("~/.emacs.d/yas/"))
(yas-global-mode)
(yas-reload-all)
)

(use-package company-c-headers :ensure t
:after company
:init (add-to-list 'company-backends 'company-c-headers)
)
(use-package clang-format :ensure t
:init (evil-leader/set-key "cf" 'clang-format-regieon)
)

(use-package rtags :ensure t
:after (helm flycheck)
:init
    (setq rtags-autostart-diagnostics t)
    (rtags-diagnostics)
    (setq rtags-completions-enabled t) (rtags-enable-standard-keybindings)
    (evil-leader/set-key "cs" 'rtags-find-symbol
                         "cr" 'rtags-find-references)
)
(use-package helm-rtags :ensure t :after (helm rtags)
:init (setq rtags-display-result-backend 'helm))

(use-package company-rtags :ensure t :after (company rtags)
:init (add-to-list 'company-backend 'company-rtags))
(use-package flycheck-rtags :ensure t
    :init
    (defun my-flycheck-rtags-setup ()
        (flycheck-select-checker 'rtags)
        (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
        (setq-local flycheck-check-syntax-automatically nil))
    (add-hook 'c-mode-hook    #'my-flycheck-rtags-setup)
    (add-hook 'c++-mode-hook  #'my-flycheck-rtags-setup)
    (add-hook 'objc-mode-hook #'my-flycheck-rtags-setup)
    (add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++17")))
    (add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++17")))
)

(use-package cmake-ide :ensure t
:init
    (cmake-ide-setup)
    (setq cmake-ide-flags-c++ (append '("-std=c++17")))
    (evil-leader/set-key "cc" 'cmake-ide-compile)
)

(use-package irony :ensure t :diminish irony-mode
:init 
    (setq irony-additional-clang-options '("-std=c++17"))
    (setq irony-cdb-search-directory-list (quote ("." "build" "bin")))
    (add-hook 'c++-mode-hook   'irony-mode)
    (add-hook 'c-mode-hook     'irony-mode)
    (add-hook 'objc-mode-hook  'irony-mode)
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
)
(use-package irony-eldoc :ensure t :after (irony eldoc)
    :hook irony-mode
)
(use-package company-irony :ensure t :after company
:init (add-to-list 'company-backends 'company-irony)
)
(use-package flycheck-irony :ensure t :after flycheck
:init (flycheck-irony-setup)
)
(use-package company-irony-c-headers :ensure t
:after company
:init (add-to-list 'company-backends 'company-irony-c-headers)
)

;;; cmake-ide + gdb/exec.
(defun run-process-in-comint (cmd)
(let* ((name (format "Process: %s" cmd))
        (buf (set-buffer (generate-new-buffer name)))
        (proc nil)
        (line-- (make-string 80 ?-))
        (proc-sentinal-fn (lambda (proc evt)
                            (insert (format "%s\n%s -- %s\n%s\n" line-- evt (current-time-string) line--))))
        (comint-mode-result (comint-mode)))
    ;;
    (switch-to-buffer-other-window buf)
    ;;
    (insert (format "Starting: %s\n%s\n" (current-time-string) line--))
    (setq proc (start-process-shell-command name buf cmd))
    (set-process-sentinel proc (lambda (proc evt)
                                (insert (format "==========\n%s -- (%s) %s\n"
                                                evt
                                                (process-exit-status proc)
                                                evt (current-time-string)))))
    ;;
    proc))
(defun cmake-ide-find-exe-file ()
(interactive)
(let* ((exec-files (seq-filter 'file-executable-p 
                                (directory-files-recursively
                                (cide--build-dir)
                                ".*")))
        (base-buffer-name (file-name-base (buffer-name)))
        (calc-dist (lambda (fn) (cons fn
                                    (levenshtein-distance
                                        base-buffer-name
                                        (file-name-base fn)))))
        (cdr-< (lambda (a b) (< (cdr a) (cdr b))))
        (distances (sort (mapcar calc-dist exec-files) cdr-<))
        ;;(---- (message distances))
        (nearest (car (first distances))))
    (cons nearest exec-files)))

(defun cmake-ide-gdb-files-source ()
"http://kitchingroup.cheme.cmu.edu/blog/2015/01/24/Anatomy-of-a-helm-source/"
(interactive)
(require 'seq)
`((name . "Executable file to debug")
    (candidates . ,(cmake-ide-find-exe-file))
    (action . (lambda (sel)
                (gdb (read-from-minibuffer
                    "Cmd: " (format "%s %s" gud-gdb-command-name sel)))))))

(defun cmake-ide-helm-run-gdb ()
(interactive)
(helm :sources (cmake-ide-gdb-files-source)))

(define-key c-mode-base-map (kbd "C-c d")
(function cmake-ide-helm-run-gdb))

(defun cmake-ide-run-files-source ()
(interactive)
(require 'seq)
`((name . "Executable file")
    (candidates . ,(cmake-ide-find-exe-file))
    (action . (lambda (sel)
                (run-process-in-comint (read-from-minibuffer "Cmd: " sel))))))

(defun cmake-ide-helm-run-exe ()
(interactive)
(helm :sources (cmake-ide-run-files-source)))

(define-key c-mode-base-map (kbd "C-c x") (function cmake-ide-helm-run-exe))

(use-package eldoc :ensure t :diminish eldoc-mode :after rtags)

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
    (interactive)
    (setq-local eldoc-documentation-function #'rtags-eldoc-function)
    (eldoc-mode 1)
)

(add-hook 'c-mode-hook 'rtags-eldoc-mode)
(add-hook 'c++-mode-hook 'rtags-eldoc-mode)

(use-package elisp-slime-nav :ensure t :diminish elisp-slime-nav-mode
:hook ((emacs-lisp-mode ielm-mode) . elisp-slime-nav-mode)
)
(add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)
(add-hook 'lisp-mode-hook       'prettify-symbols-mode)

(use-package rust-mode :ensure t)
;(use-package flymake-rust :ensure t)
(use-package racer :ensure t 
:init
(add-hook 'racer-mode-hook #'racer-mode) 
(add-hook 'racer-mode-hook #'eldoc-mode)
)
(use-package cargo :ensure t
:init (add-hook 'rust-mode-hook 'cargo-minor-mode)
)

(use-package haskell-mode :ensure t)

(use-package yaml-mode :ensure t)

(use-package cmake-mode :ensure t
:init (cmake-mode)
)


