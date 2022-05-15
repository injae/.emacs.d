;; python setting
(use-package elpy :ensure t
:ensure-system-package (jedi . "pip install --user jedi flake8 autopep8 black yapf importmagic")
:after python-mode
:hook (python-mode . elpy-enable)
:config (eldoc-mode 0)
)

(use-package anaconda-mode :ensure t
:after  python-mode
:config (add-hook 'python-mode-hook 'anaconda-mode)
        (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

(use-package company-anaconda :ensure t
:after  (company-mode anaconda-mode)
)

; eldoc setting
(use-package eldoc-rtags :no-require t :ensure nil :disabled
:after (eldoc rtags)
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


(use-package gdb-mi
:ensure (:host github :repo "weirdNox/emacs-gdb" :files ("*.el" "*.c" "*.h" "Makefile"))
:general (leader "de" 'gdb-executable
                "dn" 'gdb-next
                "di" 'gdb-step
                "df" 'gdb-finish)
:config (setq-default gdb-show-main t)
        (setq-default gdb-many-windows t)
        (fmakunbound 'gdb)
        (fmakunbound 'gdb-enable-debug)
)

(use-package evil-mc :ensure t  :disabled
:after evil
:preface
      (defun user-evil-mc-make-cursor-here ()
          (evil-mc-pause-cursors)
          (evil-mc-make-cursor-here))
:general (leader "emh" #'evil-mc-make-cursors-here
                 "ema" #'evil-mc-make-all-cursors
                 "emp" #'evil-mc-pause-cursors
                 "emr" #'evil-mc-resume-cursors
                 "emu" #'evil-mc-undo-all-cursors)
:config (global-evil-mc-mode 1)
)

(use-package evil-iedit-state :ensure t  :after (evil iedit) :disabled)

(use-package evil-smartparens :ensure t :disabled
:after (evil smartparens)
:init (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)
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

(use-package smart-tabs-mode :ensure t  :defer t :disabled
:config (smart-tabs-insinuate 'c 'c++)
)

(use-package ivy-prescient :ensure t :disabled
:after (ivy prescient)
)

(use-package exwm :ensure t  :disabled
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

(use-package evil-magit :ensure t :disabled
:after (evil magit)
:config  (evil-magit-init)
)


(use-package magit-todos :ensure t  :after magit :disabled)


;(use-package magit-delta :ensure t 
;:after magit
;:ensure-system-package 
;    :config
;    )
(use-package undo-fu :ensure t :disabled
:after evil
:general (leader "uu" 'undo-fu-only-undo
                 "ur" 'undo-fu-only-redo)
:config
    ;(global-undo-tree-mode -1) ; evil-mode auto call undo-tree-mode
    (evil-define-key 'normal 'global "u"         #'undo-fu-only-undo)
    (evil-define-key 'normal 'global (kbd "C-r") #'undo-fu-only-redo)
)

(use-package undo-fu-session :ensure t :disabled
:after undo-fu
:custom (undo-fu-session-incompletiable-files '("/COMMENT_EDITMSG\\'" "/git-rebase-todo\\'"))
:config (global-undo-fu-session-mode)
)

;(use-package undo-propose :ensure t 
;:after evil
;:commands undo-propose
;:init   (evil-define-key 'normal 'global (kbd "C-r") #'undo-propose)
;        (evil-define-key 'normal 'global "u" #'undo-only)
;:config (global-undo-tree-mode -1)
;)

(use-package vterm-toggle :ensure t :disabled
:general (leader "ut" '(vterm-toggle    :wk "toggle vterm buffer")
                 "tc" '(vterm-toggle-cd :wk "cd current dicectory")
                 "tn" '(vterm           :ew "open new vterm"))
:config (setq vterm-toggle-fullscreen-p nil)
        (setq vterm-toggle-project-root t)
        ;(setq vterm-toggle-cd-auto-create-buffer nil)
        (define-key vterm-toggle-map [(control return) #'vterm-toggle-insert-cd])
        (add-to-list 'display-buffer-alist
                     '((lambda(bufname _) (with-current-buffer bufname (equal major-mode 'vterm-mode)))
                                     (display-buffer-reuse-window display-buffer-in-direction)
                                     (direction . bottom)
                                     (reusable-frames . visible)
                                     (window-height . 0.3)))
       ;(add-hook 'counsel-tramp-post-command-hook (lambda () (vterm-toggle-cd)))
)

;(use-package execute-shell :no-require t :ensure nil
;:after eshell
;:preface
;(defun background-shell-command (command)
;    "run shell commmand background"
;    (interactive "sShell Command : ")
;    (call-process-shell-command "command" nil 0))
;:config (add-to-list 'display-buffer-alist
;        (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))
;)


(use-package ibuffer-projectile :ensure t  :disabled
:after (projectile)
:init  (add-hook 'ibuffer-hook (lambda () (ibuffer-projectile-set-filter-groups)
                                     (unless (eq ibuffer-sorting-mode 'alphabetic)
                                             (ibuffer-do-sort-by-alphabetic))))
)

;** Emacs Application Framework 
;#+BEGIN_SRC elisp
;(use-package eaf :load-path "~/.emacs.d/site-lisp/emacs-application-framework" :disabled
;    :custom
;    ; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
;    (eaf-browser-continue-where-left-off t)
;    (eaf-browser-enable-adblocker t)
;    (browse-url-browser-function 'eaf-open-browser)
;    :config
;    (require 'eaf-browser)
;    (require 'eaf-pdf-viewer)
;    (defalias 'browse-web #'eaf-open-browser)
;    ;(eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
;    (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
;    ;(eaf-bind-key take_photo "p" eaf-camera-keybinding)
;    (eaf-bind-key nil "M-q" eaf-browser-keybinding)
;
;    (require 'eaf-evil)
;    (define-key key-translation-map (kbd "SPC")
;        (lambda (prompt)
;        (if (derived-mode-p 'eaf-mode)
;            (pcase eaf--buffer-app-name
;                ("browser" (if  (string= (eaf-call-sync "call_function" eaf--buffer-id "is_focus") "True")
;                            (kbd "SPC")
;                            (kbd eaf-evil-leader-key)))
;                ("pdf-viewer" (kbd eaf-evil-leader-key))
;                ("image-viewer" (kbd eaf-evil-leader-key))
;                (_  (kbd "SPC")))
;            (kbd "SPC"))))
;) ;; unbind, see more in the Wiki
;#+END_SRC


(use-package company-quickhelp :ensure t :disabled
:unless (featurep 'lsp)
:general (:keymaps 'company-active-map "C-c h"  'company-quickhelp-manual-begin)
:custom (company-quickhelp-delay nil)
:config (company-quickhelp-mode)
)

(use-package company-prescient :ensure t :disabled
:after (prescient company)
)

(use-package company-dict :ensure t  :disabled
:after company
:custom (company-dict-dir (concat user-emacs-directory "dict/"))
        (company-dict-enable-yasnippet t)
        (company-dict-enable-fuzzy t)
:config (add-to-list 'company-backends 'company-dict)
        (define-key evil-insert-state-map (kbd "C-x C-k") 'company-dict)
        (setq company-dict-minor-mode-list t)
)

(use-package company-flx :ensure t :disabled
:after company
:config (company-flx-mode 1)
)

(use-package company-fuzzy :ensure t :disabled
:after company
:config (company-fuzzy-mode)
        (setq company-fuzzy-sorting-backend 'flx)
        ;(setq company-fuzzy-prefix-ontop t)
)

; deep learning completion
(use-package company-tabnine :ensure t :disabled
:after company
:preface
    (setq company-tabnine--disable-next-transform nil)
    (defun my-company--transform-candidates (func &rest args)
    (if (not company-tabnine--disable-next-transform)
        (apply func args)
        (setq company-tabnine--disable-next-transform nil)
        (car args)))

    (defun my-company-tabnine (func &rest args)
    (when (eq (car args) 'candidates)
        (setq company-tabnine--disable-next-transform t))
    (apply func args))

    (advice-add #'company--transform-candidates :around #'my-company--transform-candidates)
    (advice-add #'company-tabnine :around #'my-company-tabnine)
:init
    (setq +lsp-company-backend '(company-lsp :with company-tabnine :separate))
:config
    ;(add-to-list 'company-backends #'company-tabnine)
    (setq company-tabnine-annotations t)
    (setq company-tabnine-always-trigger nil)
)

(use-package flycheck-posframe :ensure t :after flycheck :disabled
:config (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode)
        (flycheck-posframe-configure-pretty-defaults)
)


(use-package quick-peek :ensure t  :after flycheck :disabled)

(use-package flycheck-inline :ensure t  :disabled
:if (not (featurep 'lsp))
:after (flycheck quick-peek)
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

(use-package paredit :ensure t  :disabled
:init
(add-hook 'emacs-lisp-mode-hook #'paredit-mode)
;; enable in the *scratch* buffer
(add-hook 'lisp-interaction-mode-hook #'paredit-mode)
(add-hook 'ielm-mode-hook #'paredit-mode)
(add-hook 'lisp-mode-hook #'paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)
(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode t)))
)

(use-package parinfer :ensure t  :disabled
:after (evil)
:general ("C-,"  'parinfer-toggle-mode)
:hook (emacs-lisp-mode common-lisp-mode lisp-mode)
:init 
;(add-hook 'emacs-lisp-mode-hook  #'parinfer-mode)
;(add-hook 'common-lisp-mode-hook #'parinfer-mode)
;(add-hook 'lisp-mode-hook        #'parinfer-mode)
;(add-hook 'clojure-mode-hook     #'parinfer-mode)
;(add-hook 'scheme-mode-hook      #'parinfer-mode)
:config
(setq parinfer-extensions '(defaults evil paredit pretty-parens)) ;lispy smart-tab smart-yank
)

;** Jekyll mode
;#+BEGIN_SRC elisp
;(use-package easy-jekyll :ensure t :disabled
;:commands easy-jekyll
;:config (setq easy-jekyll-basedir "~/dev/blog/")
;        (setq easy-jekyll-url "https://injae.github.io")
;        (setq easy-jekyll-sshdomain "blogdomain")
;        (setq easy-jekyll-root "/")
;        (setq easy-jekyll-previewtime "300")
;)
;#+END_SRC


(use-package pyvenv :ensure t 
:after  python-mode
:hook   (python-mode . pyvenv-mode)
;:init   (setenv "WORKON_HOME" "~/.pyenv/versions")
:config (pyvenv-tracking-mode)
)

(use-package pyenv-mode :ensure t :disabled
:after (python-mode projectile)
:hook (python-mode . pyenv-mode)
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
:config (add-hook 'projectile-switch-project-hook 'projectile-pyenv-mode-set)
)
(use-package pyenv-mode-auto :ensure t  :after pyenv-mode)

(use-package pip-requirements :ensure t  :disabled;:after python-mode
:hook (python-mode . pip-requirements-mode)
)

;** I3WM
; #+BEGIN_SRC elisp
;(use-package i3wm :ensure t  :defer t :disabled)
; #+END_SRC


(use-package js2-refactor :ensure t  :disabled
:after js2-mode
:hook (js2-mode . js2-refactor)
)

(use-package rjsx-mode :ensure t  :disabled
:after js2-mode
:mode (("\\.jsx$" . rjsx-mode)
       ("components/.+\\.js$" . rjsx-mode))
:hook (js2-mode . rjsx-mode)
:preface
(defun +javascript-jsx-file-p ()
    "Detect React or preact imports early in the file."
    (and buffer-file-name
         (string= (file-name-extension buffer-file-name) "js")
         (re-search-forward "\\(^\\s-*import +React\\|\\( from \\|require(\\)[\"']p?react\\)"
                            magic-mode-regexp-match-limit t)
         (progn (goto-char (match-beginning 1))
                (not (sp-point-in-string-or-comment)))))
:init (add-to-list 'magic-mode-alist '(+javascript-jsx-file-p . rjsx-mode))
)


(use-package lsp-intellij :disabled
:hook ((kotlin-mode . (lambda () (require 'lsp-intellij) (lsp-intellij-enable) (lsp)))
       (java-mode   . (lambda () (require 'lsp-intellij) (lsp-intellij-enable) (lsp))))
)
