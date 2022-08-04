;;; navigater
;;; Code:

(use-package which-key :straight t
:init   (which-key-mode t)
:config (setq which-key-allow-evil-operators t)
        (setq which-key-show-operator-state-maps t)
)

(use-package which-key-posframe :straight t  :disabled
:after which-key
:config
    (setq which-key-posframe-border-width 15)
    (setq which-key-posframe-poshandler 'posframe-poshandler-window-top-center)
    (which-key-posframe-mode)
)

;;; minibuffer
(use-package vertico :straight t
  :config (vertico-mode)
  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)
  (setq vertico-count 20)
  (setq vertico-resize t)
  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

(use-package vertico-posframe :straight t
:config
    (setq vertico-posframe-poshandler #'posframe-poshandler-frame-top-center)
	(vertico-posframe-mode t)
)

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist :straight t :init (savehist-mode))

(use-package consult :straight t :after (projectile evil-collection)
    :general (leader "fp" '(consult-projectile-find-file   :wk "Search in Project")
                     "fG" '(consult-ripgrep                :wk "Grep in Project")
                     "bS" '(consult-project-switch         :wk "Search Buffer in Project"))
    :bind (;; C-c bindings (mode-specific-map)
            ("C-c h" . consult-history)
            ("C-c m" . consult-mode-command)
            ("C-c k" . consult-kmacro)
            ;; C-x bindings (ctl-x-map)
            ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
            ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
            ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window

            ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
            ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
            ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
            ;; Custom M-# bindings for fast register access
            ("M-#" . consult-register-load)
            ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
            ("C-M-#" . consult-register)
            ;; Other custom bindings
            ("M-y" . consult-yank-pop)                ;; orig. yank-pop
            ("<help> a" . consult-apropos)            ;; orig. apropos-command
            ;; M-g bindings (goto-map)
            ("M-g e" . consult-compile-error)
            ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
            ("M-g g" . consult-goto-line)             ;; orig. goto-line
            ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
            ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
            ("M-g m" . consult-mark)
            ("M-g k" . consult-global-mark)
            ("M-g i" . consult-imenu)

            ("M-g I" . consult-imenu-multi)
            ;; M-s bindings (search-map)
            ("M-s d" . consult-find)
            ("M-s D" . consult-locate)
            ("M-s g" . consult-grep)
            ("M-s G" . consult-git-grep)
            ("M-s r" . consult-ripgrep)
            ("M-s l" . consult-line)
            ("C-s"   . consult-line)
            ("M-s L" . consult-line-multi)
            ("M-s m" . consult-multi-occur)
            ("M-s k" . consult-keep-lines)
            ("M-s u" . consult-focus-lines)
            ;; Isearch integration
            ("M-s e" . consult-isearch-history)
            :map isearch-mode-map
            ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
            ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
            ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
            ("M-s L" . consult-line-multi)
            ;; needed by consult-line to detect isearch
            ;; Minibuffer history
            :map minibuffer-local-map
            ("M-s" . consult-history)                 ;; orig. next-matching-history-element
            ("M-r" . consult-history))                ;; orig. previous-matching-history-element

    :init
    (setq register-preview-delay 0.5
          register-preview-function #'consult-register-format)

    ;; Optionally tweak the register preview window.
    ;; This adds thin lines, sorting and hides the mode line of the window.
    (advice-add #'register-preview :override #'consult-register-window)

    ;; Use Consult to select xref locations with preview
    (setq xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref)

    ;; Configure other variables and modes in the :config section,
    ;; after lazily loading the package.
    :config
    (setq consult-preview-key 'any)
    ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
    ;; For some commands and buffer sources it is useful to configure the
    ;; :preview-key on a per-command basis using the `consult-customize' macro.
    (consult-customize
        consult-theme
        :preview-key '(:debounce 0.2 any)
        consult-ripgrep consult-git-grep consult-grep
        consult-bookmark consult-recent-file consult-xref
        consult--source-bookmark consult--source-recent-file
        consult--source-project-recent-file
        :preview-key (kbd "M-."))

    ;; Optionally configure the narrowing key.
    ;; Both < and C-+ work reasonably well.

    ;;(setq consult-narrow-key "<") ;; (kbd "C-+")

    ;; Optionally make narrowing help available in the minibuffer.
    ;; You may want to use `embark-prefix-help-command' or which-key instead.
    ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)
    (autoload 'projectile-project-root "projectile")
    (setq consult-project-function (lambda (_) (projectile-project-root)))
    :config (evil-collection-consult-setup)
    )

(use-package consult-projectile :straight t)

(use-package marginalia :straight t :config (marginalia-mode))

(use-package embark :straight t
  :bind
    (("C-." . embark-act)         ;; pick some comfortable binding
     ("C-;" . embark-dwim)        ;; good alternative: M-.
     ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*" nil (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult :straight t :after (embark consult) :demand t ; only necessary if you have the hook below
  :hook (embark-collect-mode . consult-preview-at-point-mode)
)

;;; input
(use-package corfu :straight t :after evil-collection
:general (:keymaps 'corfu-map
		       :states 'insert
		       "C-n" #'corfu-next
		       "C-p" #'corfu-previous
		       "<escape>" #'evil-collection-corfu-quit-and-escape
		       "C-<return>" #'corfu-insert
		       "M-d" #'corfu-show-documentation
		       "M-l" #'corfu-show-location)
:bind (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))     
:custom
    (corfu-auto t)
    (corfu-auto-prefix 1)
    (corfu-quit-no-match t)
    (corfu-preselect-first nil)
    (corfu-max-witdh corfu-min-width)
:init (global-corfu-mode)
)

(use-package corfu-doc :straight t :after corfu
:config (add-hook 'corfu-mode-hook #'corfu-doc-mode)
)

;; Add extensions
(use-package cape :straight t
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;(add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-history)
  ;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;(add-to-list 'completion-at-point-functions #'cape-line)
  :config
    (general-add-advice '(corfu--setup corfu--teardown) :after 'evil-normalize-keymaps)
    (evil-make-overriding-map corfu-map)
    ;; Enable Corfu more generally for every minibuffer, as long as no other
    ;; completion UI is active. If you use Mct or Vertico as your main minibuffer
    ;; completion UI. From
    ;; https://github.com/minad/corfu#completing-with-corfu-in-the-minibuffer
    (defun corfu-enable-always-in-minibuffer ()
        "Enable Corfu in the minibuffer if Vertico/Mct are not active."
        (unless (or (bound-and-true-p mct--active) ; Useful if I ever use MCT
                    (bound-and-true-p vertico--input))
        (setq-local corfu-auto nil)       ; Ensure auto completion is disabled
        (corfu-mode 1)))
    (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)
    ;; Setup lsp to use corfu for lsp completion
)

(use-package kind-icon :straight t :after corfu
    :custom (kind-icon-default-face 'corfu-default)
    :config (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Completion
(use-package orderless :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))


;;;
