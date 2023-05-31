;;; +completion.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package which-key
:functions which-key-mode
:custom  (which-key-allow-evil-operators t)
         (which-key-show-operator-state-maps t)
 :config (which-key-mode)
)

(use-package which-key-posframe :disabled
:after which-key
:config
    (setq which-key-posframe-border-width 15)
    (setq which-key-posframe-poshandler 'posframe-poshandler-window-top-center)
    (which-key-posframe-mode)
)

;;; minibuffer
(use-package vertico
    :elpaca (vertico :files (:defaults "extensions/*")
                       :includes (
                          vertico-indexed
                          vertico-mouse
                          vertico-quick
                          vertico-directory
                          vertico-repeat
                          vertico-buffer
                          vertico-multiform
                          vertico-reverse
                          vertico-flat
                          vertico-grid
                          vertico-unobtrusive))
    :general (:keymaps 'vertico-map
             :state 'insert
             "<escape>" #'evil-normal-state)
    :custom
    ;; Different scroll margin
    ;; (vertico-scroll-margin 0)
    (vertico-count 20)
    (vertico-resize t)
    ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
    (vertico-cycle t)
    :config
    (vertico-mode)
)

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist :elpaca nil
    :init (savehist-mode))

(use-package emacs :elpaca nil
    :defines crm-separator
    :preface
    (defun crm-indicator (args)
        (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" "" crm-separator)
                  (car args))
            (cdr args)))
    :init
    ;; vertico
    ;; Add prompt indicator to `completing-read-multiple'.
    ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
    (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

    ;; Do not allow the cursor in the minibuffer prompt
    (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
    (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

    (setq enable-recursive-minibuffers t)
    )


(use-package vertico-posframe
    :functions vertico-posframe-mode
    :hook (emacs-startup . vertico-posframe-mode)
    :custom (vertico-posframe-poshandler #'posframe-poshandler-frame-top-center)
            (vertico-posframe-parameters '((left-fringe . 8) (right-fringe . 8)))
)


(use-package consult :after (projectile evil-collection)
    :general (leader
                 "fp" '(consult-projectile-find-file   :wk "Search in Project")
                 "fo" '(find-file-other-window         :wk "Open Other Window")
                 "fG" '(consult-ripgrep                :wk "Grep in Project")
                 "bs" '(consult-buffer                 :wk "Search Buffer")
                 "bS" '(consult-project-switch         :wk "Search Buffer in Project")
                 )
    ;; Replace bindings. Lazily loaded due by `use-package'.
    :bind (;; C-c bindings (mode-specific-map)
            ("C-c M-x" . consult-mode-command)
            ("C-c h" . consult-history)
            ("C-c k" . consult-kmacro)
            ("C-c m" . consult-man)
            ("C-c i" . consult-info)
            ([remap Info-search] . consult-info)
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
            ;; M-g bindings (goto-map)
            ("M-g e" . consult-compile-error)
            ("M-g f" . consult-flycheck)               ;; Alternative: consult-flycheck
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
            ("C-s" . consult-line)
            ("M-s L" . consult-line-multi)
            ("M-s k" . consult-keep-lines)
            ("M-s u" . consult-focus-lines)
            ;; Isearch integration
            ("M-s e" . consult-isearch-history)
            :map isearch-mode-map
            ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
            ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
            ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
            ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
            ;; Minibuffer history
            :map minibuffer-local-map
            ("M-s" . consult-history)                 ;; orig. next-matching-history-element
            ("M-r" . consult-history))                ;; orig. previous-matching-history-element
    :hook (completion-list-mode . consult-preview-at-point-mode)
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
        consult-theme :preview-key '(:debounce 0.2 any)
        consult-ripgrep consult-git-grep consult-grep
        consult-bookmark consult-recent-file consult-xref
        consult--source-bookmark consult--source-file-register
        consult--source-recent-file consult--source-project-recent-file
        :preview-key '(:debounce 0.4 any))

    (autoload 'projectile-project-root "projectile")
    (setq consult-project-function (lambda (_) (projectile-project-root)))
    )

(use-package consult-projectile)
(use-package consult-flycheck)

(use-package marginalia
    :functions marginalia-mode
    :config (marginalia-mode))

(use-package embark
    :functions embark-prefix-help-command
    :bind (("C-." . embark-act)         ;; pick some comfortable binding
           ("C-;" . embark-dwim)        ;; good alternative: M-.
           ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
    :config
    ;; Optionally replace the key help with a completing-read interface
    (setq prefix-help-command #'embark-prefix-help-command)
    ;; Hide the mode line of the Embark live/completions buffers
    (add-to-list 'display-buffer-alist '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*" nil (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
    :after (embark consult)
    :demand t ; only necessary if you have the hook below
    :hook (embark-collect-mode . consult-preview-at-point-mode)
)

;;; input
(use-package corfu
    :elpaca (corfu :files (:defaults "extensions/*")
                  :includes (
                    corfu-history
                    corfu-popupinfo))
    :after evil-collection
    :general
    (:keymaps 'corfu-map
        :states 'insert
        "C-n" #'corfu-next
        "C-p" #'corfu-previous
        "<escape>" #'evil-collection-corfu-quit-and-escape
        "C-<return>" #'corfu-insert
        "M-d" #'corfu-show-documentation
        "M-l" #'corfu-show-location)
    :bind
    (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
    :custom
    (corfu-auto t)
    (corfu-auto-prefix 2)
    (corfu-auto-delay 0.25)
    (corfu-count 14)
    (corfu-scroll-margin 4)
    (corfu-quit-no-match t)
    (corfu-preselect-first t)
    (corfu-cycle t)
    (corfu-preselect 'prompt)
    :hook ((emacs-startup . global-corfu-mode)
           (corfu-mode    . corfu-history-mode)
           (corfu-mode    . corfu-popupinfo-mode))
)

(use-package kind-icon :after corfu
    :defines corfu-margin-formatters
    :functions kind-icon-margin-formatter
    :custom (kind-icon-default-face 'corfu-default)
    :config (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Completion
(use-package orderless
    :custom (completion-styles '(orderless partial-completion basic)))


(use-package affe :after (consult orderless)
    :preface
    (defun affe-orderless-regexp-compiler (input _type _ignorecase)
        (setq input (orderless-pattern-compiler input))
        (cons input (lambda (str) (orderless--highlight input str))))
    :config (setq affe-regexp-compiler #'affe-orderless-regexp-compiler)
            (consult-customize affe-grep affe-find :preview-key 'any)
    )


;; Add extensions
(use-package cape :after evil
    ;; Bind dedicated completion commands
    ;; Alternative prefix keys: C-c p, M-p, M-+, ...
    :bind (("C-c p p" . completion-at-point) ;; capf
           ("C-c p t" . complete-tag)        ;; etags
           ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
           ("C-c p h" . cape-history)
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
    (add-to-list 'completion-at-point-functions #'cape-history)
    :config
    (general-add-advice '(corfu--setup corfu--teardown) :after 'evil-normalize-keymaps)
    (evil-make-overriding-map corfu-map)
)


(use-package cape-yasnippet :elpaca (:host github :repo "elken/cape-yasnippet")
    :after (cape)
    :config (add-to-list 'completion-at-point-functions #'cape-yasnippet)
    )

(provide '+completion)
;;; +completion.el ends here
