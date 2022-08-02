

(use-package emacs-lisp :no-require t :after general
:general (leader "le" '(eval-print-last-sexp :wk "Elisp Evaluate"))
)

(use-package scratch-comment :straight t 
:general (:keymaps 'lisp-interaction-mode-map "C-j" 'scratch-comment-eval-sexp)
)

(use-package slime :straight t  :disabled
:commands slime
:config
    (setq inferior-lisp-program (or (executable-find "sbcl")
                                    (executable-find "/usr/bin/sbcl")
                                    (executable-find "/usr/sbin/sbcl" )))
    (require 'slime-autoloads)
    (slime-setup '(slime-fancy))
)
(use-package elisp-slime-nav :straight t  :diminish elisp-slime-nav-mode
:hook ((emacs-lisp-mode ielm-mode) . elisp-slime-nav-mode)
)

(use-package prettify-symbols :no-require t
:hook ((emacs-lisp-mode lisp-mode org-mode) . prettify-symbols-mode)
)
