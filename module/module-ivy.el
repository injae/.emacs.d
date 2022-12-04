;;; module-ivy.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package ivy
;:after evil-collection
 ;ivy S-SPC remapping toogle-input-method
:general ("M-x" 'counsel-M-x )
         (:keymaps 'ivy-minibuffer-map
                        "S-SPC" 'toggle-input-method
                        "<f17>" 'toggle-input-method)
:custom (ivy-use-virtual-buffers      t)
        (ivy-use-selectable-prompt    t)
        (enable-recursive-minibuffers t)
        (ivy-height 20)
        (ivy-count-format "(%d/%d) ")
        (ivy-display-style 'fancy)
        (ivy-re-builders-alist '((counsel-M-x . ivy--regex-fuzzy) (t . ivy--regex-plus)))
        (ivy-format-function 'ivy-format-function-line)
:config
        (setq ivy-initial-inputs-alist nil)
        ;(setq search-default-mode #'char-fold-to-regexp)
        (ivy-mode 1)
)

;(use-package counsel
;:after ivy
;:config (counsel-mode)
;)

(use-package swiper  
:after ivy
:general ("C-s"    'swiper)
         ("C-S-s"  'swiper-all)
:config (setq swiper-action-recenter t)
        (setq swiper-goto-start-of-match t)
        (setq swiper-stay-on-quit t)
)

(use-package ivy-posframe  
:after ivy
:custom (ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center)))
        (ivy-posframe-parameters '((left-fringe . 8) (right-fringe . 8) (internal-border-width . 10)))
         ;ivy-posframe mutli frame focus bug fix
        ;(ivy-posframe-width 120)
:config ;(setq ivy-posframe-height-alist '((t . 20)))
        (add-function :after after-focus-change-function (lambda () (posframe-delete-all)))
        (setq ivy-posframe-height-fixed t)
        (setq ivy-posframe-widtivy-posframe-height-fixedh-fixed t)
        (ivy-posframe-mode t)
)

(use-package counsel-osx-app  
:after counsel
:general (leader "fa" '(counsel-osx-app :wk "Execute OSX App"))
)

(use-package counsel-fd   :disabled
:after counsel
:commands (counsel-fd-dired-jump counsel-fd-file-jump)
)


(use-package ivy-yasnippet  
:after (ivy yasnippet)
:general  ("C-c C-y" 'ivy-yasnippet)
;:config (advice-add #'ivy-yasnippet--preview :override #'ignore)
)

(use-package historian 
:after  (ivy)
:config (historian-mode)
)

(use-package ivy-historian  
:after  (ivy historian)
:config (ivy-historian-mode)
)

(use-package all-the-icons-ivy  
:config (all-the-icons-ivy-setup)
)

(use-package ivy-xref   :disabled
:after (ivy xref)
:config (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)
)

(use-package lsp-ivy  
:general (leader "hs" '(lsp-ivy-workspace-symbol :wk "Search Symbol")
                 "hS" '(lsp-ivy-global-workspace-symbol :wk "Search Global Symbol"))
)

(use-package counsel-projectile  
:after  (counsel projectile)
:custom (projectile-completion-system 'ivy)
        (counsel-find-file-ignore-regexp ".ccls-cache/")
:general (leader "fp" '(counsel-projectile-find-file-dwim   :wk "Search in Project")
                 "fG" '(counsel-projectile-rg               :wk "Grep in Project")
                 "bS" '(counsel-projectile-switch-to-buffer :wk "Search Buffer in Project"))

:config (counsel-projectile-mode 1)

)
(use-package counsel-world-clock
:after (counsel)
:general (:keymaps 'counsel-mode-map "C-c c k"  'counsel-world-clock)
)

(use-package counsel-tramp
:after counsel
:commands counsel-tramp
:general ("C-c s" 'counsel-tramp)
:init (setq tramp-default-method "ssh")
)

(use-package counsel-org-clock   :after (counsel org))

(use-package all-the-icons-ivy-rich  
:config
    (setq ivy-rich-parse-remote-buffer nil)
    (all-the-icons-ivy-rich-mode t)
)

(use-package ivy-rich  
    :init (setq ivy-rich-path-style    'abbrev)
          (setq ivy-virtual-abbreviate 'full)
    :config
    ;(setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
    (ivy-rich-mode 1)
)

(provide 'module-ivy)
;;; module-ivy.el ends here
