;;; module-lisp-util.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'use-package)
(require 'straight)

(use-package dash :straight t :init (global-dash-fontify-mode t))
(use-package dash-functional :straight t :after dash)
(use-package f :straight t)
(use-package s :straight t)
(use-package srefactor :straight t
    :config (require 'srefactor-lisp))
 
(defun load-modules-with-list (module-path modules)
    (require 'use-package)
    (require 'straight)
    (setq-local target
        (-map
            (lambda (module)
                (f-long
                 (f-join module-path (s-concat "module-" module))))
            modules
            )
        )
    (dolist
        (it target)
        (native-compile-async (f-swap-ext it "el"))
        (require (intern (f-filename it)))
        ))
;;;

(provide 'module-lisp-util)
;;; module-lisp-util.el ends here
