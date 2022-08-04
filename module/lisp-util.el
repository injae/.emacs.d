;;; package  --- Summary
;;; Commentary:
;;; Code:

(use-package dash :straight t :init (global-dash-fontify-mode t))
(use-package dash-functional :straight t :after dash)
(use-package f :straight t)
(use-package s :straight t)
(use-package srefactor :straight t
    :config (require 'srefactor-lisp))

(defun load-all-modules-with-filter (module-path ignore-modules)
    (setq-local ignore
        (-map
            (lambda (module)
                (f-long 
                    (f-join module-path
                        (f-swap-ext module "el"))))
            ignore-modules)
        )
    (setq-local modules (f-entries module-path))

    (dolist (it (-filter (lambda (module) (not (-contains? ignore-modules module))) modules))
        (load-file it)
        )
    )
 
(defun load-modules-with-list (module-path modules)
    (setq-local target
        (-map
            (lambda (module)
                (f-long 
                    (f-join module-path
                        (f-swap-ext module "el"))))
            modules)
        )
    (dolist (it target) (load-file it)))

;;;
