;;; module-lisp-util.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'use-package)

(use-package dash :init (global-dash-fontify-mode t))
(use-package dash-functional :after dash)
(use-package f)
(use-package s)

(use-package srefactor
    :config (require 'srefactor-lisp))
 
(defun load-modules-with-list (module-path modules)
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
        ;(native-compile-async (f-swap-ext it "el"))
        (require (intern (f-filename it)))
        ))

(defun temp-buffer ()
    "open up a guaranteed new scratch buffer"
    (interactive)
    (switch-to-buffer (cl-loop for num from 0
                            for name = (format "temp-%03i" num)
                            while (get-buffer name)
                            finally return name)))
;;;

(provide 'module-lisp-util)
;;; module-lisp-util.el ends here
