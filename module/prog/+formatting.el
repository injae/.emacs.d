;;; +formatting.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package format-all
    :hook ((prog-mode . format-all-mode)
           (format-all-mode . format-all-ensure-formatter))
    )

(use-package apheleia :after exec-path-from-shell :disabled
    :config
    (setf (alist-get 'isort apheleia-formatter)
        '("isort" "--stdout" "-"))
    (setf (alist-get 'python-mode apheleia-mode-alist)
        '(isort black))
    (apheleia-global-mode +1)
    )

;;(use-package caser)

(provide '+formatting)
;;; +formatting.el ends here

