;;; +formatting.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package format-all :disabled
    :hook (prog-mode . format-all-mode)
          (format-all-mode . format-all-ensure-formatter)
    )

(use-package apheleia :after exec-path-from-shell
    :config
    (setf (alist-get 'isort apheleia-formatter)
              '("isort" "--stdout" "-"))
    (setf (alist-get 'python-mode apheleia-mode-alist)
        '(isort black))
    (apheleia-global-mode +1)
    )




(provide '+formatting)
;;; +formatting.el ends here
