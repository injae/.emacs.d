;;; +formatting.el --- Summery -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package format-all :after exec-path-from-shell :disabled
    :hook ((prog-mode . format-all-mode)
              (format-all-mode . format-all-ensure-formatter))
                                        ;:custom (format-all-formatters
                                        ;            '(("Python"
                                        ;                  ("ruff" "check" ".")
                                        ;                  ("ruff" "format" "."))))
    )

(use-package apheleia :after exec-path-from-shell
    :config
    ;; (setf (alist-get 'isort apheleia-formatter)
    ;;     '("isort" "--stdout" "-"))
    ;; (setf (alist-get 'python-mode apheleia-mode-alist)
    ;;     '(isort black))
    (apheleia-global-mode +1)
    )


(use-package reformatter :after exec-path-from-shell :disabled
  ;;:hook
  ;;(python-ts-mode-hook . ruff-format-on-save-mode)
  ;;:config
  ;;(reformatter-define ruff-format
  ;;      :program "ruff"
  ;;      :args `("format" "--stdin-filename" ,buffer-file-name "-"))
)

(use-package ruff-format :after reformatter
    :hook (python-base-mode . ruff-format-on-save-mode)
    )


;;(use-package caser)

(provide '+formatting)
;;; +formatting.el ends here

