;;; module-formatting.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package format-all
    :hook (prog-mode . format-all-mode)
           ;(format-all-mode . format-all-ensure-formatter)
    )

(provide 'module-formatting)
;;; module-formatting.el ends here
