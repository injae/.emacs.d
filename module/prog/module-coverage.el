;;; module-coverage.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package magit :straight t
:commands magit-status
:general (leader "gs" 'magit-status)
:config (setq vc-handled-backends nil)
        ;(setq auth-source '("~/.authinfo"))
)

(use-package cov :straight t)
;(use-package coverlay :straight t :disabled)

(provide 'module-coverage)
;;; module-coverage.el ends here
