;;; module-coverage.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package magit
:commands magit-status
:general (leader "gs" 'magit-status)
:config (setq vc-handled-backends nil)
        ;(setq auth-source '("~/.authinfo"))
)

(use-package cov )
;(use-package coverlay  :disabled)

(provide 'module-coverage)
;;; module-coverage.el ends here
