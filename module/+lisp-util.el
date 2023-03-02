;;; +lisp-util.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package dash
    :config (dash-enable-font-lock))
(use-package f)
(use-package s)
(use-package dash-functional :after dash
    :functions dash-enable-font-lock
    :config (dash-enable-font-lock))

(provide '+lisp-util)
;;; +lisp-util.el ends here
