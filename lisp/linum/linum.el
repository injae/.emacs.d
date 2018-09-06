;;; linum.el --- initialization emacs
;;; Commentry:
;;; linum setting 
;;; Code:

(add-to-list 'load-path "~/.emacs.d/lisp/linum/linum-highlight-current-line-number/")
(require 'linum-highlight-current-line-number)
(setq linum-format 'linum-highlight-current-line-number)
(global-linum-mode t)
