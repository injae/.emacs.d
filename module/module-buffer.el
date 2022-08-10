;;; module-buffer.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'use-pacakge)
(require 'straight)

;; buffer manage
(use-package buffer-move :straight t
:general (leader
	   "b c" #'clean-buffer-list
       "b r" 'eval-buffer
	   "b h" 'buf-move-left
	   "b j" 'buf-move-down
	   "b k" 'buf-move-up
	   "b l" 'buf-move-right
	   "b m" 'switch-to-buffer
	   "b n" 'next-buffer
	   "b p" 'previous-buffer
)
:init  (global-set-key (kbd "C-x C-b") 'switch-to-buffer)
)

;; buffer log
(use-package command-log-mode :straight t)

(use-package buffer-zoom :straight nil
:general (leader "tu" 'text-scale-increase
                 "td" 'text-scale-decrease)
)

(provide 'module-buffer)
;;; module-buffer.el ends here