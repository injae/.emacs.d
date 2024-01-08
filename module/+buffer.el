;;; +buffer.el --- Summery -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; buffer manage
(use-package buffer-move
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
(use-package command-log-mode)

(use-package buffer-zoom :elpaca nil
:general (leader "tu" 'text-scale-increase
                 "td" 'text-scale-decrease)
)

;; buffer diff
(use-package vdiff)

(use-package ztree)

(use-package popwin :disabled
    :config (popwin-mode 1)
    )

(use-package burly :disabled)

(provide '+buffer)
;;; +buffer.el ends here
