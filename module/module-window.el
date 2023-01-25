;;; module-window.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; window manage
(use-package ace-window
:commands (ace-window)
:general (leader "wo" 'ace-window
                 "wd" 'delete-other-windows)
         ;("C-w C-o" 'ace-window)
:config (setq aw-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8))
)

(use-package eyebrowse  :defer t
:init (eyebrowse-mode t)
:general (leader
	   "w;" 'eyebrowse-last-window-config
	   "w0" 'eyebrowse-close-window-config
	   "w1" 'eyebrowse-switch-to-window-config-1
	   "w2" 'eyebrowse-switch-to-window-config-2
	   "w3" 'eyebrowse-switch-to-window-config-3
	   "w4" 'eyebrowse-switch-to-window-config-4
	   "w5" 'eyebrowse-switch-to-window-config-5
	   "w6" 'eyebrowse-switch-to-window-config-6
	   "w7" 'eyebrowse-switch-to-window-config-7)
)

(provide 'module-window)
;;; module-window.el ends here
