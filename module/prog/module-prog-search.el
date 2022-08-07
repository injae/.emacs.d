;;; module-prog-search.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package dumb-jump :straight t 
:after  company
:custom ;(dumb-jump-selector 'ivy)
        (dumb-jump-force-searcher 'rg)
        (dumb-jump-default-project "~/build")
:general (leader "hd"  '(:wk "Definition Jump")
                 "hdo" 'find-file-other-window
                 "hdj" 'xref-pop-marker-stack)
:init (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
)


(provide 'module-prog-search)
;;; module-prog-search.el ends here
