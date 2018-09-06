;;; tab.el --- initialization emacs
;;; Commentry:
;;; tab and shift tab can use script 
;;; Code:

;(use-package highlight-indent-guides
;    :ensure t
;    :init (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
;    :config 
;       (setq highlight-indent-guides-method 'character)
;       ;(set-face-background 'highlight-indent-guides-odd-face       "darkgray")
;       ;(set-face-background 'highlight-indent-guides-even-face      "dimgray" )
;       ;(set-face-background 'highlight-indent-guides-character-face "dimgray" )
;)

(use-package indent-guide
    :ensure t
    :init
       (indent-guide-global-mode)
    :config
       ;(set-face-background 'indent-guide-face "dimgray")
       ;(setq indent-guide-delay     0.1)
       (setq indent-guide-char      "|")
       (setq indent-guide-recursive t)
)

(setq-default indent-tabs-mode nil)
;(use-package python :ensure t
;    :init 
;        (define-key python-mode-map (kbd "<tab>")     'python-indent-shift-right)
;        (define-key python-mode-map (kbd "<backtab>") 'python-indent-shift-left)
;)

;(defun my-set-indent (n)
;    (setq-default tab-width n)
;    (electric-indent-mode -1)
;    (setq c-basic-offset n)
;    (setq lisp-indent-offset n)
;    (setq indent-line-function 'insert-tab)
;)
;(my-set-indent 4)
;
;(global-set-key (kbd "<backtab>") 'un-indent-by-removing-4-spaces)
;(defun un-indent-by-removing-4-spaces ()
;    (interactive)
;    (save-excursion
;    (save-match-data
;    (beginning-of-line)
;        ;; get rid of tabs at beginning of line
;    (when (looking-at "^\\s-+")
;    (untabify (match-beginning 0) (match-end 0)))
;        (when (looking-at "^    ")
;            (replace-match "")))
;        )
;)
