;;; +git.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package magit
    :commands magit-status
    :general (leader "gs" 'magit-status)
    :config (setq vc-handled-backends nil)
    ;; (setq auth-source '("~/.authinfo"))
)

(use-package forge :after magit
    :custom (forge-add-default-bindings nil)
)

(use-package git-messenger
:commands git-messenger:popup-message
:general (leader "gm" 'git-messenger:popup-message)
:config (setq git-messenger:use-magit-popup t)
)


;; 현재 git repo의 homepage link를 clipboard에 넣어준다
(use-package git-link
:general (leader "gh" 'git-link-homepage)
:config  ;(setq git-link-use-single-line-number t)
         (setf git-link-use-commit t)
)

;; git history view mode
(use-package smeargle :disabled
:commands smeargle
)

(use-package blamer
:bind (("s-i" . blamer-show-posframe-commit-info))
:general (leader "gb" 'blamer-show-posframe-commit-info)
:custom-face
    (blamer-face ((t :foreground "#7a88cf" :height 1.0 :italic t)))
:custom
    ;(blamer-view 'overlay)
    (blamer-idle-time 0.3)
    (blamer-min-offset 70)
    (blamer-force-truncate-long-line t)
;:hook (emacs-startup . global-blamer-mode)
)

(use-package difftastic
    :config
    (eval-after-load 'magit-diff
        '(transient-append-suffix 'magit-diff '(-1 -1)
        [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
            ("S" "Difftastic show" difftastic-magit-show)]))
    )

(use-package git-timemachine)

(provide '+git)
;;; +git.el ends here
