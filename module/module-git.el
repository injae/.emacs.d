;;; module-git.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package magit :straight t
:commands magit-status
:general (leader "gs" 'magit-status)
:config (setq vc-handled-backends nil)
        ;(setq auth-source '("~/.authinfo"))
)

(use-package forge :straight t  :after magit
    :config
    ;(defclass forge-gitlab-http-repository (forge-gitlab-repository)
    ;    ((issues-url-format         :initform "http://%h/%o/%n/issues")
    ;     (issue-url-format          :initform "http://%h/%o/%n/issues/%i")
    ;     (issue-post-url-format     :initform "http://%h/%o/%n/issues/%i#note_%I")
    ;     (pullreqs-url-format       :initform "http://%h/%o/%n/merge_requests")
    ;     (pullreq-url-format        :initform "http://%h/%o/%n/merge_requests/%i")
    ;     (pullreq-post-url-format   :initform "http://%h/%o/%n/merge_requests/%i#note_%I")
    ;     (commit-url-format         :initform "http://%h/%o/%n/commit/%r")
    ;     (branch-url-format         :initform "http://%h/%o/%n/commits/%r")
    ;     (remote-url-format         :initform "http://%h/%o/%n")
    ;     (create-issue-url-format   :initform "http://%h/%o/%n/issues/new")
    ;     (create-pullreq-url-format :initform "http://%h/%o/%n/merge_requests/new")
    ;     (pullreq-refspec :initform "+refs/merge-requests/*/head:refs/pullreqs/*")))
    ;(add-to-list 'ghub-insecure-hosts "git.private.network.repo/api/v4")
)

(use-package git-messenger :straight t
:commands git-messenger:popup-message
:general (leader "gm" 'git-messenger:popup-message)
:config (setq git-messenger:use-magit-popup t)
)


; 현재 git repo의 homepage link를 clipboard에 넣어준다
(use-package git-link :straight t
:general (leader "gh" 'git-link-homepage)
:config  ;(setq git-link-use-single-line-number t)
         (setf git-link-use-commit t)
)

;; git history view mode
(use-package smeargle :straight t 
:commands smeagle
)

(use-package blamer :straight t :defer t
:custom
    (blamer-view 'overlay)
    (blamer-idle-time 0.3)
    (blamer-min-offset 70)
    (blamer-force-truncate-long-line t)
:custom-face
    (blamer-face ((t :foreground "#7a88cf"
                     :background nil
                     :height 1.0
                     :italic t)))
)

(use-package evil-ediff :straight t 
:after evil
:config (evil-ediff-init)
)

(provide 'module-git)
;;; module-git.el ends here
