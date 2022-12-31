;;; module-git.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'use-package)

(use-package compat)

(use-package git-commit)

(use-package magit
:commands magit-status
:general (leader "gs" 'magit-status)
:config (setq vc-handled-backends nil)
        ;(setq auth-source '("~/.authinfo"))
)

(use-package forge :after magit
    :init (setq forge-add-default-bindings nil)
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

(use-package git-messenger
:commands git-messenger:popup-message
:general (leader "gm" 'git-messenger:popup-message)
:config (setq git-messenger:use-magit-popup t)
)


; 현재 git repo의 homepage link를 clipboard에 넣어준다
(use-package git-link
:general (leader "gh" 'git-link-homepage)
:config  ;(setq git-link-use-single-line-number t)
         (setf git-link-use-commit t)
)

;; git history view mode
(use-package smeargle
:commands smeargle
)

(use-package blamer :disabled
:bind (("s-i" . blamer-show-commit-info))
:custom
    (blamer-view 'overlay)
    (blamer-idle-time 0.3)
    (blamer-min-offset 70)
    (blamer-force-truncate-long-line t)
:custom-face
    (blamer-face ((t :foreground "#7a88cf" :background nil :height 1.0 :italic t)))
)

(use-package evil-ediff :after evil
:config (evil-ediff-init)
)

;(use-package magit-delta :after magit
;    :ensure-system-package (delta . "brew install git-delta")
;    :hook (magit-mode . magit-delta-mode)
;    ;:custom (magit-delta-delta-args "--true-color always")
;    )

(use-package magit-difftastic :straight nil :no-require t :after magit :disabled
:ensure-system-package (difft . "cargo install difftastic")
:config
    (require 'magit)
    (defun th/magit--with-difftastic (buffer command)
        "Run COMMAND with GIT_EXTERNAL_DIFF=difft then show result in BUFFER."
        (let ((process-environment
                (cons (concat "GIT_EXTERNAL_DIFF=difft --width="
                            (number-to-string (frame-width)))
                    process-environment)))
            ;; Clear the result buffer (we might regenerate a diff, e.g., for
            ;; the current changes in our working directory).
            (with-current-buffer buffer
                (setq buffer-read-only nil)
                (erase-buffer))
            ;; Now spawn a process calling the git COMMAND.
            (make-process
            :name (buffer-name buffer)
            :buffer buffer
            :command command
            ;; Don't query for running processes when emacs is quit.
            :noquery t
            ;; Show the result buffer once the process has finished.
            :sentinel (lambda (proc event)
                        (when (eq (process-status proc) 'exit)
                            (with-current-buffer (process-buffer proc)
                                (goto-char (point-min))
                                (ansi-color-apply-on-region (point-min) (point-max))
                                (setq buffer-read-only t)
                                (view-mode)
                                (end-of-line)
                                ;; difftastic diffs are usually 2-column side-by-side,
                                ;; so ensure our window is wide enough.
                                (let ((width (current-column)))
                                    (while (zerop (forward-line 1))
                                        (end-of-line)
                                        (setq width (max (current-column) width)))
                                    ;; Add column size of fringes
                                    (setq width (+ width
                                                    (fringe-columns 'left)
                                                    (fringe-columns 'right)))
                                    (goto-char (point-min))
                                    (pop-to-buffer
                                        (current-buffer)
                                        `(;; If the buffer is that wide that splitting the frame in
                                          ;; two side-by-side windows would result in less than
                                          ;; 80 columns left, ensure it's shown at the bottom.
                                            ,(when (> 80 (- (frame-width) width)) #'display-buffer-at-bottom)
                                            (window-width . ,(min width (frame-width))))))))))))
    (defun th/magit-show-with-difftastic (rev)
        "Show the result of \"git show REV\" with GIT_EXTERNAL_DIFF=difft."
        (interactive
        (list (or
                ;; If REV is given, just use it.
                (when (boundp 'rev) rev)
                ;; If not invoked with prefix arg, try to guess the REV from
                ;; point's position.
                (and (not current-prefix-arg)
                    (or (magit-thing-at-point 'git-revision t)
                        (magit-branch-or-commit-at-point)))
                ;; Otherwise, query the user.
                (magit-read-branch-or-commit "Revision"))))
        (if (not rev)
            (error "No revision specified")
            (th/magit--with-difftastic
            (get-buffer-create (concat "*git show difftastic " rev "*"))
            (list "git" "--no-pager" "show" "--ext-diff" rev))))

    (defun th/magit-diff-with-difftastic (arg)
        "Show the result of \"git diff ARG\" with GIT_EXTERNAL_DIFF=difft."
        (interactive
        (list (or
                ;; If RANGE is given, just use it.
                (when (boundp 'range) range)
                ;; If prefix arg is given, query the user.
                (and current-prefix-arg
                    (magit-diff-read-range-or-commit "Range"))
                ;; Otherwise, auto-guess based on position of point, e.g., based on
                ;; if we are in the Staged or Unstaged section.
                (pcase (magit-diff--dwim)
                    ('unmerged (error "unmerged is not yet implemented"))
                    ('unstaged nil)
                    ('staged "--cached")
                    (`(stash . ,value) (error "stash is not yet implemented"))
                    (`(commit . ,value) (format "%s^..%s" value value))
                    ((and range (pred stringp)) range)
                    (_ (magit-diff-read-range-or-commit "Range/Commit"))))))
        (let ((name (concat "*git diff difftastic"
                            (if arg (concat " " arg) "")
                            "*")))
            (th/magit--with-difftastic
            (get-buffer-create name)
            `("git" "--no-pager" "diff" "--ext-diff" ,@(when arg (list arg))))))

    (transient-define-prefix th/magit-aux-commands ()
        "My personal auxiliary magit commands."
        ["Auxiliary commands"
        ("d" "Difftastic Diff (dwim)" th/magit-diff-with-difftastic)
        ("s" "Difftastic Show" th/magit-show-with-difftastic)])
    (transient-append-suffix 'magit-dispatch "!"
        '("#" "my Magit CMD" th/magit-aux-commands))
    (define-key magit-status-mode-map (kbd "#") #'th/magit-aux-commands)
)

(provide 'module-git)
;;; module-git.el ends here
