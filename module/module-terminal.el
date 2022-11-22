;;; module-terminal.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'use-package)

(use-package vterm :after (evil-collection exec-path-from-shell)
;:commands (vterm)
;:ensure-system-package ((zsh . "chsh -s $(which zsh)")
                        ;(zinit . "sh -c \"$(curl -fsSL https://git.io/zinit-install)\""))
;:init   (setq vterm-always-compile-module t)
:config
    (define-key vterm-mode-map (kbd "C-c C-c") 'vterm-send-C-c)
    (define-key vterm-mode-map (kbd "<C-return>") 'vterm-send-right)
    (add-hook 'vterm-mode-hook (lambda () (display-line-numbers-mode -1)))
)


(use-package multi-vterm
:general (leader "tn" 'multi-vterm :wk "new terminal")
)

(use-package vterm-with-centaur-tab :no-require t :straight nil
:after (vterm-toggle centaur-tabs)
:preface (defun vmacs-awesome-tab-buffer-groups ()
          "`vmacs-awesome-tab-buffer-groups' control buffers' group rules. "
          (list
           (cond
            ((derived-mode-p 'eshell-mode 'term-mode 'shell-mode 'vterm-mode) "Term")
            ((string-match-p (rx (or "\*Helm"
                                     "\*helm"
                                     "\*tramp"
                                     "\*Completions\*"
                                     "\*sdcv\*"
                                     "\*Messages\*"
                                     "\*Ido Completions\*"))
                                     (buffer-name))
             "Emacs")
            (t "Common"))))
        (defun vmacs-term-mode-p(&optional args)
            (derived-mode-p 'eshell-mode 'term-mode 'shell-mode 'vterm-mode))
:config (setq centaur-tabs-buffer-groups-function   'vmacs-awesome-tab-buffer-groups)
        (setq vterm-toggle--vterm-buffer-p-function 'vmacs-term-mode-p)
)

(use-package shell-pop 
:custom (shell-pop-shell-type '("term" "vterm" (lambda () (vterm) )))
        (shell-pop-term-shell "/bin/zsh")
        (shell-pop-full-span t)
:general (leader "ut"'shell-pop)
:init    (global-set-key (kbd "<C-t>") 'shell-pop)
)

(use-package vterm-command :no-require t :straight nil
:after (vterm)
:preface
(defun run-in-vterm-kill (process event)
  "A process sentinel. Kills PROCESS's buffer if it is live."
  (let ((b (process-buffer process)))
    (and (buffer-live-p b)
         (kill-buffer b))))

(defun run-in-vterm (command)
  "Execute string COMMAND in a new vterm.
Interactively, prompt for COMMAND with the current buffer's file
name supplied. When called from Dired, supply the name of the file at point.
Like `async-shell-command`, but run in a vterm for full terminal features.
The new vterm buffer is named in the form `*foo bar.baz*`, the
command and its arguments in earmuffs.
When the command terminates, the shell remains open, but when the
shell exits, the buffer is killed."
  (interactive
   (list
    (let* ((f (cond (buffer-file-name)
                    ((eq major-mode 'dired-mode)
                     (dired-get-filename nil t))))
           (filename (concat " " (shell-quote-argument (and f (file-relative-name f))))))
      (read-shell-command "Terminal command: "
                          (cons filename 0)
                          (cons 'shell-command-history 1)
                          (list filename)))))
  (with-current-buffer (vterm (concat "*" command "*"))
    (set-process-sentinel vterm--process #'run-in-vterm-kill)
    (vterm-send-string command)
    (vterm-send-return)))
)

(provide 'module-terminal)
;;; module-terminal.el ends here
