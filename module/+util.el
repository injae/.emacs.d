;;; +util.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package try :defer t)

(use-package sudo-mode :elpaca nil :no-require t
:preface
(defun sudo-find-file (file-name)
    "sudo open"
    (interactive "FSudo Find File: ")
    (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
        (find-file tramp-file-name)))
:general (leader "fs" #'sudo-find-file)
)

(use-package goto-last-change
:general (leader "fl" 'goto-last-change)
)

(use-package restart-emacs)

(defun reload-emacs ()
    ;; reload emacs config 
    (interactive)
    (load-file (expand-file-name "~/.emacs.d/init.el"))
)

(defun new-buffer-save (name buffer-major-mode)
    (interactive)
    (let ((buffer (generate-new-buffer name)))
         (switch-to-buffer buffer)
         (set-buffer-major-mode buffer)
         (funcall buffer-major-mode)
         (setq buffer-offer-save t))
)

(defun new-buffer (name buffer-major-mode)
    (let ((buffer (generate-new-buffer name)))
         (switch-to-buffer buffer)
         (set-buffer-major-mode buffer)
         (funcall buffer-major-mode))
)

(defun new-no-name-buffer ()
    (interactive)
    (new-buffer "untitled" 'text-mode)
)

(use-package hungry-delete :disabled
; 공백 지울때 한꺼번에 다지워짐
:init (global-hungry-delete-mode)
)

(use-package face-picker :elpaca nil :no-require t
:preface
(defun what-face (pos)
     (interactive "d")
     (let ((face (or (get-char-property (pos) 'read-face-name)
                     (get-char-property (pos) 'face))))
          (if face (message "Face: %s" face) (message "No face at %d" pos))))
)

; text random
(defun randomize-region (beg end)
    (interactive "r")
    (if (> beg end)
        (let (mid) (setq mid end end beg beg mid)))
    (save-excursion
        ;; put beg at the start of a line and end and the end of one --
        ;; the largest possible region which fits this criteria
        (goto-char beg)
        (or (bolp) (forward-line 1))
        (setq beg (point))
        (goto-char end)
        ;; the test for bolp is for those times when end is on an empty
        ;; line; it is probably not the case that the line should be
        ;; included in the reversal; it isn't difficult to add it
        ;; afterward.
        (or (and (eolp) (not (bolp)))
            (progn (forward-line -1) (end-of-line)))
        (setq end (point-marker))
        (let ((strs (shuffle-list
                    (split-string (buffer-substring-no-properties beg end)
                                "\n"))))
        (delete-region beg end)
        (dolist (str strs)
            (insert (concat str "\n"))))))

(defun shuffle-list (list)
"Randomly permute the elements of LIST.
All permutations equally likely."
(let ((i 0) j temp
    (len (length list)))
        (while (< i len)
        (setq j (+ i (random (- len i))))
        (setq temp (nth i list))
        (setcar (nthcdr i list) (nth j list))
        (setcar (nthcdr j list) temp)
        (setq i (1+ i))))
    list)

;;; 키입력 보여주는 플러그인
(use-package keypression 
:commands keypression-mode
:custom (keypression-use-child-frame t)
        (keypression-fade-out-delay 1.0)
        (keypression-frame-justify 'keypression-right-fringe)
        (keypression-cast-command-name t)
        (keypression-cast-coommand-name-format "%s  %s")
        (keypression-frame-background-mode 'white)
        (keypression-combine-same-keystrokes t)
        (keypression-frames-maxnum 20)
        (keypression-font-face-attribute '(:width normal :height 200 :weight bold))
)

(use-package undo-fu :after general
    :general (leader "uu" 'undo-fu-only-undo
                     "ur" 'undo-fu-only-redo)
    :custom
    (undo-limit         67108864) ;;  64mb.
    (undo-strong-limit 100663296) ;;  96mb.
    (undo-outer-limit 1006632960) ;; 960mb.
    :config
    (evil-define-key 'normal 'global (kbd "C-r") #'undo-fu-only-redo)
    (evil-define-key 'normal 'global "u" #'undo-fu-only-undo)
    (defalias 'redo 'undo-fu-only-redo)
    (defalias 'undo 'undo-fu-only-undo)
    (setq evil-undo-system 'undo-fu)
)

(use-package undohist :after undo-fu
    :config (undohist-initialize))

(use-package undo-fu-session :after undo-fu :disabled
    :functions undo-fu-session-global-mode 
    :config (undo-fu-session-global-mode))

(use-package vundo :after general
    :general (leader "uv" 'vundo))

(use-package server :elpaca nil
    :config (add-hook 'after-init-hook 'server-start t))

(provide '+util)
;;; +util.el ends here
