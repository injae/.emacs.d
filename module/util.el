; package testing 
(use-package try :straight t  :defer t)


(use-package sudo-mode :no-require t :ensure nil
:preface
(defun sudo-find-file (file-name)
    "sudo open"
    (interactive "FSudo Find File: ")
    (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
        (find-file tramp-file-name)))
:general (leader "fs" #'sudo-find-file)
)

(use-package goto-last-change :straight t  :defer t
;https://github.com/camdez/goto-last-change.el
:general (leader "fl" 'goto-last-change)
)

(use-package no-littering :straight t 
:config (require 'recentf)
        (add-to-list 'recentf-exclude no-littering-var-directory)
        (add-to-list 'recentf-exclude no-littering-etc-directory)
        (setq auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
)

(use-package restart-emacs :straight t)

(defun reload-emacs ()
    ; reload emacs config 
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

(use-package hungry-delete :straight t :disabled
; 공백 지울때 한꺼번에 다지워짐
:init (global-hungry-delete-mode)
)

(use-package face-picker :no-require t :ensure nil
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
(use-package keypression :straight t
:commands keypression-mode
:custom (keypression-use-child-frame t)
        (keypression-fade-out-delay 1.0)
        (keypression-frame-justify 'keypression-left-fringe)
        (keypression-cast-command-name t)
        (keypression-cast-coommand-name-format "%s  %s")
        (keypression-frame-background-mode 'white)
        (keypression-combine-same-keystrokes t)
        (keypression-frames-maxnum 20)
        (keypression-font-face-attribute '(:width normal :height 200 :weight bold))
)

(use-package undo-tree :straight t  :diminish undo-tree-mode :after general
:commands (undo-tree-undo undo-tree-redo)
:general (leader "uu" 'undo-tree-undo
                 "ur" 'undo-tree-redo)
:init
    (evil-define-key 'normal 'global (kbd "C-r") #'undo-tree-redo)
    (evil-define-key 'normal 'global "u" #'undo-tree-undo)
    (defalias 'redo 'undo-tree-redo)
    (defalias 'undo 'undo-tree-undo)
:config
    (global-undo-tree-mode)
)

(use-package server :config (unless (server-running-p) (server-start)))
