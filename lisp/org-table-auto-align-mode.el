;;; package --- Summary
;;; Commentary:
;;; Code:
(require 'subr-x)

(defgroup org-table-auto-align nil
  "org table auto align group"
  :group 'convenience)

(defcustom org-table-auto-align-in-progress nil
    "org table auto align in progress"
    :type 'boolean :group 'org-table-auto-align)

(defun org-table-auto-align (begin end length)
  "org table auto align mode function"
  (save-match-data
    (unless (or org-table-auto-align-in-progress
                (not (org-at-table-p))
                (and (eq this-command 'org-self-insert-command)
                     (member (this-command-keys) '(" " "+" "|" "-"))))
      ;; uses zero-idle timer so the buffer content is settled after
      ;; the change, the cursor is moved, so we know what state we
      ;; have to restore after auto align
      (run-with-idle-timer
       0 nil
       (lambda ()
         (if (looking-back "| *\\([^|]+\\)")
             (let ((pos (string-trim-right (match-string 1))))
               (setq org-table-auto-align-in-progress t)
               (unwind-protect
                   (progn
                     (org-table-align)
                     (search-forward pos nil t))
                 (setq org-table-auto-align-in-progress nil)))))))))

(define-minor-mode org-table-auto-align-mode
  "A mode for aligning Org mode tables automatically as you type."
  :lighter " OrgTblAA"
  (if org-table-auto-align-mode
      (add-hook 'after-change-functions #'org-table-auto-align t t)
    (remove-hook 'after-change-functions #'org-table-auto-align t)))

(provide 'org-table-auto-align-mode)
;;; org-table-auto-align-mode.el ends here
