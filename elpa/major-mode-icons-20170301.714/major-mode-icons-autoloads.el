;;; major-mode-icons-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "major-mode-icons" "major-mode-icons.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from major-mode-icons.el

(defvar major-mode-icons-icons-style 'xpm "\
Use `all-the-icons' package to show major mode icons.

If set to symbol `all-the-icons' then use `all-the-icons'.
Otherwise symbol `xpm' to use built-in xpm image files.")

(custom-autoload 'major-mode-icons-icons-style "major-mode-icons" t)

(autoload 'major-mode-icons--major-mode-list-match "major-mode-icons" "\
Return the matched item in `major-mode-list'.

\(fn)" nil nil)

(autoload 'major-mode-icons-show "major-mode-icons" "\
Show icon on mode-line.

\(fn)" nil nil)

(defpowerline powerline-major-mode-icons (cl-case major-mode-icons-icons-style (`all-the-icons (all-the-icons-icon-for-buffer)) (`xpm (let* ((match (major-mode-icons--major-mode-list-match)) (icon (cdr match)) (icon-path (concat major-mode-icons-icons-path icon ".xpm"))) (propertize (format-mode-line mode-name) 'display (if (and (image-type-available-p 'xpm) (file-exists-p icon-path)) (create-image icon-path 'xpm nil :ascent 'center)) 'mouse-face 'mode-line-highlight 'help-echo "Major-mode\nmouse-1: Display major mode menu\nmouse2: Show help for major mode\nmouse-3: Toggle minor modes" 'local-map (let ((map (make-sparse-keymap))) (define-key map [mode-line down-mouse-1] `(menu-item ,(purecopy "Menu Bar") ignore :filter (lambda (_) (mouse-menu-major-mode-map)))) (define-key map [mode-line mouse-2] 'describe-mode) (define-key map [mode-line down-mouse-3] mode-line-mode-menu) map))))))

(defvar major-mode-icons-lighter (cl-case major-mode-icons-icons-style (`all-the-icons (all-the-icons-icon-for-buffer)) (`xpm (let* ((match (major-mode-icons--major-mode-list-match)) (icon (cdr match))) (propertize (format-mode-line mode-name) 'display (let ((icon-path (concat major-mode-icons-icons-path icon ".xpm"))) (if (and (image-type-available-p 'xpm) (file-exists-p icon-path)) (create-image icon-path 'xpm nil :ascent 'center))) 'mouse-face 'mode-line-highlight 'help-echo "Major-mode\nmouse-1: Display major mode menu\nmouse2: Show help for major mode\nmouse-3: Toggle minor modes" 'local-map (let ((map (make-sparse-keymap))) (define-key map [mode-line down-mouse-1] `(menu-item ,(purecopy "Menu Bar") ignore :filter (lambda (_) (mouse-menu-major-mode-map)))) (define-key map [mode-line mouse-2] 'describe-mode) (define-key map [mode-line down-mouse-3] mode-line-mode-menu) map))))) "\
Lighter for minor mode `major-mode-icons'.")

(defvar major-mode-icons-mode t "\
Non-nil if Major-Mode-Icons mode is enabled.
See the `major-mode-icons-mode' command
for a description of this minor mode.")

(custom-autoload 'major-mode-icons-mode "major-mode-icons" nil)

(autoload 'major-mode-icons-mode "major-mode-icons" "\
A minor mode of showing icon for major-mode of current buffer.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "major-mode-icons" '("major-mode-icons-")))

;;;***

;;;### (autoloads nil nil ("major-mode-icons-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; major-mode-icons-autoloads.el ends here
