

;; python setting
(use-package elpy :ensure t
:ensure-system-package (jedi . "pip install --user jedi flake8 autopep8 black yapf importmagic")
:after python-mode
:hook (python-mode . elpy-enable)
:config (eldoc-mode 0)
)

(use-package anaconda-mode :ensure t
:after  python-mode
:config (add-hook 'python-mode-hook 'anaconda-mode)
        (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

(use-package company-anaconda :ensure t
:after  (company-mode anaconda-mode)
)

; eldoc setting
(use-package eldoc-rtags :no-require t :ensure nil :disabled
:after (eldoc rtags)
:preface
    (defun fontify-string (str mode)
        "Return STR fontified according to MODE."
        (with-temp-buffer
            (insert str)
            (delay-mode-hooks (funcall mode))
            (font-lock-default-function mode)
            (font-lock-default-fontify-region
            (point-min) (point-max) nil)
            (buffer-string)
        )
    )

    (defun rtags-eldoc-function ()
        (let ((summary (rtags-get-summary-text)))
            (and summary
                (fontify-string
                (replace-regexp-in-string
                "{[^}]*$" ""
                (mapconcat
                    (lambda (str) (if (= 0 (length str)) "//" (string-trim str)))
                    (split-string summary "\r?\n")
                    " "))
                major-mode))))

    (defun rtags-eldoc-mode ()
        "rtags eldoc extensions"
        (interactive)
        (setq-local eldoc-documentation-function #'rtags-eldoc-function)
        (eldoc-mode 1)
    )
:config
    (add-hook 'c-mode-hook   'rtags-eldoc-mode)
    (add-hook 'c++-mode-hook 'rtags-eldoc-mode)
)


(use-package gdb-mi
:ensure (:host github :repo "weirdNox/emacs-gdb" :files ("*.el" "*.c" "*.h" "Makefile"))
:general (leader "de" 'gdb-executable
                "dn" 'gdb-next
                "di" 'gdb-step
                "df" 'gdb-finish)
:config (setq-default gdb-show-main t)
        (setq-default gdb-many-windows t)
        (fmakunbound 'gdb)
        (fmakunbound 'gdb-enable-debug)
)
