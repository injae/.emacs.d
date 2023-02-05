;;; +run-command.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package run-command :after vterm
    :ensure-system-package (grip . "pip install grip")
    :preface
    (defun run-command-recipe ()
        (list
            ;; Run a simple command
            (list :command-name "say-hello"
                  :command-line "echo Hello, World!")

            ;; Do something with current file if it's a README
            ;; (uses https://github.com/joeyespo/grip)
            (when (equal (buffer-name) "README.md")
                (list :command-name "preview-github-readme"
                      :command-line "grip --browser --norefresh"
                      :display "Preview GitHub README"))

            ;; Do something with current file if it's executable
            (let ((buffer-file (buffer-file-name)))
                (when (and buffer-file
                            (file-executable-p buffer-file))
                (list :command-name "run-buffer-file"
                      :command-line buffer-file
                      :display "Run this buffer's file")))
            )
        )
    :custom (run-command-selector #'run-command-selector-completing-read)
            (run-command-default-runner #'run-command-runner-vterm)
            (run-command-recipes '(run-command-recipe))
    )


(provide '+run-command)
;;; +run-command.el ends here
