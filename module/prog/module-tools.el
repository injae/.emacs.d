;;; module-tools.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'use-package)
(require 'straight)

; brew install rust base system command
(use-package rust-system-command :straight nil :no-require t
:ensure-system-package ((rg    . "cargo install ripgrep")
                        (exa   . "cargo install exa")
                        (bat   . "cargo install bat")
                        (procs . "cargo install procs")
                        (dust  . "cargo install du-dust")
                        (ytop  . "cargo install ytop"))
)

(provide 'module-tools)
;;; module-tools.el ends here
