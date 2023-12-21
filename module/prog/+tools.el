;;; +tools.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

; brew install rust base system command
(use-package rust-system-command :elpaca nil :no-require t
:ensure-system-package ((rg    . "cargo install ripgrep")
                        (exa   . "cargo install exa")
                        (bat   . "cargo install bat")
                        (procs . "cargo install procs")
                        (dust  . "cargo install du-dust")
                        (ytop  . "cargo install ytop")
                        (mcfly . "cargo install mcfly")
                        (erd   . "cargo install erdtree")
                        (cargo-install-update . "cargo install cargo-update"))
)

(provide '+tools)
;;; +tools.el ends here
