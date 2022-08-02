
; brew install rust base system command
(use-package rust-system-command :no-require t :ensure nil
:ensure-system-package ((rg    . "cargo install ripgrep")
                        (exa   . "cargo install exa")
                        (bat   . "cargo install bat")
                        (procs . "cargo install procs")
                        (dust  . "cargo install du-dust")
                        (ytop  . "cargo install ytop"))
)
