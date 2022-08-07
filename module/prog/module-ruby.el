;;; module-ruby.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package ruby-mode :straight t
:mode "\\.rb\\'"
:mode "Rakefile\\'"
:mode "Gemfile\\'"
:mode "Berksfile\\'"
:mode "Vagrantfile\\'"
:interpreter "ruby"
:custom (ruby-indent-level 4)
        (ruby-indent-tabs-mode nil)
)

(use-package rvm :straight t
:after ruby-mode
:ensure-system-package (rvm . "curl -sSL https://get.rvm.io | bash -s stable")
:config (rvm-use-default)
)

(use-package yari :straight t :after ruby-mode)

(use-package rubocop :straight t
:ensure-system-package (rubocop . "sudo gem install rubocop")
:after ruby-mode
:init (add-hook 'ruby-mode-hook 'rubocop-mode)
)

(use-package robe :straight t
:after (ruby-mode company)
:ensure-system-package (pry . "sudo gem install pry pry-doc")
:init (add-hook 'ruby-mode-hook 'robe-mode)
:config (push 'company-robe company-backends)
)

(use-package ruby-tools :straight t
:after ruby-mode
:init (add-hook 'ruby-mode-hook 'ruby-tools-mode)
)


(provide 'module-ruby)
;;; module-ruby.el ends here
