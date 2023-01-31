;;; module-ruby.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package ruby-mode
:mode "\\.rb\\'"
:mode "Rakefile\\'"
:mode "Gemfile\\'"
:mode "Berksfile\\'"
:mode "Vagrantfile\\'"
:interpreter "ruby"
:custom (ruby-indent-level 4)
        (ruby-indent-tabs-mode nil)
)

(use-package rvm
:after ruby-mode
;:ensure-system-package (rvm . "curl -sSL https://get.rvm.io | bash -s stable")
:config (rvm-use-default)
)

(use-package yari :after ruby-mode)

(use-package rubocop :after ruby-mode
:ensure-system-package (rubocop . "gem install rubocop")
:init (add-hook 'ruby-mode-hook 'rubocop-mode)
)

(use-package robe :after ruby-mode
:ensure-system-package (pry . "gem install pry pry-doc")
:init (add-hook 'ruby-mode-hook 'robe-mode)
)

(use-package ruby-tools :after ruby-mode
:init (add-hook 'ruby-mode-hook 'ruby-tools-mode)
)

(provide 'module-ruby)
;;; module-ruby.el ends here
