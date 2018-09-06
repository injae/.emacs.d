;;; package.el --- initialization emacs
;;; Commentry:
;;; emacs init file 
;;; Code:



(require 'package)
    (add-to-list 'package-archives '("elpa"      . "https://tromey.com/elpa/")            t)
    (add-to-list 'package-archives '("org"       . "http://orgmode.org/elpa/")            t)
    (add-to-list 'package-archives '("melpa"     . "http://melpa.milkbox.net/packages/")  t)
    (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
    (package-initialize)
    (unless (package-installed-p 'use-package)
        (package-refresh-contents)
        (package-install 'use-package)
    )
