;;; module-flycheck.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'use-package)

(use-package flycheck
    :custom
    (flycheck-clang-language-standard "c++17")
    :config
    (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
    (setq straight-fix-flycheck t)
    (global-flycheck-mode)
)

(use-package flycheck-package  :after flycheck
    :config (flycheck-package-setup))

(use-package flyspell :after flycheck
:hook ((prog-mode . flyspell-prog-mode)
       (text-mode . flyspell-mode))
:general (leader "sk" '((lambda () (interactive) (ispell-change-dictionary "ko_KR") (flyspell-buffer)) :wk "Spell Dictionary Korean")
                 "se" '((lambda () (interactive) (ispell-change-dictionary "en_US") (flyspell-buffer)) :wk "Spell Dictionary English"))
:custom (ispell-dictionary   "en_US")
        (ispell-program-name "aspell")
:config
    ;; 스펠체크 넘어가는 부분 설정
    ;(add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
    ;(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
    ;(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))
)

(provide 'module-flycheck)
;;; module-flycheck.el ends here
