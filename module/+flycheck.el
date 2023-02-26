;;; +flycheck.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package flycheck :after exec-path-from-shell
    :custom (flycheck-clang-language-standard "c++17")
    :config (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
    :hook (emacs-startup . global-flycheck-mode)
)

(use-package flycheck-package :after flycheck
    :config (flycheck-package-setup))

(use-package flyspell :elpaca nil
    :general (leader "sk" '((lambda () (interactive) (ispell-change-dictionary "ko_KR") (flyspell-buffer)) :wk "Spell Dictionary Korean")
                     "se" '((lambda () (interactive) (ispell-change-dictionary "en_US") (flyspell-buffer)) :wk "Spell Dictionary English"))
    :hook ((prog-mode . flyspell-mode)
           (text-mode . flyspell-mode))
    :custom (ispell-dictionary   "en_US")
    :config
        ;; Make sure new aspell is installed
        (when (executable-find "aspell")
            (setq ispell-program-name "aspell")
            (setq ispell-list-command "--list"))
        (setq-default ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--camel-case"))

        (setq ispell-personal-dictionary (f-join user-emacs-directory ".personal-dict"))
        ;; 스펠체크 넘어가는 부분 설정
        ;(add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
        ;(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
        ;(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))
)

(use-package flyspell-correct :after flyspell
    :general (leader "sf"  #'flyspell-correct-wrapper)
    :config
    (define-key flyspell-mode-map (kbd "C-;") #'flyspell-correct-wrapper)
    )

(use-package consult-flyspell :after (flyspell-correct consult)
    :custom
    (consult-flyspell-select-function nil
          consult-flyspell-set-point-after-word t
          consult-flyspell-always-check-buffer nil)
    (consult-flyspell-correct-function (lambda () (flyspell-correct-at-point) (consult-flyspell))))


(provide '+flycheck)
;;; +flycheck.el ends here
