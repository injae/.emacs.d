;;; major-mode-icons.el --- display icon for major-mode on mode-line.

;; Authors: stardiviner <numbchild@gmail.com>
;; Package-Requires: ((emacs "24.3") (powerline "2.4") (all-the-icons "2.3.0"))
;; Version: 0.2
;; Keywords: frames multimedia
;; homepage: http://github.com/stardiviner/major-mode-icons

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; If you want to use `major-mode-icons--major-mode-extra' extra info, you
;; should install corresponding packages.
;;
;; - clojure-mode <-> cider
;; - ruby-mode <-> rbenv
;; - python-mode <-> pyvenv


;;; Code:
;;; ----------------------------------------------------------------------------

(require 'cl-lib)
(require 'powerline)

(defgroup major-mode-icons nil
  "Show icon for current buffer's major-mode."
  :group 'mode-line)

(defconst major-mode-icons--icons-default-path
  (concat
   (file-name-directory (or load-file-name
                            (buffer-file-name)))
   "icons/")
  "Default icons path of major-mode-icons.")

(defcustom major-mode-icons-icons-path major-mode-icons--icons-default-path
  "Path to icons."
  :group 'major-mode-icons
  :type 'string)

(defcustom major-mode-icons-mode-name-font "Segoe Print"
  "The font family used for major mode name."
  :group 'major-mode-icons
  :type 'string)

;;;###autoload
(defcustom major-mode-icons-icons-style 'xpm
  "Use `all-the-icons' package to show major mode icons.

If set to symbol `all-the-icons' then use `all-the-icons'.
Otherwise symbol `xpm' to use built-in xpm image files."
  :group 'major-mode-icons
  :type 'string)

(require (pcase major-mode-icons-icons-style
           (`all-the-icons 'all-the-icons)
           (`xpm 'xpm)))

;; major mode with icon
(defvar major-mode-icons--major-mode-list
  '(((emacs-lisp-mode
      inferior-emacs-lisp-mode
      ielm-mode) . "Emacs")
    ((lisp-mode
      inferior-lisp-mode
      slime-repl-mode sly-mrepl-mode) . "Common-Lisp")
    ((scheme-mode) . "Scheme")
    ((clojure-mode
      cider-repl-mode) . "Clojure")
    ((clojurescript-mode) . "ClojureScript")
    ((python-mode) . "Python")
    ((enh-ruby-mode ruby-mode) . "Ruby")
    ((inf-ruby-mode) . "inf-ruby")
    ((c-mode) . "C")
    ((c++-mode) . "C++")
    ((csharp-mode) . "C#")
    ((go-mode) . "Go")
    ((swift-mode) . "Swift")
    ((rust-mode) . "Rust")
    ((java-mode) . "Java")
    ((php-mode) . "PHP")
    ((web-mode html-mode) . "HTML")
    ((css-mode) . "CSS")
    ((javascript-mode
      js-mode js2-mode js3-mode inferior-js-mode) . "JavaScript")
    ((coffee-mode) . "CoffeeScript")
    ((org-mode org-agenda-mode) . "Org-mode")
    ((tex-mode latex-mode TeX-mode LaTeX-mode) . "TeX")
    ((bibtex-mode) . "BibTeX")
    ((markdown-mode) . "Markdown")
    ((yaml-mode) . "YAML")
    ((rst-mode) . "reStructuredText")
    ((eshell-mode) . "Command-Line")
    ((sh-mode shell-mode) . "Shell")
    ((term-mode) . "term")
    ((powershell-mode) . "powershell")
    ((ess-mode R-mode) . "R")
    ((julia-mode ess-julia-mode) . "Julia")
    ((gnuplot-mode) . "gnuplot")
    ((octave-mode) . "Octave")
    ((matlab-mode) . "Matlab")
    ((haskell-mode) . "Haskell")
    ((scala-mode) . "Scala")
    ((erlang-mode) . "Erlang")
    ((prolog-mode) . "Prolog")
    ((ocaml-mode) . "OCaml")
    ((sql-mode) . "SQL")
    ((xml-mode nxml-mode) . "XML")
    ((json-mode) . "JSON")
    ((diff-mode ediff-mode magit-diff-mode) . "diff")
    ((asm-mode nasm-mode) . "Assembly")
    ((android-mode) . "Android")
    ((qt-mode) . "Qt")
    ((arduino-mode) . "Arduino")
    ((systemd-mode) . "Systemd")
    ((docker-mode) . "Docker")
    ((projectile-rails-mode) . "Rails")
    ((slim-mode) . "Slim")
    ((sass-mode) . "Sass")
    ((spice-mode) . "Electric")
    )
  "Pairs: ([mode-list] . [icon-name])."
  )

;;;###autoload
(defun major-mode-icons--major-mode-list-match ()
  "Return the matched item in `major-mode-list'."
  (assoc
   (cl-some ; or use (remove nil '(nil nil (clojure-mode) nil nil ...))
    (lambda (elem)
      (when (not (null elem))
        elem))
    (mapcar
     (lambda (element)
       (member major-mode element))
     (mapcar 'car major-mode-icons--major-mode-list)))
   major-mode-icons--major-mode-list))

(defun major-mode-icons--major-mode-icon (&optional extra)
  "Display icon for current buffer's `major-mode' and `EXTRA' info."
  (let* ((match (major-mode-icons--major-mode-list-match))
         (icon (cdr match))
         (icon-path (concat major-mode-icons-icons-path icon ".xpm")))
    (concat
     (if (and (file-exists-p icon-path) (image-type-available-p 'xpm))
         (propertize
          "  "
          'display (if (and (file-exists-p icon-path) (image-type-available-p 'xpm))
                       (create-image icon-path 'xpm nil :ascent 'center))
          'mouse-face 'mode-line-highlight
          'help-echo "Major-mode\n\ mouse-1: Display major mode menu\n\ mouse2: Show help for major mode\n\ mouse-3: Toggle minor modes"
          'local-map (let ((map (make-sparse-keymap)))
                       (define-key map [mode-line down-mouse-1]
                         `(menu-item ,(purecopy "Menu Bar") ignore
                                     :filter (lambda (_) (mouse-menu-major-mode-map))))
                       (define-key map [mode-line mouse-2] 'describe-mode)
                       (define-key map [mode-line down-mouse-3] mode-line-mode-menu)
                       map)
          )
       (propertize (format-mode-line mode-name))
       )
     ;;; extra
     (if extra
         (propertize (format " %s" (or extra "")))
       "")
     )))

;;; auto show extra info
(defun major-mode-icons--major-mode-extra ()
  "Extend function `major-mode-icon' with extra info."
  (let ((extra
         (cl-case major-mode
           ('clojure-mode
            (if (and (featurep 'cider)
                     (not (equal (cider--modeline-info) "not connected")))
                (cider--project-name nrepl-project-dir)))
           ('enh-ruby-mode
            (if (and (featurep 'rbenv) global-rbenv-mode)
                (rbenv--active-ruby-version) ; `rbenv--modestring'
              ))
           ('python-mode
            (if (and (featurep 'pyvenv) pyvenv-mode)
                ;; `pyvenv-mode-line-indicator' -> `pyvenv-virtual-env-name'
                pyvenv-virtual-env-name
              ;; conda: `conda-env-current-name'
              ))
           )))))

;;;###autoload
(defun major-mode-icons-show ()
  "Show icon on mode-line."
  (cl-case major-mode-icons-icons-style
    (`all-the-icons
     (all-the-icons-icon-for-buffer))
    (`xpm
     (major-mode-icons--major-mode-icon (major-mode-icons--major-mode-extra)))))

;;;###autoload
(defpowerline powerline-major-mode-icons
  (cl-case major-mode-icons-icons-style
    (`all-the-icons
     (all-the-icons-icon-for-buffer))
    (`xpm
     (let* ((match (major-mode-icons--major-mode-list-match))
            (icon (cdr match))
            (icon-path (concat major-mode-icons-icons-path icon ".xpm")))
       (propertize (format-mode-line mode-name) ; display `mode-name' text.
                   'display ; display icon
                   (if (and (image-type-available-p 'xpm)
                            (file-exists-p icon-path))
                       (create-image icon-path 'xpm nil :ascent 'center))
                   'mouse-face 'mode-line-highlight
                   'help-echo "Major-mode\n\ mouse-1: Display major mode menu\n\ mouse2: Show help for major mode\n\ mouse-3: Toggle minor modes"
                   'local-map (let ((map (make-sparse-keymap)))
                                (define-key map [mode-line down-mouse-1]
                                  `(menu-item ,(purecopy "Menu Bar") ignore
                                              :filter (lambda (_) (mouse-menu-major-mode-map))))
                                (define-key map [mode-line mouse-2] 'describe-mode)
                                (define-key map [mode-line down-mouse-3] mode-line-mode-menu)
                                map))))))

;;;###autoload
(defvar major-mode-icons-lighter
  (cl-case major-mode-icons-icons-style
    (`all-the-icons
     ;; - `all-the-icons-icon-for-buffer'
     ;; - `all-the-icons-icon-for-file'
     ;; - `all-the-icons-icon-for-mode'
     (all-the-icons-icon-for-buffer)
     )
    (`xpm
     (let* ((match (major-mode-icons--major-mode-list-match))
            (icon (cdr match)))
       (propertize (format-mode-line mode-name) ; display `mode-name' text.
                   'display ; display icon
                   (let ((icon-path
                          (concat major-mode-icons-icons-path icon ".xpm")))
                     (if (and (image-type-available-p 'xpm)
                              (file-exists-p icon-path))
                         (create-image icon-path 'xpm nil :ascent 'center)
                       ))
                   'mouse-face 'mode-line-highlight
                   'help-echo "Major-mode\n\ mouse-1: Display major mode menu\n\ mouse2: Show help for major mode\n\ mouse-3: Toggle minor modes"
                   'local-map (let ((map (make-sparse-keymap)))
                                (define-key map [mode-line down-mouse-1]
                                  `(menu-item ,(purecopy "Menu Bar") ignore
                                              :filter (lambda (_) (mouse-menu-major-mode-map))))
                                (define-key map [mode-line mouse-2] 'describe-mode)
                                (define-key map [mode-line down-mouse-3] mode-line-mode-menu)
                                map))))
    )
  "Lighter for minor mode `major-mode-icons'.")

(put 'major-mode-icons-lighter 'risky-local-variable t)

;;;###autoload
(define-minor-mode major-mode-icons-mode
  "A minor mode of showing icon for major-mode of current buffer."
  :init-value t
  :lighter major-mode-icons-lighter
  :global t)

;;; ----------------------------------------------------------------------------

(provide 'major-mode-icons)

;;; major-mode-icons.el ends here
