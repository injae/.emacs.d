;;; use-package-evil-leader.el --- evil-leader support for use-package -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Injae Lee

;; Author: Injae Lee <8687lee@gmail.com>
;; Keywords: lisp
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Put a description of the package here

;;; Code:

(require 'use-package)
(require 'evil-leader)

(defun use-package-evil-leader--normalize (name keyword args)
    "use-package :evil-leader keyword handler."
    (let (arg result*)
        (while args
            (cond
                (and (consp (x (car args)))
                 (or (stringp  (car x))
                     (vectorp  (car x)))
                 (or (use-package-reco (cdr x) t #'stringp)))
                  
                ((listp (car args))
                   (setq result* (nconc result* (use-package-evil- name keyword (car args))) 
                         args (cdr args)))
                (t (use-package-error ":evil-leader wants (name func)"))
            )
        )
    )


    (let ((arg args) args*)
         (while arg
            (let ((x (car arg)))
                 (cond
                    ((and (consp x)
                                (or (stringp (car x))
                                    (vectorp (car x)))
                                (or (use-package-recognize-function (cdr x) t #'stringp)))
                            (setq args* (nconc args* (list x)))
                            (setq arg (cdr arg)))
                    ((listp x)
                        (setq args* (nconc args* (use-package-evil-leader--normalize name keyword x)))
                        (setq arg (cdr arg)))
                    (t (use-package-error ":evil-leader wants (name func)")))))))

(defalias 'use-package-normalize/:evil-leader 'use-package-evil-leader--normalize
    "Normalize for the definition  of one or more evil-leader")

(defun use-package-handler/:evil-leader (name keyword args rest state)
  "use-package :evil-leader keyword handler."
    (use-package-concat
        (mapcar #'(lambda (key) '(evil-leader/set-key key)) args)
        (use-package-process-keywords name rest state)
    )
)
(defun use-package-evil-leader-setup ()
  "Set up use-package keyword :evil-leader ."
  (add-to-list 'use-package-keywords :evil-leader))

(provide 'use-package-evil-leader)
























