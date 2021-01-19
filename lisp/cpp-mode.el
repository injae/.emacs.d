;;; cpp-mode.el --- cpp base mode -*- lexical-binding: t; -*-

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
(defgroup cpp-mode nil "cpp-mode" :group 'tools)

(defcustom cpp-mode-on-p nil
    "cpp mode on off flag"
    :type 'boolean
    :group 'cpp-mode)

(defun cpp-mode-is-on ()
    "cpp mode is on off"
    (interactive)
    (if cpp-mode-on-p
        (progn (message "true"))
        (message "false")
    )
)

;;;###autoload
(define-minor-mode cpp-mode
"cpp mode"
:group 'cpp-mode
:lighter " cpp-mode"
:global nil
    (if cpp-mode
        (progn
            (setq cpp-mode-on-p t))
        (setq cpp-mode-on-p nil))
)

(provide 'cpp-mode)
;;; cpp-mode.el ends here
