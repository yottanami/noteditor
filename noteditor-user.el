;;; Noteditor --- Based on NOTEDITOR -*- lexical-binding: t; -*-
 ;;
 ;;
 ;;
 ;; This program is free software; you can redistribute it and/or modify
 ;; it under the terms of the GNU General Public License as published by
 ;; the Free Software Foundation, either version 3 of the License, or
 ;; (at your option) any later version.
 ;;
 ;; This program is distributed in the hope that it will be useful,
 ;; but WITHOUT ANY WARRANTY; without even the implied warranty of
 ;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 ;; GNU General Public License for more details.
 ;;
 ;; You should have received a copy of the GNU General Public License
 ;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
 ;;
;;; Commentary:
 ;;
 ;;
;;; Code:

;; (when (file-exists-p "~/.noteditor-user.el")
;;   (load "~/.noteditor-user.el"))


;; ---------------------------------------------------------------------------
;; Developer languages.  Enable/disable full IDE support (LSP + tree-sitter
;; highlighting + debugging) per language.  Default: all six on.  This is read
;; at startup -- no Nix rebuild is needed to change the subset.
;; Valid symbols: c-cpp python ruby java typescript javascript
;;
;; (setq noteditor-devel-languages '(python ruby typescript))
;; ---------------------------------------------------------------------------

(provide 'noteditor-user) ;;;
;;; noteditor-user.el ends here
