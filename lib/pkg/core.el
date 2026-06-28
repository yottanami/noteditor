;;; Noteditor --- Based on FG42 -*- lexical-binding: t; -*-
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

(require 'core/utils)

;; All packages are provided by Nix at build time and are already on
;; `load-path' (via emacsWithPackages).  We therefore drive everything
;; through plain `use-package' with `:ensure nil' -- no network access,
;; no package manager bootstrap, no writes to the store.

(defun pkg/initialize ()
  "Initialize the package layer.
Packages are preinstalled by Nix and live on `load-path'.  Activate
their autoloads (this also runs at a normal startup, but not under
`--batch', so we call it explicitly), then load `use-package' and make
sure it never tries to install anything itself."
  (require 'package)
  (package-activate-all)
  (require 'use-package)
  (setq use-package-always-ensure nil))

(defun pkg/strip-straight (args)
  "Return ARGS (a `use-package' plist tail) without any `:straight' pair.
Removes the `:straight' keyword and its single value form, whatever its
shape (t, a symbol, or a list recipe such as (smart-mode-line :source melpa))."
  (let (out (rest args))
    (while rest
      (if (eq (car rest) :straight)
          (setq rest (cddr rest))       ; drop the keyword and its value
        (push (car rest) out)
        (setq rest (cdr rest))))
    (nreverse out)))

(defun inject-defer (args)
  "Inject `:defer t' into ARGS if the key was missing."
  (if (member :defer args)
      args
    (append args '(:defer t))))

(defmacro pkg/use (pkg &rest details)
  "Configure the preinstalled package PKG via `use-package' with DETAILS.
Any `:straight' recipe is stripped (packages come from Nix) and `:ensure
nil' is forced so `use-package' never reaches out to a package archive."
  (declare (indent defun))
  `(use-package ,pkg :ensure nil
     ,@(inject-defer (pkg/strip-straight details))))

(provide 'lib/pkg/core)
;;; core.el ends here
