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
(require 'seq)

(defvar noteditor/after-plugins-setup-hook nil
  "A hook that will be run after all the active plugins got setup
This hook is dedicated for the codes that need to do stuff based on other plugins
presence.  With this hook we eliminate the need for plugin ordering.

It will be called in the `noteditor-config' and the proper way to use
it is to use `noteditor/after-plugins' macro.")


(defvar noteditor/available-plugins '()
  "A list of all the registered plugins.")

(defvar noteditor/-plugins-body-hook nil
  "A Hook that is internal to NOTEDITOR and will be used to run the cub bodies.
The execution happens after Emacs is initialized.")


(defun noteditor/extract-props (body-list &optional acc)
  "Extract the props pairs from BODY-LIST with an optional accumulator ACC.

It will returen a pair in form of (body . props)."
  (let ((k (car body-list))
        (rest (cdr body-list)))

    (if (and k (keywordp k))
        (noteditor/extract-props
         (cdr rest)
         (cons (cdr rest) (plist-put (cdr acc) k (car rest))))
      (cons body-list (cdr acc)))))


(defun noteditor/run-plugin-after-initialization (f)
  "Run the given Plugin body function F after NOTEDITOR's initialization.
If NOTEDITOR is already initialized, just run F."
  (if (null noteditor/initialized)
      (add-hook 'noteditor/-plugins-body-hook f)
    (funcall f)))


(defmacro defplugin (plugin-name docs &rest props-n-body)
  "Define a plugin with the given PLUGIN-NAME, DOCS and a PROPS-N-BODY.

TODO: Docs"
  (declare (indent defun) (doc-string 2))

  ;; Make sure that props is a plist and contains the `:docs' key
  ;; TODO: Maybe use `cl-check-type' here
  (when (not (stringp docs))
    (error "Missing docstring for '%s' plugin" plugin-name))

  (let* ((parsed-body (noteditor/extract-props props-n-body))
         (body (car parsed-body))
         (props (cdr parsed-body)))

    (when (not (plist-get props :title))
      (error "Missing :titel key for '%s' plugin" plugin-name))

    (let ((complete-props (plist-put props :docs docs))
          (plugin-name-internal (intern (format "%s-internal" plugin-name)))
          ;; prop hooks
          (init-hook (plist-get props :init-hook))
          (ui-hook (plist-get props :ui-hook))

          (params-var (intern (format "%s-params" plugin-name)))
          (active-var (intern (format "%s-active-p" plugin-name)))
          (pre-lang-server-up-hook (intern (format "%s-pre-lang-server-up-hook" plugin-name)))
          (post-lang-server-up-hook (intern (format "%s-post-lang-server-up-hook" plugin-name)))
          (pre-lang-server-down-hook (intern (format "%s-pre-lang-server-down-hook" plugin-name)))
          (post-lang-server-down-hook (intern (format "%s-post-lang-server-down-hook" plugin-name)))
          (pre-init-hook (intern (format "%s-pre-init-hook" plugin-name)))
          (post-init-hook (intern (format "%s-post-init-hook" plugin-name)))
          (post-init-hook (intern (format "%s-post-init-hook" plugin-name)))

          (flag-var (or (plist-get props :flag) plugin-name))
          (flag-docstring-var (or (plist-get props :flag-doc)
                                  (format "The flag to enable/disable the '%s' plugin." plugin-name)))
          (flag-default (plist-get props :flag-default))
          (flag-conflict (plist-get props :conflicts-with))
          (no-flag? (or (plist-get props :no-flag) ())))

      (add-to-list 'noteditor/available-plugins plugin-name)

      `(progn

         ;; Create a new flag for each plugin to control the plugins systemwide.
         (when (not ,no-flag?)
           (defflag ,flag-var ,flag-docstring-var ,flag-default))

         ;; Params variable contains the list of params the is passed to
         ;; the current plugin call
         (defvar ,params-var nil
           ,(format "Parameters for the '%s' plugin." plugin-name))

         ;; * Hooks

         ;; This hook can be used by others to run code just before running that
         ;; plugin's body
         (defvar ,pre-init-hook nil
           ,(format "The hook that runs befor the '%s' plugin initialization." plugin-name))


         ;; This hook can be used by others to run code just after the body of
         ;; the plugin
         (defvar ,post-init-hook nil
           ,(format "The hook that runs after the '%s' plugin initialization." plugin-name))

         ;; TODO: Move language server related hooks to lang-server
         ;; TODO: Provide a way to let different parts of the
         ;;       codebase to create plugin hooks

         ;; ** Language Server
         ;;; The hook that enables users to change the language server configuration
         ;;; of the current plugin before activating the server
         (defvar ,pre-lang-server-up-hook nil
           ,(format "The hook that runs befor the '%s' plugin's language server activates ." plugin-name))

         ;;; The hook to do any post configuration for the lang server of the plugin
         (defvar ,post-lang-server-up-hook nil
           ,(format "The hook that runs after the '%s' plugin's language server activates." plugin-name))

         ;;; The hook to run code just before the language server is about to shutdown
         (defvar ,pre-lang-server-down-hook nil
           ,(format "The hook that runs befor the '%s' plugin's language server shuts down." plugin-name))

         ;;; The hook to run code after the language server successfully shuts down
         (defvar ,post-lang-server-down-hook nil
           ,(format "The hook that runs after the '%s' plugin's language server shuts down." plugin-name))

         ;; This way we can bypass the flag system if we really really want to.
         (defun ,plugin-name-internal (params)
           (if (or (not (boundp (quote ,active-var)))
                   (not ,active-var))
               (progn
                 ;; Mark this plugin as active
                 (setq ,active-var t)

                 ;; Set the parameters in PLUGIN-NAME-params to be accessable
                 ;; in the body
                 (setq ,params-var params)

                 ;; Run the pre init hook
                 (run-hooks (quote ,pre-init-hook))

                 (noteditor/info "Initializing '%s' plugin." (quote ,plugin-name))
                 ;; Run the body
                 (let ((result (progn ,@body)))
                   ;; Run the post init hook
                   (run-hooks (quote ,post-init-hook))
                   result))
             (noteditor/info "The '%s' plugin is already active." ',plugin-name)))

         (defun ,plugin-name (&rest params)
           (interactive)
           (let ((noteditor/---f-sym
                  (lambda ()
                    (when (not (null ,ui-hook))
                      (add-hook 'noteditor/ui-hook #',ui-hook))

                    (when (not (null ,init-hook))
                      (funcall #',init-hook params))

                    ;; Run the plugin internal after initialization or
                    ;; if Emacs is already up, just run it.
                    (noteditor/run-plugin-after-initialization
                     (lambda ()
                       (,plugin-name-internal params))))))

             (if ,no-flag?
                 ;; If no flag is need to control this plugin
                 (funcall noteditor/---f-sym)
               ;; Otherwise check for the flag to be active
               (if-flag ,flag-var
                 (funcall noteditor/---f-sym)
                 (noteditor/info "The flag for '%s' plugin is disabled. Skiping." ,(symbol-name plugin-name))))))

         ;; Set the symbol-plist of the plugin-name to its props
         (setplist ',plugin-name ',complete-props)))))


(defmacro noteditor/after-plugins (&rest body)
  "Add the BODY to `noteditor/after-plugins-setup-hook' hook."
  (declare (indent defun))
  `(add-hook 'noteditor/after-plugins-setup-hook
             (lambda ()
               ,@body)))


(defmacro ->plugin (pkg docs &rest body)
  "A wrapper to create a plugin that use only a PKG.
It passes the BODY to `pkg/use'.
And passes DOCS to `defplugin' as the plugin documentation."
  (declare (indent defun) (doc-string 2))
  `(defplugin ,(intern (format "noteditor/%s-plugin" pkg))
     ,docs
     :title ,(format "%s plugin" pkg)
     :flag ,pkg
     :flag-default t
     (pkg/use ,pkg ,@body)))


(provide 'noteditor/plugin)
;;; plugin.el ends here
