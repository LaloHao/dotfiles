;;; hao-js.el --- javascript development config   -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'prelude-packages "~/.emacs.d/init.el")

(with-no-warnings
  (require 'cl))

(require 'js)
(require 'rjsx-mode)
(require 'emmet-mode)
(require 'add-node-modules-path)
(require 'evil)
(require 'js2-refactor)
(require 'js-doc)
(require 'flow-minor-mode)
;; (require 'node-ac-mode)

(setq js-indent-level 2)

(setq js2-mode-show-parse-errors nil)
(setq js2-mode-show-strict-warnings nil)

(setq emmet-expand-jsx-className? t)
(setq emmet-self-closing-tag-style " /")

(evil-define-key
  'insert rjsx-mode-map (kbd "C-d") 'rjsx-delete-creates-full-tag)

(setq
 js-doc-mail-address "lalohao@gmail.com"
 js-doc-author (format "Eduardo V. <%s>" js-doc-mail-address)
 js-doc-url "https://hao.systems"
 js-doc-license "WTFPL")

;; (delete '("\\.js\\'" . js2-mode) auto-mode-alist)
;; (delete '("\\.js\\'" . rjsx-mode) auto-mode-alist)
;; (add-to-list 'auto-mode-alist '("\\.js$" . rjsx-mode))
;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(defvar react-filter-files nil
  "Ignore these files when reading directories.")

(setq
 react-filter-files
 '("."
   ".."
   ".#index.js"
   "index.js"
   "./"
   ".eslintrc"
   ".eslintrc.json"
   ))

(cl-defun react-directory-files (&optional (directory "."))
  "List DIRECTORY files and remove `react-filter-files'."
  (remove-if
   (lambda (file) (member file react-filter-files))
   (directory-files directory)))

;; (react-directory-files)

(defun remove-hyphens (string)
  "Remove hyphens from STRING."
  (replace-regexp-in-string "-" "" string))

(defun react-native/update-index-file ()
  "Update index file."
  (interactive)
  (let ((files (react-directory-files)))
    (erase-buffer)
    (mapc
     (lambda (file)
       (let* ((file (file-name-base file))
           (export file)
           ;; (export (remove-hyphens (capitalize file)))
           )
         (insert
          (concat "export { default as " export " } from './" file "';\n")
          ;; (concat "export * from './" file "';\n")
          )))
     files)))

(defun react-native/update-reducers-file ()
  "Update reducers file."
  (interactive)
  (let ((files (react-directory-files)))
    (erase-buffer)
    (insert "import { combineReducers } from 'redux';

export default combineReducers({
")
    (mapc
     (lambda (f)
       (insert
        (concat
         "    "
         (replace-regexp-in-string ".js" "" f)
         ": require('./" f "'),
"
         )))
     files)
    (insert "});")
    ))

(defun js2r-setup-keys ()
  "Setup js2-reafactor.el."
  (define-key evil-normal-state-map (kbd "C-k ee") #'js2r-expand-node-at-point)
  (define-key evil-normal-state-map (kbd "C-k cc") #'js2r-contract-node-at-point)
  (define-key evil-normal-state-map (kbd "C-k wi") #'js2r-wrap-buffer-in-iife)
  (define-key evil-normal-state-map (kbd "C-k ig") #'js2r-inject-global-in-iife)
  (define-key evil-normal-state-map (kbd "C-k ev") #'js2r-extract-var)
  (define-key evil-normal-state-map (kbd "C-k el") #'js2r-extract-let)
  (define-key evil-normal-state-map (kbd "C-k ec") #'js2r-extract-const)
  (define-key evil-normal-state-map (kbd "C-k iv") #'js2r-inline-var)
  (define-key evil-normal-state-map (kbd "C-k rv") #'js2r-rename-var)
  (define-key evil-normal-state-map (kbd "C-k vt") #'js2r-var-to-this)
  (define-key evil-normal-state-map (kbd "C-k ag") #'js2r-add-to-globals-annotation)
  (define-key evil-normal-state-map (kbd "C-k sv") #'js2r-split-var-declaration)
  (define-key evil-normal-state-map (kbd "C-k ss") #'js2r-split-string)
  (define-key evil-normal-state-map (kbd "C-k st") #'js2r-string-to-template)
  (define-key evil-normal-state-map (kbd "C-k ef") #'js2r-extract-function)
  (define-key evil-normal-state-map (kbd "C-k em") #'js2r-extract-method)
  (define-key evil-normal-state-map (kbd "C-k ip") #'js2r-introduce-parameter)
  (define-key evil-normal-state-map (kbd "C-k lp") #'js2r-localize-parameter)
  (define-key evil-normal-state-map (kbd "C-k tf") #'js2r-toggle-function-expression-and-declaration)
  (define-key evil-normal-state-map (kbd "C-k ta") #'js2r-toggle-arrow-function-and-expression)
  (define-key evil-normal-state-map (kbd "C-k ts") #'js2r-toggle-function-async)
  (define-key evil-normal-state-map (kbd "C-k ao") #'js2r-arguments-to-object)
  (define-key evil-normal-state-map (kbd "C-k uw") #'js2r-unwrap)
  (define-key evil-normal-state-map (kbd "C-k wl") #'js2r-wrap-in-for-loop)
  (define-key evil-normal-state-map (kbd "C-k 3i") #'js2r-ternary-to-if)
  (define-key evil-normal-state-map (kbd "C-k lt") #'js2r-log-this)
  (define-key evil-normal-state-map (kbd "C-k dt") #'js2r-debug-this)
  (define-key evil-normal-state-map (kbd "C-k sl") #'js2r-forward-slurp)
  (define-key evil-normal-state-map (kbd "C-k ba") #'js2r-forward-barf)
  (define-key evil-normal-state-map (kbd "C-k k") #'js2r-kill)
  (define-key evil-normal-state-map (kbd "C-k <C-down>") #'js2r-move-line-down)
  (define-key evil-normal-state-map (kbd "C-k <C-up>") #'js2r-move-line-up)

  (define-key evil-insert-state-map (kbd "C-k") nil)
  (define-key evil-insert-state-map (kbd "C-k ee") #'js2r-expand-node-at-point)
  (define-key evil-insert-state-map (kbd "C-k cc") #'js2r-contract-node-at-point)
  (define-key evil-insert-state-map (kbd "C-k wi") #'js2r-wrap-buffer-in-iife)
  (define-key evil-insert-state-map (kbd "C-k ig") #'js2r-inject-global-in-iife)
  (define-key evil-insert-state-map (kbd "C-k ev") #'js2r-extract-var)
  (define-key evil-insert-state-map (kbd "C-k el") #'js2r-extract-let)
  (define-key evil-insert-state-map (kbd "C-k ec") #'js2r-extract-const)
  (define-key evil-insert-state-map (kbd "C-k iv") #'js2r-inline-var)
  (define-key evil-insert-state-map (kbd "C-k rv") #'js2r-rename-var)
  (define-key evil-insert-state-map (kbd "C-k vt") #'js2r-var-to-this)
  (define-key evil-insert-state-map (kbd "C-k ag") #'js2r-add-to-globals-annotation)
  (define-key evil-insert-state-map (kbd "C-k sv") #'js2r-split-var-declaration)
  (define-key evil-insert-state-map (kbd "C-k ss") #'js2r-split-string)
  (define-key evil-insert-state-map (kbd "C-k st") #'js2r-string-to-template)
  (define-key evil-insert-state-map (kbd "C-k ef") #'js2r-extract-function)
  (define-key evil-insert-state-map (kbd "C-k em") #'js2r-extract-method)
  (define-key evil-insert-state-map (kbd "C-k ip") #'js2r-introduce-parameter)
  (define-key evil-insert-state-map (kbd "C-k lp") #'js2r-localize-parameter)
  (define-key evil-insert-state-map (kbd "C-k tf") #'js2r-toggle-function-expression-and-declaration)
  (define-key evil-insert-state-map (kbd "C-k ta") #'js2r-toggle-arrow-function-and-expression)
  (define-key evil-insert-state-map (kbd "C-k ts") #'js2r-toggle-function-async)
  (define-key evil-insert-state-map (kbd "C-k ao") #'js2r-arguments-to-object)
  (define-key evil-insert-state-map (kbd "C-k uw") #'js2r-unwrap)
  (define-key evil-insert-state-map (kbd "C-k wl") #'js2r-wrap-in-for-loop)
  (define-key evil-insert-state-map (kbd "C-k 3i") #'js2r-ternary-to-if)
  (define-key evil-insert-state-map (kbd "C-k lt") #'js2r-log-this)
  (define-key evil-insert-state-map (kbd "C-k dt") #'js2r-debug-this)
  (define-key evil-insert-state-map (kbd "C-k sl") #'js2r-forward-slurp)
  (define-key evil-insert-state-map (kbd "C-k ba") #'js2r-forward-barf)
  (define-key evil-insert-state-map (kbd "C-k k") #'js2r-kill)
  (define-key evil-insert-state-map (kbd "C-k <C-down>") #'js2r-move-line-down)
  (define-key evil-insert-state-map (kbd "C-k <C-up>") #'js2r-move-line-up))

(add-hook 'js2-mode-hook #'js2-refactor-mode)
(add-hook 'js2-mode-hook #'js2r-setup-keys)
(add-hook 'js2-mode-hook #'emmet-mode)
(add-hook 'js2-mode-hook #'add-node-modules-path)
(add-hook 'js2-mode-hook #'flow-js2-mode)

;; (remove-hook 'rjsx-mode-hook #'js2-refactor-mode)
;; (remove-hook 'rjsx-mode-hook #'emmet-mode)
;; (remove-hook 'rjsx-mode-hook #'add-node-modules-path)

(provide 'hao-js)
;;; hao-js.el ends here
