;;; snippets.el --- Custom snippets                  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021  Eduardo V.
;;
;; Author: Eduardo V. <lalohao@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:

;;

;;; Code:

(defvar snippets-dir
  (file-name-directory
   (cond (load-in-progress load-file-name)
         ((bound-and-true-p byte-compile-current-file)
          byte-compile-current-file)
         (buffer-file-name)))
  "The base directory of the snippets library.")

;;;###autoload
(defun snippets-initialize ()
  "Add `snippets-dir' to `yas-snippet-dirs', replacing the default
yasnippet directory."
  (setq yas-wrap-around-region nil)
  (add-to-list 'yas-snippet-dirs 'snippets-dir)
  (yas-load-directory snippets-dir t))

;;;###autoload
(eval-after-load 'yasnippet
  (lambda () (snippets-initialize)))

(provide 'snippets)
;;; snippets.el ends here
