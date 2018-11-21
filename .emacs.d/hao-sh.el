;;; hao-sh.el --- shell development config   -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'prelude-packages "~/.emacs.d/init.el")

(prelude-require-packages
 '(exec-path-from-shell))

(exec-path-from-shell-initialize)

(provide 'hao-sh)
;;; hao-sh.el ends here
