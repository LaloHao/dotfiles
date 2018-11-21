;;; hao-editorconfig.el --- editorconfig config   -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'prelude-packages "~/.emacs.d/init.el")

(prelude-require-packages
 '(editorconfig))

(editorconfig-mode 1)

(provide 'hao-editorconfig)
;;; hao-editorconfig.el ends here
