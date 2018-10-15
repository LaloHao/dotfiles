;;; hao-js.el --- javascript development config   -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'prelude-packages "~/.emacs.d/init.el")

(prelude-require-packages
 '(emmet-mode))

(require 'emmet-mode)

(defun php-emmet-config ()
  "Configure php with emmet-mode."
  (setq emmet-expand-jsx-className? nil)
  (setq emmet-self-closing-tag-style " /")
  (setq web-mode-markup-indent-offset 2)
  (emmet-mode))

(add-hook 'web-mode-hook #'php-emmet-config)

(delete '("\\.php\\'" . php-mode) auto-mode-alist)
(add-to-list 'auto-mode-alist '("\\.php$" . web-mode))

(provide 'hao-js)
;;; hao-js.el ends here
