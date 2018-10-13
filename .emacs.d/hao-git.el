;;; hao-git.el --- Git configuration              -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'prelude-packages "~/.emacs.d/init.el")

(prelude-require-packages
 '(magit-gitflow
   ansi-color
   magit-gh-pulls))

(require 'magit-gitflow)
(require 'ansi-color)
(require 'magit-gh-pulls)

(defun color-buffer (proc &rest args)
  "Color PROC buffer."
  (interactive)
  (with-current-buffer (process-buffer proc)
    (read-only-mode -1)
    (ansi-color-apply-on-region (point-min) (point-max))
    (read-only-mode 1)))

(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)
(advice-add 'magit-process-filter :after #'color-buffer)
(add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)

(defun turn-off-linum ()
  ""
  (linum-relative-mode -1))

(add-hook 'magit-mode-hook 'turn-off-linum)

(provide 'hao-git)
;;; hao-git.el ends here
