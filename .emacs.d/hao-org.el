;;; hao-org.el --- org-mode configuration         -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'prelude-packages "~/.emacs.d/init.el")

(prelude-require-packages
 '(org-ref helm-bibtex
   evil-org org-page org-evil
   org-attach-screenshot
   ;; ob-spice
   web-mode org-trello))

(require 'org-ref)
(require 'org-ref-pdf)
(require 'org-ref-url-utils)

(eval-after-load "org"
  '(require 'ox-gfm nil t))

(if (require 'toc-org nil t)
    (add-hook 'org-mode-hook 'toc-org-enable)
  (warn "toc-org not found"))


(provide 'hao-org)
;;; hao-org.el ends here
