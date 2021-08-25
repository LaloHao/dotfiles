;;; config.el --- Doom Emacs configuration -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021
;;
;; Author:  <https://github.com/hao>
;; Maintainer:  <hao@emma>
;; Created: February 23, 2021
;; Modified: February 23, 2021
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/hao/config
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Doom Emacs configuration
;;
;;; Code:

;; Fallback to substitute command using key `s'
(after! evil-snipe
  (evil-snipe-mode -1))

(setq user-full-name "Eduardo V."
      user-mail-address "lalohao@gmail.com")

(add-hook 'org-mode-hook (lambda () (electric-indent-local-mode -1)))

(setq-default web-mode-sql-indent-offset 2)
(setq-default web-mode-attr-indent-offset 2)
(setq-default web-mode-markup-indent-offset 2)
(setq-default web-mode-code-indent-offset 2)
(setq-default web-mode-indent-style 2)
(setq-default web-mode-css-indent-offset 2)

(setq-default js-indent-level 2)
(setq-default js-jsx-indent-level 2)
(setq-default sh-indentation 2)
(setq-default typescript-indent-level 2)

(use-package! company-nixos-options)
(use-package! nix-sandbox)
(use-package! nixos-options)
(use-package! graphql-mode)

(after! ox
  (use-package! ox-extra
    :config
    (ox-extras-activate '(latex-header-blocks ignore-headlines)))
  (use-package! ox-latex
    :config
    (setq org-latex-pdf-process
          '("pdflatex -interaction nonstopmode -output-directory %o %f"
            "bibtex %b"
            "pdflatex -interaction nonstopmode -output-directory %o %f"
            "pdflatex -interaction nonstopmode -output-directory %o %f"))
    (setq org-latex-logfiles-extensions
          (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "bbl" "xmpi" "run.xml" "bcf" "acn" "acr" "alg" "glg" "gls" "ist")))
    (ox-extras-activate '(latex-header-blocks ignore-headlines))) )

;; (use-package! happy-mode
;;   :mode "\\.y$"
;;   :commands happy-mode)

(setq fancy-splash-image "~/Pictures/black-hole.png")
(setq +doom-dashboard-banner-padding '(4 . 0))

;;; This seems to not work at all (compared to edit-server)
;; (use-package! atomic-chrome)
;; (after! atomic-chrome
;;   (atomic-chrome-start-server))

;;; edit-server requires at least 1 frame already open to work
(use-package! edit-server
  :config
  (setq edit-server-new-frame-alist nil)
  (add-hook 'edit-server-done-hook '(lambda () (kill-ring-save (point-min) (point-max))))
  (edit-server-start))

(use-package! org-journal
  :custom
  (org-journal-date-prefix "#+TITLE: ")
  (org-journal-time-prefix "* ")
  (org-journal-date-format "%e %b %d de %mmmm %Y")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-dir "~/journal/2021/"))

(setq org-babel-default-header-args:jupyter-typescript
      '((:session . "ts")
        (:kernel . "tslab")))

(after! org-src
  (dolist (lang '(python jupyter))
    (cl-pushnew (cons (format "jupyter-%s" lang) lang)
                org-src-lang-modes :key #'car))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (typescript . t)
     (python . t)
     (jupyter . t))))

(setq python-indent 2)
(setq python-indent-offset 2)

(provide 'config)
;;; config.el ends here
