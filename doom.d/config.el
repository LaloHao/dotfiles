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
(use-package! poly-ein)

(after! ox
  (use-package! ox-extra
    :config
    (ox-extras-activate '(latex-header-blocks ignore-headlines)))
  (use-package! ox-latex
    :config
    (setq org-latex-pdf-process
          '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
            "bibtex %b"
            "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
            "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
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

(defun with-spanish-locale (f &rest a)
  "Run function `F' using spanish `SYSTEM-TIME-LOCALE' with args `A'."
  (let ((system-time-locale "Spanish"))
    (apply f a)))

;;; Nice
;; (advice-add 'format-time-string :around #'with-spanish-locale)
;; (advice-remove 'format-time-string #'with-spanish-locale)

(use-package! org-journal
  :custom
  (org-journal-date-prefix "#+TITLE: ")
  (org-journal-time-prefix "* ")
  (org-journal-date-format "%A %d de %B %Y, semana %U") ;; See [[info:Elisp#Time Parsing][Elisp#Time Parsing]]
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-dir "~/journal/2021/")
  :config
  (advice-add 'org-journal-new-entry :around #'with-spanish-locale)
  (advice-add 'org-journal-new-date-entry :around #'with-spanish-locale)
  (advice-add 'org-journal-new-scheduled-entry :around #'with-spanish-locale))

(after! (orglink ol-info)
  (global-orglink-mode))

(after! (simple rainbow-mode)
  (add-hook 'fundamental-mode-hook #'rainbow-mode)
  (add-hook 'prog-mode-hook #'rainbow-mode)
  (add-hook 'text-mode-hook #'rainbow-mode))

(setq org-babel-default-header-args:jupyter-typescript
      '((:session . "ts")
        (:kernel . "tslab")))

(setq org-babel-default-header-args:jupyter-python
      '((:session . "jupyter")
        (:async . "yes")
        (:results . "value replace")
        (:kernel . "python")
        ))

(setq org-babel-default-header-args:jupyter
      '(;; (:session . "jupyter")
        ;; (:async . "no")
        (:results . "output replace")
        ;; (:kernel . "python")
        ))

(setq org-babel-default-header-args:python
      '(;; (:session . "python")
        ;; (:async . "no")
        (:results . "output replace")
        ;; (:kernel . "python")
        ))

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

(defun home-manager-switch ()
  (interactive)
  "Switch to current home-manager configuration"
  (async-shell-command "home-manager switch"))

(use-package! org-ref
  :config
  (setq
   org-ref-bibliography-notes     "~/notes/bibliography.org"
   org-ref-default-bibliography '("~/lib/references.bib")
   org-ref-pdf-directory          "~/lib/"))

(after! org-ref
  (use-package! doi-utils)
  (use-package! org-ref-pdf)
  (use-package! org-ref-url-utils)
  (use-package! x2bib))

(after! ispell
  :config
  (setq ispell-program-name "hunspell")
  (setq ispell-dictionary "en_US,es_MX")
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_US,es_MX"))

;; (after! lisp-mode
;;   :config
;;   (setq lisp-indent-offset 2))

(setq org-babel-default-header-args
    '((:noweb    . "no")
      (:session  . "none")
      (:results  . "replace drawer")
      ;; (:results  . "output replace drawer")
      ;; (:results  . "output verbatim replace drawer")
      (:exports  . "both")
      (:cache    . "no")
      (:hlines   . "no")
      (:tangle   . "no")
      ;; (:post     . )
       ))

(after! org-mode
  (use-package! org-html-themify)
  :hook (org-mode . org-html-themify-mode)
  :custom
  (org-html-themify-themes
   '((dark . modus-vivendi)
     (light . modus-operandi))))

(after! display-line-numbers
  :config
  (setq display-line-numbers-type 'relative)
  (set-face-attribute 'line-number nil
    :foreground "#777777")
  (set-face-attribute 'line-number-current-line nil
    :foreground "#dbe5e5"
    :weight 'bold))

(after! ob-exp ;; fucker deleted all my work when exporting
  :config ;; reran all the code
  (setq org-export-use-babel nil)
  (setq org-export-babel-evaluate nil))

(defvar indium-chrome--default-data-dir (expand-file-name "~/.indium/chrome")
  "Default directory used as Chrome data directory.")

(use-package! indium)
(use-package! websocket)
(use-package! ov)

;; (after! ivy
;;   (map! :leader :desc "Switch to last buffer"))

(map! :leader :desc "Delete other windows" :n "1" #'delete-other-windows)

;; (after! company
;;   (defun good-completion (&rest ignore)
;;     (completion-at-point))
;;   (advice-add 'company-complete-common :around 'good-completion))
(setq spanish-calendar-week-start-day 1
      spanish-calendar-day-name-array ["domingo" "lunes" "martes" "miércoles"
                                       "jueves" "viernes" "sábado"]
      spanish-calendar-month-name-array ["enero" "febrero" "marzo" "abril" "mayo"
                                         "junio" "julio" "agosto" "septiembre"
                                         "octubre" "noviembre" "diciembre"])

(use-package! graphviz-dot-mode)
(use-package! company-graphviz-dot)
(setq confirm-kill-emacs nil)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8)

(defun org-export-insert-shell-prompt (_backend)
  "Prefix a dollar sign ($) to exported shell commands.

Ignore `_BACKEND'."
  (org-babel-map-src-blocks nil         ; nil implies current buffer
    (let (;; capture macro-defined variables
          (lang lang)
          (beg-body beg-body)
          (end-body end-body)
          ;; other variables
          (shell-langs org-babel-shell-names)
          (prefix "$ ")
          (is-contd-from-prev-line nil)) ; t if prev line ends in '\'; nil otherwise
      (when (member lang shell-langs)
        (goto-char beg-body)
        (skip-chars-forward "\n\s" end-body) ; not sure why OP included '-'
        (while (< (point) end-body)
          (if (not is-contd-from-prev-line) ; skip prefix if continuing previous line
                (insert prefix))
          (end-of-line)
          (if (eq ?\\ (char-after (- (point) 1))) ; check if statement continues in next line
              (setq is-contd-from-prev-line t)
            (setq is-contd-from-prev-line nil))
          (skip-chars-forward "\n\s" end-body))))))

(after! ox
  (add-hook 'org-export-before-parsing-hook #'org-export-insert-shell-prompt))

(setq search-default-mode 'char-fold-to-regexp)
(setq char-fold-symmetric t)

(provide 'config)
;;; config.el ends here
