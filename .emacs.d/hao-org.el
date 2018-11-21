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

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (shell . t)))

(setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted"))
      org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(setq
 ieeetran-class
 '("IEEEtran" "\\documentclass[11pt]{/home/hao/dev/org/latex-plantilla/IEEEtran}
               \\usepackage[spanish,mexico]{babel}
               \\addto\\captionsspanish{\\renewcommand{\\contentsname}{Contenido}}"
   ("\\section{%s}" . "\\section*{%s}")
   ("\\subsection{%s}" . "\\subsection*{%s}")
   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
   ("\\paragraph{%s}" . "\\paragraph*{%s}")
   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes ieeetran-class t)

(defun org-remove-headlines (backend)
  "Remove headlines with :no_title: tag."
  (org-map-entries (lambda () (let ((beg (point)))
                           (outline-next-visible-heading 1)
                           (backward-char)
                           (delete-region beg (point))))
                   "no_export" tree)
  (org-map-entries (lambda () (delete-region (point-at-bol) (point-at-eol)))
                   "no_title"))

(add-hook 'org-export-before-processing-hook #'org-remove-headlines)
(remove-hook 'org-export-before-processing-hook #'org-remove-headlines)

(provide 'hao-org)
;;; hao-org.el ends here
