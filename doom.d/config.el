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

(setq user-full-name "Eduardo V."
      user-mail-address "lalohao@gmail.com")

(add-hook 'org-mode-hook (lambda () (electric-indent-local-mode -1)))

(defalias 'bind 'destructuring-bind)

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

;; Two ways of defining the same function
;; See [1] and [2]
;;
;; [1] (cl-defun cons* (list &key (null t))
;;       (and (consp list)
;;            (consp (rest list))
;;            (bind (first second . rest) list
;;             (bind element (cons first second)
;;               (cond
;;                (second     (cons element (cons* rest)))
;;                (null       (cons element (cons* rest)))
;;                (t                        (cons* rest)))))))
;;
;; [2] (cl-defun cons* (list &key (null t))
;;       (and (consp list)
;;            (consp (rest list))
;;            (bind (first second . rest) list
;;             (bind element (cons first second)
;;               (if (-all? #'null (list second (not null)))
;;                   (cons* rest)
;;                 (cons element (cons* rest :null null)))))))

;; Decided to leave it simple and filter manually
;; (-filter #'rest (cons* '(k1 v1 k2 nil)))
(defun cons* (list)
  (and (consp list)
       (consp (rest list))
       (cons (cons (first list) (second list))
             (cons* (rest (rest list))))))

;; (cons* '(k1 v1 k2 v2))

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
  (add-hook 'edit-server-done-hook '#(lambda () (kill-ring-save (point-min) (point-max))))
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

(progn ;; Use global links in emacs
  (use-package! orglink)
  (use-package! ol-info)

  (after! (orglink ol-info)
    (global-orglink-mode)))

(after! (simple rainbow-mode)
  (add-hook 'fundamental-mode-hook #'rainbow-mode)
  (add-hook 'prog-mode-hook #'rainbow-mode)
  (add-hook 'text-mode-hook #'rainbow-mode))

(defun pairp (pair)
  "Return `t' if `PAIR' is a dotted pair, otherwise `NIL'.

Example:
  (pairp '1) => nil
  (pairp '(1)) => nil
  (pairp '(1 2)) => nil

  (pairp '(1 . 2)) => t

  (pairp '(1 . ())) => nil

Notes:
  (pairp '(1 . (2 3))) => nil
 ^ Since '(1 . (2 3)) actually means '(1 2 3)

  (pairp '(1 . (2 . 3))) => nil
 ^ Since '(1 . (2 . 3)) actually means '(1 2 . 3)
"
  (and (listp pair) (not (listp (cdr pair)))))

(defun org-babel-add-language (lang &optional load)
  "Add the language `LANG' to `org-babel'.

Optionally if `LOAD' then loads the language now."
  (let ((lang (cons lang t)))
    (add-to-list 'org-babel-load-languages lang)
    (when load
      (org-babel-load-language lang)))
  lang)

(defun org-babel-load-language (lang)
  "Load the language `LANG' in `org-babel'."
  (let ((lang (cons lang t)))
    (org-babel-do-load-languages
     'org-babel-load-languages
     (append org-babel-load-languages (list lang)))))

(use-package! cl-format)

(defun org-babel--format-header-args (args)
  "Returns a string containing formatted `ARGS' for use in SRC blocks.

Example
  If FORMATTED-ARGS is the result of evaluating
    (org-babel-format-header-args org-babel-default-header-args:emacs-lisp)

  Then you can replace it and obtain a valid header as:

  #+begin_src elisp FORMATTED-ARGS
  ...
  #+end_src
"
  (cl-format nil "~{~{~s ~a~}~^ ~}"  args))

(defun org-babel--default-header-args-symbol (&optional lang)
  "`LANG'."
  (let* ((lang-string (cl-format nil "~@[~a~]" lang))
         (alias-for-lang (alist-get lang-string org-src-lang-modes nil nil #'equal))
         (format-string "~a~^~@[:~*~#[~:;~^~:[~:*~;~]~]~:*~a~]"))
    (cl-format nil format-string "org-babel-default-header-args" lang alias-for-lang)))

;; (let ((s "babel~^~@[:~*~#[~:;~^~:[~:*~;~]~]~]~:*~a"))
;;   (values
;;    (cl-format nil s)
;;    (cl-format nil s 'left)
;;    (cl-format nil s 'left 'right)
;;    (cl-format nil s 'left nil)))

;; (org-babel--default-header-args-symbol)
;; (org-babel--default-header-args-symbol 'elisp)
;; (org-babel--default-header-args-symbol 'elis)

(defun org-babel-default-header-args-symbol (&optional lang)
  "Get `LANG'."
  (intern-soft (org-babel--default-header-args-symbol lang)))

;; (defun org-babel--default-header-args (&optional lang)
;;   "`LANG'."
;;   (symbol-value (org-babel-default-header-args-symbol lang)))

;; (defun org-babel-default-header-args (&optional lang)
;;   "`LANG'."
;;   (org-babel-combine-header-arg-lists
;;    (org-babel--default-header-args)
;;    (and lang (org-babel--default-header-args lang))))

(defmacro org-babel-default-header-args (&optional lang)
  "`LANG'"
  `(symbol-value `,(org-babel-default-header-args-symbol ,lang)))

(use-package! ob-shell)

;; :prologue "exec 2>&1" :epilogue ":"
(setf (org-babel-default-header-args 'ash)
      '((:prologue . "exec 2>&1")
        (:epilogue . ":")))

(setf (org-babel-default-header-args 'sh)
      (cons* `(:prologue "exec 2>&1" :epilogue ":")))

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

(defvar org-babel-format-header-args '(:exports)
  "Default header args to insert on expanded org src headers.")

(defun org-babel-format-header-args (lang)
  "`LANG'."
  (let ((h (org-babel-default-header-args lang))
        (as org-babel-format-header-args)
        (-compare-fn (-lambda ((h) a) (equalp a h))))
    (org-babel--format-header-args (-intersection h as))))

;; (org-babel-format-header-args 'elisp)
;; (org-babel-format-header-args 'shell)

(defun with-cl-format (f s &rest a)
  (if (stringp s)
      (apply f s a)
    (apply #'cl-format s a)))

(advice-add 'format :around 'with-cl-format)

;; (advice-remove 'format 'with-cl-format)

;; (values
;;  (format "%s" "test")
;;  (format nil "~a" "test"))

(defun yas-choose-org-src ()
  "."
  (let* ((lang (yas-choose-value (org-babel-load-languages)))
         (header-args (org-babel-format-header-args lang)))
    (cl-format nil "~a ~a" lang header-args)))

(cl-defun org-babel-load-languages (&optional (do-load nil))
  "Prints `org-babel-load-languages' and load them if `DO-LOAD'."
  (prog1 (-map #'car org-babel-load-languages)
    (when do-load
      (org-babel-do-load-languages
       'org-babel-load-languages
       org-babel-load-languages))))

;; (org-babel-load-languages)

(after! org-src
  (org-babel-add-language 'emacs-lisp)
  (org-babel-add-language 'typescript)
  (org-babel-add-language 'python)
  (org-babel-add-language 'jupyter)
  (org-babel-load-languages)

  (defalias 'org-babel-alias-language 'org-babel-make-language-alias)
  (org-babel-alias-language "jupyter-python" "python")
  (org-babel-alias-language "jupyter-jupyter" "jupyter"))


;; (after! org-src
;;   (dolist (lang '(python jupyter))
;;     (cl-pushnew (cons (format "jupyter-%s" lang) lang)
;;                 org-src-lang-modes :key #'car))
;;   (org-babel-do-load-languages
;;    'org-babel-load-languages
;;    (append org-babel-load-languages
;;     '((emacs-lisp . t)
;;      (typescript . t)
;;      (python . t)
;;      (jupyter . t)))))

(setq python-indent 2)
(setq python-indent-offset 2)

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
    '((:results  . "replace verbatim")
      (:exports  . "both")))

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

(use-package! alert
  :config
  (alert-add-rule :status '(buried)))

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
(map! :desc "Async shell command" :n "M-!" #'async-shell-command)

;; (after! company
;;   (defun good-completion (&rest ignore)
;;     (completion-at-point))
;;   (advice-add 'company-complete-common :around 'good-completion))
(setq spanish-calendar-week-start-day 1
      spanish-calendar-day-name-array ["domingo" "lunes" "martes" "miercoles"
                                       "jueves" "viernes" "sabado"]
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

(use-package! visual-regexp)
(use-package! visual-regexp-steroids)

;; (after! typescript-mode
;;   (defun ts-compile-current-file (&optional dir)
;;     (let* ((ts (file-name-nondirectory buffer-file-name))
;;            (js (format "%s.js" (file-name-sans-extension ts))))
;;       (format "tsc --outFile %s %s" js ts)))

;;   (defun ts-compile-functions-setup ()
;;     (add-to-list 'counsel-compile-local-builds 'ts-compile-current-file))

;;   (add-hook! 'typescript-mode-hook #'ts-compile-functions-setup))

;; https://books.google.com.mx/books?id=9apQfCRhvm0C&pg=PA230&lpg=PA230&dq=lisp+format+separate+space&source=bl&ots=UgJQLHr_de&sig=ACfU3U0xTSZf2WUoRvr2Npo-BI8I5AEuHw&hl=en&sa=X&ved=2ahUKEwj4gq2slKvzAhUEk2oFHYMKAd8Q6AF6BAgZEAM#v=onepage&q=lisp%20format%20separate%20space&f=false
;; (format nil "|~{~<a~%b~,33:;~2d ~>~}|" (loop for x below 100 collect x))

;;; handle "fd" as if pressing Escape
;;; or a better way:
;; https://stackoverflow.com/questions/63322146/how-to-map-jh-and-fd-to-esc-in-doom-emacs
;;; or globally in the window manager (?)
;; TODO: stop developing every idea
(after! key-chord
  :config
  (key-chord-mode t)
  (key-chord-define-global "fd" 'evil-normal-state))

;;; untested
;;; references
;; https://emacs.stackexchange.com/questions/4217/how-to-modify-a-buffer-without-undo-noticing
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Maintaining-Undo.html
;; https://emacs.stackexchange.com/questions/7558/how-to-collapse-undo-history
(defmacro save-undo-excursion (&rest body)
  (declare (indent 0) (debug t))
  (let ((ul (make-symbol "undo-list"))
        (ut (make-symbol "undo-tree")))
    `(let ((,ul (buffer-undo-list))
           (,ut (buffer-undo-tree)))
       (buffer-disable-undo)
       (unwind-protect (progn ,@body)
         (setq buffer-undo-list ,ul
               buffer-undo-tree ,ut)
         (buffer-enable-undo)))))

(provide 'config)
;;; config.el ends here
