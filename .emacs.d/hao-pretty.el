;;; hao-pretty.el --- Pretty symbols                     -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun my-pretty-symbols ()
  "Símbolos chulos."
  (setq prettify-symbols-alist
        '(
          ;; ("define" . 8801)   ; ≡
          ("setf" . 8801)   ; ≡
          ("defun"   . 402)   ; ƒ
          ("lambda" . 955) ; λ
          ;; ("map" . 8614)   ; ↦
          ("mapc" . 8614)   ; ↦
          ("if" . 8594)    ; →
          ("when"   . 9633)   ; □
          ("cond"   . 9671)   ; ◇
          ("let" . 8866)   ; ⊢
          ("let*" . 8872)   ; ⊨
          ("car"   . 127032)   ; 🀸
          ("cdr"   . 127039)   ; 🀿
          ("cadr"   . 127026)   ; 🀲
          ("list->vector" . 8794)   ; ≚
          ("vector->list" . 8793)   ; ≙
          ("add-hook" . 9875)   ; ⚓
          ("for" . 5572)   ; ᗄ
          ("in" . 8712)   ; ∈
          ("and" . 8743)   ; ∧
          ("or" . 8744)   ; ∨
          ("loop" . 8635)   ; ↻
          ("defvar" . 8258)   ; ⁂
          ("last" . 937) ; Ω
          ))
  (prettify-symbols-mode))

(add-hook 'emacs-lisp-mode-hook 'my-pretty-symbols)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'gimp-mode-hook 'my-pretty-symbols)
(add-hook 'lisp-mode-hook 'my-pretty-symbols)
(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)

(font-lock-add-keywords
 'gimp-mode
 '(("\\<\\(and\\|or\\|not\\|list->vector\\|vector->list\\|cadr\\|car\\|defun\\)\\>" . 'font-lock-keyword-face)))

(font-lock-add-keywords
 'emacs-lisp-mode
 '(("\\<\\(and\\|or\\|not\\|list->vector\\|vector->list\\|cadr\\|car\\|defun\\)\\>" . 'font-lock-keyword-face)))

(defun fontify-glyph (item glyph)
  "Fontify ITEM with GLYPH."
  `((,item
     (0 font-lock-keyword-face t)
     (0 (prog1
            (compose-region (match-beginning 0)
                            (match-end 0)
                            ,glyph) nil)))))

(font-lock-add-keywords 'emacs-lisp-mode
                        (fontify-glyph "\\\\\\\\|" "+"))
(font-lock-add-keywords 'emacs-lisp-mode
                        (fontify-glyph "\\\\\\\\(" "("))
(font-lock-add-keywords 'emacs-lisp-mode
                        (fontify-glyph "\\\\\\\\)" ")"))
(font-lock-add-keywords 'emacs-lisp-mode
                        (fontify-glyph "\\\\\\\\<" "<"))
(font-lock-add-keywords 'emacs-lisp-mode
                        (fontify-glyph "\\\\\\\\>" ">"))



(provide 'hao-pretty)
;;; hao-pretty.el ends here
