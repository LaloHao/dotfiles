;;; hao-translate.el --- Translate                       -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'prelude-packages "~/.emacs.d/init.el")

(prelude-require-packages
 '(google-translate))

(require 'google-translate)

(defvar *google-translation-file* "~/.translated")
(defvar *google-translation-buffer* "*Google Translate*")
(setq google-translate-output-destination 'echo-area)
;; (setq google-translate-output-destination nil)

(defun google-translate-to-file (from to text)
  "Translate TEXT FROM language TO language using google."
  (interactive)
  (switch-to-buffer *google-translation-buffer*)
  (insert (google-translate-translate from to text))
  (write-file *google-translation-file*)
  (kill-buffer))

;; (google-translate-to-file "auto" "es" "test")
;; (google-translate-translate "auto" "es" "test")

(provide 'hao-translate)
;;; hao-translate.el ends here
