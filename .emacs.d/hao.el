;;; hao.el --- Hao                                   -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; (require 'hao-init (expand-file-name "hao-init"))
(load (expand-file-name "hao-fence"))
(load (expand-file-name "hao-git"))
(load (expand-file-name "hao-init"))
(load (expand-file-name "hao-js"))
(load (expand-file-name "hao-org"))
(load (expand-file-name "hao-pretty"))
(load (expand-file-name "hao-translate"))
(load (expand-file-name "hao-ui"))
(load (expand-file-name "hao-utils"))
(load (expand-file-name "hao-evil"))

;; ;;;###autoload
;; (defun hao-init ()
;;   "Initialize configuration."
;;   (interactive)
;;   (hao-initialize))

(provide 'hao)
;;; hao.el ends here
