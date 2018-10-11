;;; hao-ui.el --- UI                                 -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq frame-title-format
      '(""
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(provide 'hao-ui)
;;; hao-ui.el ends here
