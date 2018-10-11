;;; hao-init.el --- Init
;;; Commentary:
;;; Code:

(require 'prelude-packages "~/.emacs.d/init.el")
(require 'hao-utils (expand-file-name "hao-utils.el"))

(prelude-require-packages
 '(transpose-mark ssh slime
   epc linum-relative google-translate
   help-fns+ stumpwm-mode
   aggressive-indent restclient ob-restclient))

(set-keys
 '(("C-M-t" transpose-mark)
   ("C-c C-v"  comment-or-uncomment-region)
   ("C-x M-d" prelude-duplicate-and-comment-current-line-or-region)

   ("M-[" insert-pair)
   ("C-M-{" insert-pair)
   ("\"" insert-pair)
   ("C-*" "**\C-b")

   ("<f7>" linum-relative-toggle)
   ("<f8>" linum-mode)

   ("C-c l" org-store-link)
   ("C-c a" org-agenda)
   ("C-c z" org-capture)
   ("C-c b" org-iswitchb)
   ("C-M-g" org-plot/gnuplot)

   ("M-$" flyspell-buffer)

   ("<menu>" helm-M-x)

   ("C-x C-8" toggle-window-split)
   ("C-x C-1" delete-other-windows)
   ("C-x C-2" split-window-below)
   ("C-x C-3" split-window-right)))

(defvar flyspell-default-dictionary)
(setq flyspell-default-dictionary "es")

(require 'prelude-helm-everywhere)
(key-chord-mode nil)

(scroll-bar-mode)

(setq select-enable-primary t)
(setq select-enable-clipboard t)

(yas-global-mode t)
(yas-reload-all)

(linum-relative-global-mode)

(provide 'hao-init)
;;; hao-init.el ends here
