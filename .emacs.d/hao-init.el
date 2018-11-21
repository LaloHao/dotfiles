;;; hao-init.el --- Init
;;; Commentary:
;;; Code:

(require 'prelude-packages "~/.emacs.d/init.el")

(prelude-require-packages
 '(transpose-mark ssh slime
   epc linum-relative google-translate
   stumpwm-mode
   aggressive-indent restclient ob-restclient
   yasnippet))

(defun toggle-window-split ()
  "Switch window layout from vertical to horizontal or viceversa."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
          (next-win-buffer (window-buffer (next-window)))
          (this-win-edges (window-edges (selected-window)))
          (next-win-edges (window-edges (next-window)))
          (this-win-2nd (not (and (<= (car this-win-edges)
                                    (car next-win-edges))
                                (<= (cadr this-win-edges)
                                    (cadr next-win-edges)))))
          (splitter
           (if (= (car this-win-edges)
                 (car (window-edges (next-window))))
               'split-window-horizontally
             'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun load-custom-file (file)
  "Load FILE from personal directory."
  (let ((file (concat "~/.emacs.d/personal/" file)))
    (if (file-exists-p file)
        (load-file file)
      (message (format "%s does not exists!" file)))))

(defun load-custom-file-list (file-list)
  "Load FILE-LIST from personal directory."
  (mapc (lambda (x) (load-custom-file x)) file-list))

(defun require-several (packages)
  "Require PACKAGES."
  (mapc (lambda (pkg) (require pkg)) packages))

(defun set-key (key function)
  "Forcefully set global KEY to execute given FUNCTION."
  (global-unset-key (kbd key))
  (global-set-key (kbd key) function))

(defun set-keys (key-pairs)
  "Forcefully `set-key' for each key KEY-PAIRS.

Example:
\(set-keys '((\"C-1\" delete-other-windows)
            (\"C-2\" split-window-below)
            (\"C-3\" split-window-right)))

Is equivalent to:
\(set-key \"C-1\" 'delete-other-windows)
\(set-key \"C-2\" 'split-window-below)
\(set-key \"C-3\" 'split-window-right)"
  (mapc (lambda (key-pair)
       (apply
        (lambda (key function)
          (set-key key function))
        (car key-pair)
        (cdr key-pair)))
     key-pairs))

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

(require 'prelude-helm-everywhere)

(scroll-bar-mode)

(setq select-enable-primary t)
(setq select-enable-clipboard t)

(yas-global-mode t)
(yas-reload-all)

(linum-relative-global-mode)

(provide 'hao-init)
;;; hao-init.el ends here
