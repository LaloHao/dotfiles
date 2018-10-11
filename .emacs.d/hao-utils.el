;;; hao-utils.el --- Utils                           -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

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

(provide 'hao-utils)
;;; hao-utils.el ends here
