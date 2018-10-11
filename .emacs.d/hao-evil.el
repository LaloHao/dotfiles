;;; hao-evil.el --- Evil config                   -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'prelude-packages "~/.emacs.d/init.el")

(prelude-require-packages
 '(evil-tutor
   evil-vimish-fold evil-surround
   evil-paredit evil-numbers evil-nerd-commenter
   evil-extra-operator
   evil-org org-page org-evil
   evil-magit
   hydra))

(require 'evil)
(require 'evil-org)
(require 'evil-magit)
(require 'evil-numbers)
(require 'evil-surround)
(require 'evil-nerd-commenter)
(require 'evil-extra-operator)

(evil-mode)
(global-evil-surround-mode 1)
(evil-vimish-fold-mode 1)
(evil-paredit-mode)

(define-key evil-normal-state-map (kbd "SPC") nil)

(defun hao/ace-1 ()
   (interactive)
   (ace-window 1)
   (add-hook 'ace-window-end-once-hook
       'hydra-window/body))

(defun hao/split-vertical ()
  (interactive)
  (split-window-right)
  (windmove-right))

(defun hao/split-horizontal ()
  (interactive)
  (split-window-below)
  (windmove-down))

(defun hao/switch ()
  (interactive)
  (ace-window 4)
  (add-hook 'ace-window-end-once-hook
      'hydra-window/body))

(defun hao/delete ()
  (interactive)
  (ace-window 16)
  (add-hook 'ace-window-end-once-hook
      'hydra-window/body))

(defun hao/projectile ()
  (interactive)
  (projectile-command-map))
(defun hao/find-file ()
  (interactive)
  (helm-projectile-find-file))

(defhydra hydra-window ()
   "
Movement^^        ^Split^         ^Switch^
----------------------------------------------------------------
_h_ ←       	_v_ertical    	_b_uffer
_j_ ↓        	_x_ horizontal	_f_ind files
_k_ ↑        	_z_ undo      	_a_ce 1
_l_ →        	_Z_ reset      	_S_wap
_F_ollow		_D_lt Other   	_s_ave
_q_ cancel	_1_only this	_d_elete
_p_rojectile    ma_g_it status
"
   ("h" windmove-left )
   ("j" windmove-down )
   ("k" windmove-up )
   ("l" windmove-right )
   ("b" helm-mini)
   ("f" hao/find-file)
   ("F" follow-mode)
   ("a" hao/ace-1)
   ("v" hao/split-vertical)
   ("x" hao/split-horizontal)
   ("S" hao/switch)
   ("s" save-buffer)
   ("d" delete-window)
   ("D" hao/delete)
   ("1" delete-other-windows)
   ("i" ace-maximize-window)
   ("z" winner-undo)
   ("Z" winner-redo)
   ("g" magit-status)
   ("SPC" crux-switch-to-previous-buffer)
   ("q" nil))

(define-key evil-normal-state-map (kbd "ñ p") 'projectile-command-map)
(define-key evil-normal-state-map (kbd "ñ h") 'helm-command-prefix)
(define-key evil-normal-state-map (kbd "SPC") 'hydra-window/body)

(define-key evil-normal-state-map (kbd "C-e") 'eval-last-sexp)
(define-key evil-normal-state-map (kbd "z i") 'ispell-word)
(define-key evil-normal-state-map (kbd "S-SPC") 'avy-goto-char)

(define-key evil-normal-state-map (kbd "+") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "-") 'evil-numbers/dec-at-pt)
(define-key evil-normal-state-map (kbd "<kp-add>") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "<kp-subtract>") 'evil-numbers/dec-at-pt)

(define-key evil-normal-state-map (kbd "C-<tab>") 'vimish-fold)
(define-key evil-normal-state-map (kbd "M-n") 'vimish-fold-next-fold)
(define-key evil-normal-state-map (kbd "M-p") 'vimish-fold-previous-fold)

(define-key evil-insert-state-map (kbd "ñ") 'evil-execute-in-normal-state)

(evilnc-default-hotkeys)

(global-evil-extra-operator-mode 1)

(define-key evil-normal-state-map (kbd "0") 'evil-indent)
(define-key evil-normal-state-map (kbd "1") 'evil-shell-command)
(define-key evil-normal-state-map (kbd "2") 'evil-use-register)
(define-key evil-normal-state-map (kbd "3") 'evil-search-word-backward)
(define-key evil-normal-state-map (kbd "4") 'evil-end-of-line)
(define-key evil-normal-state-map (kbd "5") 'evil-jump-item)
(define-key evil-normal-state-map (kbd "6") 'evil-ex-repeat-substitute)
(define-key evil-normal-state-map (kbd "7") 'evil-search-forward)
(define-key evil-normal-state-map (kbd "8") 'evil-backward-sentence-begin)
(define-key evil-normal-state-map (kbd "9") 'evil-forward-sentence-begin)

(define-key evil-insert-state-map (kbd "<") "|")

(provide 'hao-evil)
;;; hao-evil.el ends here
