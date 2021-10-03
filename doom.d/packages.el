;;; packages.el --- Custom configuration packages -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Eduardo V.
;;
;; Author: Eduardo V. <https://github.com/hao>
;; Maintainer: Eduardo V. <lalohao@gmail.com>
;; Created: March 16, 2021
;; Modified: March 16, 2021
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/hao/packages
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Custom configuration packages
;;
;;; Code:

(cl-flet
  ((recipe (name) (eval
    `(package! ,name
       :recipe (:host github :repo "travisbhartwell/nix-emacs")))))

  (recipe 'company-nixos-options)
  (recipe 'helm-nixos-options)
  (recipe 'nix-sandbox)
  (recipe 'nixos-options))

(package! graphql-mode)

(package! happy-mode)
(package! org-plus-contrib :recipe (:host github :repo "emacs-straight/org"))

(package! edit-server)

(package! org-ref)

(package! org-html-themify)
(package! hexrgb)
(package! htmlize)
(package! dash)
(package! s)
(package! indium)
(package! websocket)
(package! ov)
(package! orglink)
(package! graphviz-dot-mode)

(package! visual-regexp)
(package! visual-regexp-steroids)
(package! cl-format)
(package! snippets)

(provide 'packages)
;;; packages.el ends here
