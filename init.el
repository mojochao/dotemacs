;; This init file is used to bootstrap Emacs for loading its
;; configuration file from an org file.

;; Initially, turn off all the visual trim.  Turn it back on in your
;; org configuration file as you like.
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'tooltip-mode) (tooltip-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Next, configure package management
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(setq use-package-verbose nil)

;; Load org mode.
(use-package org
  :ensure t
  :config
  (progn
    (setq org-startup-indented nil)
    ))

;; Finally, load your org config file.
(org-babel-load-file (expand-file-name "mojochao.org" user-emacs-directory))
