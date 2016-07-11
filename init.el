;;; init.el --- my personal Emacs configuration

;; Version: 0.1.0
;; Author: Allen Gooch <allen.gooch@gmail.com>
;; Url: https://github.com/mojochao/npm-mode
;; Keywords: configuration
;; Package-Requires: ((emacs "24.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This package provides my personal Emacs configuration.

;;; Code:

;;-------------------------------------------------------------------------
;; initial configuration

;; Turn off mouse interface early in startup to avoid momentary display
(if (and (not (eq system-type 'darwin)) (fboundp 'menu-bar-mode)) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'tooltip-mode) (tooltip-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please or scratch buffer content
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; configure customization data to be persisted outside of the .emacs file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; configure backups to be persisted outside of source directories
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; configure gnus configuration in external file
(setq gnus-init-file "~/.emacs.d/.gnus.el")

;; show filename in window frame
(setq frame-title-format "%f")

;; no truncation of messages in 'echo' area
(setq eval-expression-print-length nil)

(setq column-number-mode t)             ; show column numbers in modeline
(setq line-number-mode t)               ; show line numbers in modeline
(delete-selection-mode 1)               ; delete marked region and replace with new content
(fset 'yes-or-no-p 'y-or-n-p)           ; use shortcuts for all yes/no prompts

;; you don't shut down emacs, do ya?!
(require 'server)
(unless (server-running-p)
  (server-start))

(prefer-coding-system 'utf-8)

(when (display-graphic-p)
  ;; smooth scrolling with trackpad or mouse
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
  (setq mouse-wheel-progressive-speed nil)
  (setq scroll-step 1)
  (put 'scroll-left 'disabled nil)
  (put 'scroll-right 'disabled nil)
  ;; (global-set-key (kbd "<mouse-6>") 'scroll-right)
  ;; (global-set-key (kbd "<mouse-7>") 'scroll-left)
  (global-set-key [wheel-right] 'scroll-left)
  (global-set-key [wheel-left] 'scroll-right))


;;-------------------------------------------------------------------------
;; package configuration

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar common-packages '(docker
                          dockerfile-mode
                          elpy
                          emmet-mode
                          exec-path-from-shell
                          expand-region
                          flycheck
                          gitignore-mode
                          helm
                          helm-ag
                          helm-flycheck
                          helm-mt
                          helm-projectile
                          htmlize
                          idea-darkula-theme
                          js2-mode
                          json-mode
                          lorem-ipsum
                          magit
                          markdown-mode
                          multi-term
                          multiple-cursors
                          nvm
                          ob-restclient
                          plantuml-mode
                          projectile
                          restclient
                          smart-mode-line
                          web-mode
                          yaml-mode
                          yasnippet)
  "A list of common external packages to ensure are installed at launch.")

(defvar macos-packages '(reveal-in-osx-finder)
  "A list of Mac OS packages to ensure are installed at launch.")

(dolist (package common-packages)
  (when (not (package-installed-p package))
    (package-install package)))

(when (memq window-system '(mac ns))
  (dolist (package macos-packages)
    (when (not (package-installed-p package))
      (package-install package))))


;;------------------------------------------------------------------------------
;; appearance

(load-theme 'idea-darkula)

(require 'smart-mode-line)
(setq sml/no-confirm-load-theme t)
(sml/setup)
(setq sml/theme 'dark)


;;------------------------------------------------------------------------------
;; interaction

(define-key global-map [s-return] 'toggle-frame-fullscreen) ;; same as iTerm
(define-key global-map [home] 'beginning-of-line)
(define-key global-map [end] 'end-of-line)

(require 'recentf)
(recentf-mode t)
(setq recentf-max-saved-items 50)

(defun open-line-below ()
  "Open a line below current line with point at indent position."
  (interactive)
  (move-end-of-line 1)
  (newline)
  (indent-according-to-mode))

(defun open-line-above ()
  "Open a line above current line with point at indent position."
  (interactive)
  (forward-line -1)
  (open-line-below))

(define-key global-map [C-return] 'open-line-below)
(define-key global-map [C-S-return] 'open-line-above)

(defun sudo-save ()
  "Save the current buffer as sudo."
  (interactive)
  (if (not buffer-file-name)
      (write-file (concat "/sudo:root@localhost:" (ido-read-file-name "File:")))
    (write-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun find-file-line-at-point()
  "Find file at point and jump to line number delimited by colon. (main.cpp:23)"
  (interactive)
  (let ((line-num 0))
    (save-excursion
      (search-forward-regexp "[^ ]:" (point-max) t)
      (if (looking-at "[0-9]+")
          (setq line-num (string-to-number (buffer-substring (match-beginning 0) (match-end 0))))))
    (find-file (ffap-guesser))
    (if (not (equal line-num 0))
        (forward-line line-num))))

(define-key global-map [f12] 'find-file-line-at-point)
(define-key global-map [S-f12] 'find-file-at-point)

(require 'expand-region)
(global-set-key (kbd "<M-up>") 'er/expand-region)
(global-set-key (kbd "<M-down>") 'er/contract-region)

(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(require 'helm-config)
(helm-mode 1)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)   ; make TAB works in terminal
(define-key helm-map (kbd "C-z") 'helm-select-action)               ; list actions using C-z
(global-set-key (kbd "M-x") 'helm-M-x)

(require 'multi-term)
(setq multi-term-program "/bin/zsh")

(require 'helm-mt)
(helm-mt/wrap-shells t)
(global-set-key (kbd "C-x t") 'helm-mt)

(require 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching t)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

(require 'magit)
(setq magit-last-seen-setup-instructions "1.4.0")
(global-set-key (kbd "C-c g") 'magit-status)

(require 'yasnippet)
(add-hook 'prog-mode-hook (lambda ()
                            (linum-mode 1)
                            (show-paren-mode)
                            (yas-minor-mode)))

(add-to-list 'load-path "~/.emacs.d/site-lisp/npm-mode")
(require 'npm-mode)

(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode) ; turn on flychecking globally
(setq-default flycheck-temp-prefix ".flycheck") ; customize flycheck temp file prefix

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defun kill-most-buffers ()
  "Kill most buffers, except the two Emacs initial ones."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (member (buffer-name buffer) '("*scratch*" "*Messages*"))
      (kill-buffer buffer))))

;;------------------------------------------------------------------------------
;; Elisp tooling

(defun my-emacs-lisp-mode-hook ()
  "Configure elisp mode."
  (setq indent-tabs-mode nil))
(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)

(defun ielm-auto-complete ()
  "Enables `auto-complete' support in \\[ielm]."
  (setq ac-sources '(ac-source-functions
                     ac-source-variables
                     ac-source-features
                     ac-source-symbols
                     ac-source-words-in-same-mode-buffers))
  (add-to-list 'ac-modes 'inferior-emacs-lisp-mode)
  (auto-complete-mode 1))
(add-hook 'ielm-mode-hook 'ielm-auto-complete)


;;------------------------------------------------------------------------------
;; C++ tooling

(require 'cc-mode)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-hook 'c++-mode-hook (lambda ()
                           (c-set-style "stroustrup")
                           (setq indent-tabs-mode t)))


;;------------------------------------------------------------------------------
;; JavaScript tooling

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '(".babelrc\\'" . json-mode))
(add-to-list 'auto-mode-alist '(".eslintignore\\'" . gitignore-mode))
(add-to-list 'auto-mode-alist '(".eslintrc\\'" . json-mode))
(add-to-list 'auto-mode-alist '(".editorconfig\\'" . json-mode))

(setq js2-basic-offset 2)

;; prefer eslint to js2-mode warnings of missing semi-colons
(setq js2-strict-missing-semi-warning nil)

;; add buffer-local indicator for whether prog-mode-hook has run, to work around
;; js2-mode not being derived from prog-mode.
;;
;; http://yoo2080.wordpress.com/2012/03/15/js2-mode-setup-recommendation/
(defun my-set-pmh-ran ()
  "Add buffer-local indicator for whether 'prog-mode-hook' has run, to work around."
  (set (make-local-variable 'my-pmh-ran) t))
(add-hook 'prog-mode-hook 'my-set-pmh-ran)

;; disable jshint in preference to eslint
(setq-default flycheck-disabled-checkers (append flycheck-disabled-checkers '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers (append flycheck-disabled-checkers '(json-jsonlist)))


(add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)))

(defun json-format ()
  "Format JavaScript region."
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name) t)))

(add-to-list 'load-path "~/.emacs.d/site-lisp/npm-mode")
(require 'npm-mode)

(require 'nvm)


;;------------------------------------------------------------------------------
;; Web tooling

(require 'web-mode)
(add-hook 'web-mode-hook (lambda ()
                           (setq web-mode-markup-indent-offset 2)
                           (setq web-mode-css-indent-offset 2)
                           (setq web-mode-code-indent-offset 2)))

(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))

(require 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)

(setq css-indent-offset 2)

;; React tooling
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  "Edit React JSX with web mode."
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))


;;------------------------------------------------------------------------------
;; Python tooling

(add-hook 'python-mode-hook
          '(lambda () (modify-syntax-entry ?_ "w" python-mode-syntax-table)))
(elpy-enable)


;;------------------------------------------------------------------------------
;; Database tooling

(add-hook 'sql-interactive-mode-hook (lambda ()
                                       (toggle-truncate-lines t)))


;;------------------------------------------------------------------------------
;; Docker tooling

(docker-global-mode)


;;------------------------------------------------------------------------------
;; Structured text tooling

(defun structured-text-hook ()
  "Configure structured text editing settings."
  (auto-fill-mode))

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-hook 'markdown-mode-hook 'structured-text-hook)

(setq plantuml-jar-path "/usr/local/Cellar/plantuml/8024/plantuml.8024.jar")
(add-to-list 'auto-mode-alist '("\\.pu\\'" . plantuml-mode))

(add-hook 'org-mode-hook 'structured-text-hook)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((sh . t)
   (sql . t)
   (plantuml . t)
   (python . t)
   (restclient . t)))

(setq org-list-description-max-indent 5)
(setq org-adapt-indentation nil)
(setq org-plantuml-jar-path plantuml-jar-path)

(add-hook 'rst-mode-hook 'structured-text-hook)

;;------------------------------------------------------------------------------
;; macOS configuration

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize) ; use shell $PATH on Emacs.app for Mac OS X
  (require 'reveal-in-osx-finder)
  (setq insert-directory-program (executable-find "gls"))) ; use coreutils 'ls' command

;;; init.el ends here
