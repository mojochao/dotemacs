;; This configuration is intended for use with Emacs 24.3 or later.

(set-frame-height (selected-frame) 60)
(set-frame-width (selected-frame) 200)

;;-------------------------------------------------------------------------
;;
;; Initial Configuration
;;
;;-------------------------------------------------------------------------

;; Turn off mouse interface early in startup to avoid momentary display
;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please or scratch buffer content
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; configure customization data to be persisted outside of the .emacs file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; configure backups to be persisted outside of source directories
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;;-------------------------------------------------------------------------
;;
;; Emacs Daemon Configuration.  Why would you ever want to kill it anyway?!
;;
;;-------------------------------------------------------------------------

(require 'server)
(unless (server-running-p)
  (server-start))

;;-------------------------------------------------------------------------
;;
;; Package Management Configuration
;;
;;-------------------------------------------------------------------------

(defvar vendor-packages '(js2-mode
			  magit
			  plantuml-mode
			  web-mode
			  yasnippet
			  zenburn-theme)
  "A list of external packages to ensure are installed at launch.")

(require 'package)

(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (package vendor-packages)
  (when (not (package-installed-p package))
    (package-install package)))
(setq line-number-mode t)               ; show line numbers in modeline
(delete-selection-mode 1)               ; delete marked region and replace with new content
(tooltip-mode -1)                       ; hide tool tips
(fset 'yes-or-no-p 'y-or-n-p)           ; use shortcuts for all yes/no prompts

(load-theme 'zenburn)

;;------------------------------------------------------------------------------
;;
;; Interactivity Enhancers
;;
;;------------------------------------------------------------------------------

(require 'ido)
(setq ido-enable-flex-matching t)
(ido-mode t)
(ido-everywhere t)

(require 'recentf)
(recentf-mode t)
(setq recentf-max-saved-items 50)

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

(global-set-key (kbd "C-x C-r") 'ido-recentf-open)


(defun open-line-above ()
  "Open a line above the line the point is at.
Then move to that line and indent accordning to mode"
  (interactive)
  (move-beginning-of-line 1)
  (newline)
  (previous-line)
  (indent-according-to-mode))

(defun open-line-below ()
  "Open a line below the line the point is at.
Then move to that line and indent accordning to mode"
  (interactive)
  (move-end-of-line 1)
  (newline)
  (indent-according-to-mode))

;;------------------------------------------------------------------------------
;;
;; Keybindings
;;
;;------------------------------------------------------------------------------

(define-key global-map [home] 'beginning-of-line)
(define-key global-map [end] 'end-of-line)
(define-key global-map [C-return] 'open-line-below)
(define-key global-map [C-S-return] 'open-line-above)

;;------------------------------------------------------------------------------
;;
;; General IDE
;;
;;------------------------------------------------------------------------------

(require 'yasnippet)
(add-hook 'prog-mode-hook (lambda () 
			    (hl-line-mode 1)
			    (linum-mode 1)
			    (show-paren-mode)
			    (yas-minor-mode)))

;; add buffer-local indicator for whether prog-mode-hook has run, to work around
;; js2-mode not being derived from prog-mode.
;;
;; http://yoo2080.wordpress.com/2012/03/15/js2-mode-setup-recommendation/
(defun my-set-pmh-ran ()
  (set (make-local-variable 'my-pmh-ran) t))

(add-hook 'prog-mode-hook 'my-set-pmh-ran)

;;------------------------------------------------------------------------------
;;
;; C++ IDE
;;
;;------------------------------------------------------------------------------

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-hook 'c++-mode-hook (lambda ()
			   (c-set-style "stroustrup")
			   (setq indent-tabs-mode t)))

;;------------------------------------------------------------------------------
;;
;; HTML IDE
;;
;;------------------------------------------------------------------------------

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))

;; (add-hook 'html-mode-hook 'my-run-pmh-if-not-ran)
;; (defun my-run-pmh-if-not-ran ()
;;   (unless (bound-and-true-p my-pmh-ran)
;;     (run-hooks 'prog-mode-hook)))

;;------------------------------------------------------------------------------
;;
;; JavaScript IDE
;;
;;------------------------------------------------------------------------------

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(add-hook 'js2-mode-hook (lambda () 
			   (setq js-indent-level 2)
			   (setq js2-indent-level 2)
			   (setq js2-basic-offset 2)))
(add-hook 'js2-mode-hook 'my-run-pmh-if-not-ran)

(defun my-run-pmh-if-not-ran ()
  (unless (bound-and-true-p my-pmh-ran)
    (run-hooks 'prog-mode-hook)))

;;------------------------------------------------------------------------------
;;
;; PlantUML IDE
;;
;;------------------------------------------------------------------------------

(require 'plantuml-mode)
(add-to-list 'auto-mode-alist '("\\.pu\\'" . plantuml-mode))

(add-hook 'image-mode-hook (lambda ()
			     (auto-revert-mode 1)
			     (auto-image-file-mode +1)))
