;; This configuration is intended for use with Emacs 24.3 or later.

;;-------------------------------------------------------------------------
;;
;; Initial Configuration
;;
;;-------------------------------------------------------------------------

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

;; show filename in window frame
(setq frame-title-format "%f")

;; No truncation of messages in 'echo' area
(setq eval-expression-print-length nil)

(setq column-number-mode t)	        ; show line numbers in modeline
(setq line-number-mode t)               ; show line numbers in modeline
(delete-selection-mode 1)               ; delete marked region and replace with new content
(fset 'yes-or-no-p 'y-or-n-p)           ; use shortcuts for all yes/no prompts

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
			  smex
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

;;------------------------------------------------------------------------------
;;
;; Theming
;;
;;------------------------------------------------------------------------------

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

(setq smex-save-file "~/.emacs.d/smex.save")
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-X") 'smex)

(require 'recentf)
(recentf-mode t)
(setq recentf-max-saved-items 50)

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

(defun open-line-above ()
  "Open a line above the line the point is at.
Then move to that line and indent according to mode"
  (interactive)
  (move-beginning-of-line 1)
  (newline)
  (previous-line)
  (indent-according-to-mode))

(defun open-line-below ()
  "Open a line below the line the point is at.
Then move to that line and indent according to mode"
  (interactive)
  (move-end-of-line 1)
  (newline)
  (indent-according-to-mode))

(defun find-file-at-point-with-line()
  "Find file at point and jump to line number delimited by colon. (main.cpp:23)"
  (interactive)
  (setq line-num 0)
  (save-excursion
    (search-forward-regexp "[^ ]:" (point-max) t)
    (if (looking-at "[0-9]+")
         (setq line-num (string-to-number (buffer-substring (match-beginning 0) (match-end 0))))))
  (find-file (ffap-guesser))
  (if (not (equal line-num 0))
      (goto-line line-num)))

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

(require 'cc-mode)
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

;;------------------------------------------------------------------------------
;;
;; JavaScript IDE
;;
;;------------------------------------------------------------------------------

(require 'js2-mode)
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
;; Common Lisp IDE
;;
;;------------------------------------------------------------------------------

(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "/usr/local/bin/sbcl")

;;------------------------------------------------------------------------------
;;
;; Keybindings
;;
;;------------------------------------------------------------------------------

(define-key global-map [home] 'beginning-of-line)
(define-key global-map [end] 'end-of-line)
(define-key global-map [C-return] 'open-line-below)
(define-key global-map [C-S-return] 'open-line-above)
(define-key global-map [S-f12] 'find-file-at-point)
(define-key global-map [f12] 'find-file-at-point-with-line)
(define-key global-map [C-x C-g] 'ido-recentf-open)
