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

;; configure gnus to use configuration in ~/.emacs.d directory
(setq gnus-init-file "~/.emacs.d/.gnus.el")
(setq gnus-save-newsrc-file nil)

;; configure backups to be persisted outside of source directories
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; show filename in window frame
(setq frame-title-format "%f")

;; no truncation of messages in 'echo' area
(setq eval-expression-print-length nil)

(setq column-number-mode t)             ; show line numbers in modeline
(setq line-number-mode t)               ; show line numbers in modeline
(delete-selection-mode 1)               ; delete marked region and replace with new content
(fset 'yes-or-no-p 'y-or-n-p)           ; use shortcuts for all yes/no prompts

;; smooth scrolling with trackpad or mouse
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq scroll-step 1)

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

(defvar common-packages '(cider
                          docker
                          dockerfile-mode
                          elpy
                          emmet-mode
                          exec-path-from-shell
			  gitignore-mode
                          helm
                          helm-mt
                          helm-projectile
                          js2-mode
                          json-mode
                          less-css-mode
                          lorem-ipsum
                          magit
                          markdown-mode
                          multi-term
                          paredit
                          plantuml-mode
                          projectile
                          restclient
                          smart-mode-line
                          web-mode
                          yasnippet
                          zenburn-theme)
  "A list of common external packages to ensure are installed at launch.")

(defvar darwin-packages '(reveal-in-osx-finder)
  "A list of Mac OS packages to ensure are installed at launch.")

(defvar vendor-packages common-packages
  "A list of all packages to ensure are installed at launch.")
(if (eq system-type 'darwin)
    (append vendor-packages darwin-packages))

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
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

(define-key global-map [s-return] 'toggle-frame-fullscreen) ;; same as iTerm
(define-key global-map [home] 'beginning-of-line)
(define-key global-map [end] 'end-of-line)

(require 'recentf)
(recentf-mode t)
(setq recentf-max-saved-items 50)

(defun open-line-above ()
  "Open a line above the line the point is at.
Then move to that line and indent according to mode"
  (interactive)
  (move-beginning-of-line 1)
  (newline)
  (previous-line)
  (indent-according-to-mode))

(define-key global-map [C-return] 'open-line-below)

(defun open-line-below ()
  "Open a line below the line the point is at.
Then move to that line and indent according to mode"
  (interactive)
  (move-end-of-line 1)
  (newline)
  (indent-according-to-mode))

(define-key global-map [C-S-return] 'open-line-above)

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

(define-key global-map [f12] 'find-file-at-point-with-line)
(define-key global-map [S-f12] 'find-file-at-point)


;;------------------------------------------------------------------------------
;;
;; Terminals
;;
;;------------------------------------------------------------------------------

(require 'multi-term)
(setq multi-term-program "/bin/zsh")

;;------------------------------------------------------------------------------
;;
;; Helm/Projectile
;;
;;------------------------------------------------------------------------------

(require 'helm-config)
(helm-mode 1)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)   ; make TAB works in terminal
(define-key helm-map (kbd "C-z") 'helm-select-action)               ; list actions using C-z
(global-set-key (kbd "M-x") 'helm-M-x)

(projectile-global-mode)
(setq projectile-enable-caching t)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

(require 'smart-mode-line)
(sml/setup)

(require 'helm-mt)
(helm-mt/wrap-shells t)
(global-set-key (kbd "C-x t") 'helm-mt)

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


(defun sudo-save ()
  (interactive)
  (if (not buffer-file-name)
      (write-file (concat "/sudo:root@localhost:" (ido-read-file-name "File:")))
    (write-file (concat "/sudo:root@localhost:" buffer-file-name))))

(setq magit-last-seen-setup-instructions "1.4.0")
(global-set-key (kbd "C-c g") 'magit-status)

;;------------------------------------------------------------------------------
;;
;; Docker tooling
;;
;;------------------------------------------------------------------------------

(docker-global-mode)

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
;; Web IDE
;;
;;------------------------------------------------------------------------------

(require 'web-mode)
(add-hook 'web-mode-hook (lambda ()
                           (setq web-mode-markup-indent-offset 2)
                           (setq web-mode-css-indent-offset 2)
                           (setq web-mode-code-indent-offset 2)))

(require 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)

(setq css-indent-offset 2)

(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

(add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)))

(defun json-format ()
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name) t)))

(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

;; https://github.com/mojochao/emacs-npm
(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-npm")
(require 'emacsnpm)
(emacsnpm-global-mode)

;;------------------------------------------------------------------------------
;;
;; Python IDE
;;
;;------------------------------------------------------------------------------

(add-hook 'python-mode-hook
          '(lambda () (modify-syntax-entry ?_ "w" python-mode-syntax-table)))
(elpy-enable)

;;------------------------------------------------------------------------------
;;
;; SQL IDE
;;
;;------------------------------------------------------------------------------

(add-hook 'sql-interactive-mode-hook (lambda ()
                                       (toggle-truncate-lines t)))

;;------------------------------------------------------------------------------
;;
;; Markdown IDE
;;
;;------------------------------------------------------------------------------

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;------------------------------------------------------------------------------
;;
;; PlantUML IDE
;;
;;------------------------------------------------------------------------------

(setq plantuml-jar-path "/usr/local/Cellar/plantuml/8024/plantuml.8024.jar")
(add-to-list 'auto-mode-alist '("\\.pu\\'" . plantuml-mode))

;;------------------------------------------------------------------------------
;;
;; Org mode
;;
;;------------------------------------------------------------------------------

(org-babel-do-load-languages
 'org-babel-load-languages
 '(;; other Babel languages
   (plantuml . t)))

(setq org-list-description-max-indent 5)
(setq org-adapt-indentation nil)
(setq org-plantuml-jar-path plantuml-jar-path)

;;------------------------------------------------------------------------------
;;
;; OS-specific configuration
;;------------------------------------------------------------------------------

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize) ; use shell $PATH on Emacs.app for Mac OS X
  (require 'reveal-in-osx-finder)
  (setq insert-directory-program (executable-find "gls"))) ; use coreutils verson of 'ls' command
