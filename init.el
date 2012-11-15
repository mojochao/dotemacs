;; configure customization data to be persisted outside of the .emacs file

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; configure backups to be persisted outside of source directories when editing

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; configure stfu

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; configure global ui settings

(setq frame-title-format '("%S" (buffer-file-name "%f" (dired-directory dired-directory "%b")))) ; display full path of active buffer in window title area
(setq eval-expression-print-length nil) ; prevent emacs truncation of messages in the echo area
(setq visible-bell t)                   ; flash frame instead of audible bell on error
(setq column-number-mode t)             ; show column numbers in modeline
(setq line-number-mode t)               ; show line numbers in modeline
(delete-selection-mode 1)               ; delete marked region and replace with new content
(scroll-bar-mode -1)                    ; hide scroll bars
(menu-bar-mode -1)                      ; hide menu bar
(tool-bar-mode -1)                      ; hide tool bar
(tooltip-mode -1)                       ; hide tool tips
(fset 'yes-or-no-p 'y-or-n-p)           ; use shortcuts for all yes/no prompts

;; configure elisp repositories
(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

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

(require 'dired-x)
(global-set-key (kbd "C-x C-n") 'find-name-dired)

;; load sub-configurations
(load-library "~/.emacs.d/config/config-mark-multiple.el")
