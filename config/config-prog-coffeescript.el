(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/coffee-mode"))
(autoload 'coffee-mode "coffee-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))

(add-hook 'coffee-mode-hook (lambda () 
			      (hl-line-mode 1)
			      (linum-mode 1)
			      (setq whitespace-action '(auto-cleanup)) ;; automatically clean up bad whitespace
			      (setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab)))) ;; only show bad whitespace
