;; configuration used for all programming modes
(add-hook 'prog-mode-hook (lambda () 
			    (hl-line-mode 1)
			    (linum-mode 1)))
