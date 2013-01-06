;; configure js2-mode from https://github.com/mooz/js2-mode, per recommendations at
;;   http://yoo2080.wordpress.com/2012/03/15/js2-mode-setup-recommendation/
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/js2-mode"))
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(defun my-set-prog-mode-hook-ran ()
  (set (make-local-variable 'my-prog-mode-hook-ran) t))

(defun my-run-prog-mode-hook-if-not-ran ()
  (unless (bound-and-true-p my-prog-mode-hook-ran)
    (run-hooks 'prog-mode-hook)))

(defun my-disable-indent-tabs-mode ()
  (set-variable 'indent-tabs-mode nil))

(add-hook 'prog-mode-hook 'my-set-prog-mode-hook-ran)
(add-hook 'js2-mode-hook 'my-run-prog-mode-hook-if-not-ran)
(add-hook 'js2-mode-hook 'my-disable-indent-tabs-mode)

(eval-after-load "js2-mode"
  '(progn
     (setq js2-missing-semi-one-line-override t)
     ;(setq-default js2-basic-offset 2) ; 2 spaces for indentation
     ;; following is from http://www.emacswiki.org/emacs/Js2Mode
     (add-hook 'js2-post-parse-callbacks 'my-js2-parse-global-vars-decls)
     (defun my-js2-parse-global-vars-decls ()
       (let ((btext (replace-regexp-in-string
                     ": *true" " "
                     (replace-regexp-in-string "[\n\t ]+" " " (buffer-substring-no-properties 1 (buffer-size)) t t))))
         (setq js2-additional-externs
               (split-string
                (if (string-match "/\\* *global *\\(.*?\\) *\\*/" btext) (match-string-no-properties 1 btext) "")
                " *, *" t))
         ))
     ))
