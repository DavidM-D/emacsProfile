;;; JSCS
(autoload 'jscs-indent-apply "jscs" nil t)
(autoload 'jscs-fix "jscs" nil t)
(autoload 'jscs-fix-before-save "jscs" nil t)
(with-eval-after-load 'js
  (add-hook 'js-mode-hook #'jscs-indent-apply))

(with-eval-after-load 'js2-mode
  (add-hook 'js2-mode-hook #'jscs-indent-apply))
(add-hook 'before-save-hook #'jscs-fix-before-save)

;;; js2-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
