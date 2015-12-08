(global-set-key (kbd "C-c C-a") 'ag-project)
;;;flyspell
(global-set-key (kbd "<f8>") 'ispell-word)
(global-set-key (kbd "C-S-<f8>") 'flyspell-mode)
(global-set-key (kbd "C-M-<f8>") 'flyspell-buffer)
(global-set-key (kbd "C-<f8>") 'flyspell-check-previous-highlighted-word)
(global-set-key (kbd "<C-s-268632076>") 'linum-mode)
;;; ace window
(global-set-key (kbd "C-x o") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?: ?'))


(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word)
  )
(global-set-key (kbd "M-<f8>") 'flyspell-check-next-highlighted-word)
(setenv "DICTIONARY" "english")
;;; markdown
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . visual-line-mode))
(setq system-uses-terminfo nil)

;; allows the ansi term mode to use the clear line escape
(defun toolbear:term-handle-more-ansi-escapes (proc char)
  "Handle additional ansi escapes."
  (cond
   ;; \E[nG - Cursor Horizontal Absolute, e.g. move cursor to column n
   ((eq char ?G)
    (let ((col (min term-width (max 0 term-terminal-parameter))))
      (term-move-columns (- col (term-current-column)))))
   (t)))
(advice-add 'term-handle-ansi-escape :before #'toolbear:term-handle-more-ansi-escapes)
