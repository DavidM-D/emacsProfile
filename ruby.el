(require 'smartparens-ruby)
(add-to-list 'load-path "~/.emacs.d/vendor")
(require 'rubocop)
(add-hook 'ruby-mode-hook 'rubocop-mode)
