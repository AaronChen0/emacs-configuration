(require 'rainbow-delimiters)

(add-hook 'emacs-lisp-mode-hook       'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook             'rainbow-delimiters-mode)
(add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode)
(add-hook 'scheme-mode-hook           'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook       'paredit-mode)
(add-hook 'lisp-mode-hook             'paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'paredit-mode)
(add-hook 'scheme-mode-hook           'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)

