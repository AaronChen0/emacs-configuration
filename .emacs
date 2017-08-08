(setq delete-by-moving-to-trash t)
(setq display-time-24hr-format t)
(setq display-time-default-load-average nil)
(setq frame-title-format "Emacs")
(setq inhibit-startup-screen t)
(setq visible-bell t)
(setq default-frame-alist
      '((width . 80)
	(height . 50)
	(left . 680)
	(top . 0)))
(setq user-full-name "Aaron Chen")
(setq initial-scratch-message ";; Good day, Aaron.\n\n")

(electric-pair-mode 1)
(tool-bar-mode -1)
(column-number-mode 1)
(show-paren-mode 1)
(display-time-mode 1)
(display-time)
(fset 'yes-or-no-p 'y-or-n-p)
(mouse-avoidance-mode 'proteus)
(global-prettify-symbols-mode 1)
(setq-default make-backup-files nil)
(set-language-environment 'utf-8)
(set-fontset-font
 (frame-parameter nil 'font)
 'han
 (font-spec :family "文泉驿等宽微米黑" :size 14))

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)

(add-to-list 'load-path "~/.emacs.d/my/")
(load "external-open")
(load "search")
(load "elpa-add-to-list")
(require 'htmlize)

;; auto-complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories
	     "~/.emacs.d/elpa/auto-complete-20170124.1845/dict")
(ac-config-default)
(require 'ac-c-headers)
(add-hook 'c-mode-hook
	  (lambda ()
	    (add-to-list 'ac-sources 'ac-source-c-headers)
	    (add-to-list 'ac-sources 'ac-source-c-header-symbols t)))

(require 'ido)
(ido-mode 1)

;; package management
(package-initialize)
(setq package-archives
      '(("melpa" . "http://melpa.org/packages/")
	("melpa-stable" . "http://stable.melpa.org/#/")))

(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code."
  t)

;; Common Lisp
(setq slime-lisp-implementations
      '((sbcl ("/usr/local/bin/sbcl"))
	(ccl ("~/ccl/lx86cl64"))
	(clisp ("/usr/bin/clisp"))))
(add-to-list 'load-path "~/slime/")
(require 'slime)
(setq slime-net-coding-system 'utf-8-unix)
(slime-setup '(slime-fancy))

;; C Language
(setq-default c-default-style "linux"
	      c-basic-offset 4)

(defun open-dot-emacs ()
  (interactive)
  (find-file "~/.emacs"))
(global-set-key (kbd "C-c C-d") 'open-dot-emacs)
