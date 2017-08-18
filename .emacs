(add-to-list 'load-path "~/.emacs.d/my/")
(load "external-open")
(load "search")
(load "add-to-list-config")
(load "add-hook-config")
(load "mit-scheme-settings")

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

(column-number-mode 1)
(delete-selection-mode 1)
(display-time)
(display-time-mode 1)
(electric-pair-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(global-prettify-symbols-mode 1)
(mouse-avoidance-mode 'proteus)
(setq-default make-backup-files nil)
(show-paren-mode 1)
(tool-bar-mode -1)
(setq-default indent-tabs-mode nil)
(set-language-environment 'utf-8)
(set-fontset-font
 (frame-parameter nil 'font)
 'han
 (font-spec :family "文泉驿等宽微米黑" :size 14))

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

(recentf-mode 1)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
(require 'htmlize)
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
