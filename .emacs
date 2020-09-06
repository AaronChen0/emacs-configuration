(defvar file-name-handler-alist-old file-name-handler-alist)

(setq package-enable-at-startup nil
      file-name-handler-alist nil
      gc-cons-threshold (* 8 1024 1024)
      gc-cons-percentage 0.6)

(add-hook 'after-init-hook
          `(lambda ()
             (setq file-name-handler-alist file-name-handler-alist-old
                   gc-cons-threshold 800000
                   gc-cons-percentage 0.1)
             (garbage-collect)) t)

(setq load-prefer-newer t)
(setq delete-by-moving-to-trash t)
(setq display-time-24hr-format t)
(setq display-time-default-load-average nil)
(setq inhibit-startup-screen t)
(setq visible-bell t)
(setq user-full-name "Aaron Chen")
(setq initial-scratch-message ";; Good day, Aaron.\n\n")
(setq comint-prompt-read-only t)
(setq dired-listing-switches "-al --group-directories-first -v")
(setq make-backup-files nil)
(setq-default indent-tabs-mode nil)

(display-time)
(display-time-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(mouse-avoidance-mode 'proteus)
(global-prettify-symbols-mode 1)
(column-number-mode 1)
(delete-selection-mode 1)
(electric-pair-mode 1)
(show-paren-mode 1)
(blink-cursor-mode 0)
(tool-bar-mode 0)
(set-language-environment 'utf-8)
(add-to-list 'load-path "~/.emacs.d/my/")

(when window-system
  (set-fontset-font
   (frame-parameter nil 'font)
   'han
   (font-spec :family "WenQuanYi Micro Hei" :size 14))
  (setq frame-title-format
        (list (format "%s %%S: %%j " (system-name))
	      '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))
  (setq default-frame-alist
        '((width . 80)
	  (height . 50)
	  (left . 680)
	  (top . 0))))

(load "add-to-list-config")
(load "add-hook-config")
(load "handy")
(load "search")

;; shift custom-set-variables and custom-set-faces into their own file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; package management
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("elpa"  . "https://elpa.gnu.org/packages/")))
(when (< emacs-major-version 27)
  (package-initialize))

(require 'use-package)
(setq use-package-compute-statistics t)

;; remote file editing
(use-package tramp
  :defer t
  :config
  (setq tramp-default-method "scp"))

;; auto-complete
(use-package auto-complete-config
  :defer 5
  :config
  (add-to-list 'ac-dictionary-directories
	       "~/.emacs.d/elpa/auto-complete-20170124.1845/dict")
  (add-to-list 'ac-modes 'slime-repl-mode)
  (add-to-list 'ac-modes 'lisp-mode)
  (ac-config-default)
  (global-auto-complete-mode t))

(use-package ac-c-headers
  :defer t
  :config
  (add-hook 'c-mode-hook
	    (lambda ()
	      (add-to-list 'ac-sources 'ac-source-c-headers)
	      (add-to-list 'ac-sources 'ac-source-c-header-symbols t)))
  (set-default
   'ac-sources
   (append ac-sources '(ac-source-filename ac-source-files-in-current-dir))))

;; maxima
(add-to-list 'auto-mode-alist '("\\.ma[cx]" . maxima-mode))
(autoload 'imaxima "imaxima" "Frontend of Maxima CAS" t)
(autoload 'maxima-mode "maxima" "Major mode for writing Maxima programs" t)
(autoload 'imath "imath" "Interactive Math mode" t)
(autoload 'imath-mode "imath" "Interactive Math mode" t)
(setq imaxima-use-maxima-mode-flag t)
(setq imaxima-fnt-size "large")
(setq imaxima-pt-size 9)
(setq maxima-save-input-history t)

(use-package recentf
  :defer 5
  :bind ("C-x C-r" . 'recentf-open-files)
  :config
  (recentf-mode 1))

(use-package htmlize :defer t)
(use-package ido
  :config
  (ido-mode 1))

(autoload 'run-scheme "mit-scheme-settings" "run a scheme process" t)
(autoload 'paredit-mode "paredit" t)

(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))
(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)

;; Common Lisp
(use-package slime
  :commands slime
  :load-path "~/slime/"
  :bind
  :config
  (global-set-key "\C-z" 'slime-selector)
  (bind-key* "M-s" 'paredit-splice-sexp)
  (cd "~/code/cl")
  (setq slime-load-failed-fasl 'never)
  (setq common-lisp-hyperspec-root
        (concat "file://" (expand-file-name "~/OnePiece/AI/LISP/HyperSpec/HyperSpec/")))
  (setq
   slime-lisp-implementations
   '((sbcl ("/usr/local/bin/sbcl" "--dynamic-space-size" "1024"))
     (ccl ("~/ccl/lx86cl64"))
     (clisp ("/usr/bin/clisp"))))
  (slime-setup '(slime-fancy)))

(use-package ac-slime
  :hook ((slime-mode-hook slime-repl-mode-hook lisp-mode-hook) . set-up-slime-ac))

;; elisp
(use-package elisp-slime-nav
  :hook (emacs-lisp-mode-hook ielm-mode-hook))

;; C Language
(setq-default c-default-style "linux"
	      c-basic-offset 4)

;; Python
(setq python-shell-interpreter "python3")

;; Javascript
(use-package web-beautify
  :defer t
  :config
  (eval-after-load 'js
    '(define-key js-mode-map (kbd "C-c b") 'web-beautify-js))
  (eval-after-load 'json-mode
    '(define-key json-mode-map (kbd "C-c b") 'web-beautify-js))
  (eval-after-load 'sgml-mode
    '(define-key html-mode-map (kbd "C-c b") 'web-beautify-html))
  (eval-after-load 'web-mode
    '(define-key web-mode-map (kbd "C-c b") 'web-beautify-html))
  (eval-after-load 'css-mode
    '(define-key css-mode-map (kbd "C-c b") 'web-beautify-css)))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))
