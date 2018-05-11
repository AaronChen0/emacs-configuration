(defun open-dot-emacs ()
  (interactive)
  (find-file "~/.emacs"))
(global-set-key (kbd "C-c e") 'open-dot-emacs)

(defun switch-to-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))
(global-set-key (kbd "C-c s") 'switch-to-scratch)

(add-to-list 'load-path "~/.emacs.d/my/")
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq load-prefer-newer t)
(load "add-to-list-config")
(load "add-hook-config")
(load "external-open")
(load "search")

(setq delete-by-moving-to-trash t)
(setq display-time-24hr-format t)
(setq display-time-default-load-average nil)
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
	    '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))
(setq inhibit-startup-screen t)
(setq visible-bell t)
(setq default-frame-alist
      '((width . 80)
	(height . 50)
	(left . 680)
	(top . 0)))
(setq user-full-name "Aaron Chen")
(setq initial-scratch-message ";; Good day, Aaron.\n\n")
(setq blink-cursor-mode nil)
(setq comint-prompt-read-only t)
(setq dired-listing-switches "-al --color=auto --group-directories-first")

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

;; remote file editing
(require 'tramp)
(setq tramp-default-method "scp")

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

(require 'yasnippet)
(yas-global-mode 1)

(recentf-mode 1)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
(require 'htmlize)
(require 'ido)
(ido-mode 1)

;; package management
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
;       ("elpa" .  "https://elpa.gnu.org/packages/")
	("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)

(autoload 'run-scheme "mit-scheme-settings" "run a scheme process" t)
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

(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

;; elisp
(require 'elisp-slime-nav)
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'elisp-slime-nav-mode))

;; C Language
(setq-default c-default-style "linux"
	      c-basic-offset 4)

;; Python
(setq python-shell-interpreter "python3")

;; Javascript
(require 'web-beautify)
(eval-after-load 'js
  '(define-key js-mode-map (kbd "C-c b") 'web-beautify-js))

(eval-after-load 'json-mode
  '(define-key json-mode-map (kbd "C-c b") 'web-beautify-js))

(eval-after-load 'sgml-mode
  '(define-key html-mode-map (kbd "C-c b") 'web-beautify-html))

(eval-after-load 'web-mode
  '(define-key web-mode-map (kbd "C-c b") 'web-beautify-html))

(eval-after-load 'css-mode
  '(define-key css-mode-map (kbd "C-c b") 'web-beautify-css))

(defun insert-time-stamp ()
  (interactive)
  (insert (format-time-string "%a %Y-%m-%d %T")))
(global-set-key (kbd "C-c x") 'insert-time-stamp)

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (buffer-list))))

(defun kill-all-dired-buffers()
  "Kill all dired buffers."
  (interactive)
  (save-excursion
    (let((count 0))
      (dolist(buffer (buffer-list))
        (set-buffer buffer)
        (when (equal major-mode 'dired-mode)
          (setq count (1+ count))
          (kill-buffer buffer)))
      (message "Killed %i dired buffer(s)." count ))))
(global-set-key (kbd "C-c k") 'kill-other-buffers)
(global-set-key (kbd "C-c C-a k") 'kill-all-dired-buffers)

(global-set-key (kbd "S-<return>")
                (lambda () (interactive) (move-end-of-line 1) (newline-and-indent)))

(global-set-key (kbd "C-c c")
                (lambda () (interactive)
                  (let ((revert-without-query '("")))
                    (revert-buffer-with-coding-system 'chinese-gbk))))

(global-set-key [f5]
                '(lambda ()
                   "Refresh the buffer from the disk"
                   (interactive)
                   (revert-buffer t (not (buffer-modified-p)) t)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (go-mode js-auto-beautify markdown-mode gh-md yasnippet sudoku paredit magit helm graphviz-dot-mode elisp-slime-nav ac-slime ac-octave ac-c-headers))))
