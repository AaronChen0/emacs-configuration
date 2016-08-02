(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(display-time-24hr-format t)
 '(display-time-default-load-average nil)
 '(display-time-mode t)
 '(frame-title-format "%b - emacs")
 '(inhibit-startup-screen t)
 '(initial-buffer-choice "~/.emacs")
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(visible-bell t)
 '(default-frame-alist
    '((width . 80)
      (height . 42)
      (left . 680)
      (top . 0)))
 '(user-full-name "JiaSheng Chen"))


(display-time)
(fset 'yes-or-no-p 'y-or-n-p)
(global-linum-mode 1) ; always show line numbers
(mouse-avoidance-mode 'proteus)                                
(global-prettify-symbols-mode 1)
(setq-default make-backup-files nil)
(set-language-environment 'utf-8)
(set-fontset-font
 (frame-parameter nil 'font)
 'han
 (font-spec :family "Microsoft Yahei" :size 12))


;; auto-complete
(add-to-list 'load-path "~/.emacs.d/elpa/auto-complete-20160710.1544")
(add-to-list 'load-path "~/.emacs.d/elpa/popup-20160709.729")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-20160710.1544/dict")
(ac-config-default)


;; package management
(package-initialize)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")
			 ("melpa-stable" . "http://stable.melpa.org/#/")))


(add-hook 'find-file-hooks 'auto-insert) ; 自动插入
;; (setq auto-insert t)
;; (setq auto-insert-query t) (setq auto-insert-directory "~/insert/")
;; (define-auto-insert ...)

(add-hook 'write-file-hooks 'time-stamp)
(setq time-stamp-format "%:u %02m/%02d/%04y %02H:%02M:%02S")

(add-to-list 'load-path "~/.emacs.d/my")
(require 'tabbar)
(tabbar-mode t)


;; (load "desktop")          ; 保存桌面
;; (desktop-load-default)
;; (desktop-read)

(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code."
  t)

(setq inferior-lisp-program "F:/Recent/ccl/wx86cl")
(add-to-list 'load-path "F:/Recent/slime/")
(require 'slime)
(setq slime-net-coding-system 'utf-8-unix)
(slime-setup '(slime-fancy))






;;go to char
(defun wy-go-to-char (n char)
  "Move forward to Nth occurence of CHAR.
Typing `wy-go-to-char-key' again will move forwad to the next Nth
occurence of CHAR."
  (interactive "p\ncGo to char: ")
  (search-forward (string char) nil nil n)
  (while (char-equal (read-char)
		     char)
    (search-forward (string char) nil nil n))
  (setq unread-command-events (list last-input-event)))

(define-key global-map (kbd "C-c a") 'wy-go-to-char)

(defun xah-open-in-external-app ()
  "Open the current file or dired marked files in external app.
 app is chosen from your OS's preference.

URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2015-01-26"
  (interactive)
  (let* (
         (¦Îfile-list
          (if (string-equal major-mode "dired-mode")
              (dired-get-marked-files)
            (list (buffer-file-name))))
         (¦Îdo-it-p (if (<= (length ¦Îfile-list) 5)
			t
		      (y-or-n-p "Open more than 5 files? "))))

    (when ¦Îdo-it-p
      (cond
       ((string-equal system-type "windows-nt")
        (mapc
         (lambda (fPath)
           (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" fPath t t))) ¦Îfile-list))
       ((string-equal system-type "darwin")
        (mapc
         (lambda (fPath) (shell-command (format "open \"%s\"" fPath)))  ¦Îfile-list))
       ((string-equal system-type "gnu/linux")
        (mapc
         (lambda (fPath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" fPath))) ¦Îfile-list))))))
(global-set-key (kbd "<end>") 'xah-open-in-external-app)

;;;;maximize the window display
;; (defun maximize-frame ()
;;  "Maximizes the active frame in Windows"
;;  (interactive)
;;  ;; Send a `WM_SYSCOMMAND' message to the active frame with the
;;  ;; `SC_MAXIMIZE' parameter.
;;  (when (eq system-type 'windows-nt)
;;    (w32-send-sys-command 61488)))
;; (add-hook 'window-setup-hook 'maximize-frame t)

