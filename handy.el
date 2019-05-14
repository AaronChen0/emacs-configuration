(defun xah-open-in-external-app ()
  "Open the current file or dired marked files in external app.
 app is chosen from your OS's preference.
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2015-01-26"
  (interactive)
  (let* (
         ($file-list
          (if (string-equal major-mode "dired-mode")
              (dired-get-marked-files)
            (list (buffer-file-name))))
         ($do-it-p (if (<= (length $file-list) 5)
			t
		      (y-or-n-p "Open more than 5 files? "))))

    (when $do-it-p
      (cond
       ((string-equal system-type "windows-nt")
        (mapc
         (lambda (fPath)
           (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" fPath t t))) $file-list))
       ((string-equal system-type "darwin")
        (mapc
         (lambda (fPath) (shell-command (format "open \"%s\"" fPath)))  $file-list))
       ((string-equal system-type "gnu/linux")
        (mapc
         (lambda (fPath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" fPath))) $file-list))))))

(defun xah-open-in-desktop ()
  "Show current file in desktop (OS's file manager).
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2015-11-30"
  (interactive)
  (cond
   ((string-equal system-type "windows-nt")
    (w32-shell-execute "explore" (replace-regexp-in-string "/" "\\" default-directory t t)))
   ((string-equal system-type "darwin") (shell-command "open ."))
   ((string-equal system-type "gnu/linux")
    (let ((process-connection-type nil)
          (openFileProgram (if (file-exists-p "/usr/bin/gvfs-open")
                               "/usr/bin/gvfs-open"
                             "/usr/bin/xdg-open")))
      (start-process "" nil openFileProgram "."))
    ;; (shell-command "xdg-open .") ;; 2013-02-10 this sometimes froze emacs till the folder is closed. For example: with nautilus
    )))

(defun xah-open-in-terminal ()
  "Open the current dir in a new terminal window.
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2015-12-10"
  (interactive)
  (cond
   ((string-equal system-type "windows-nt")
    (message "Microsoft Windows not supported. File a bug report or pull request."))
   ((string-equal system-type "darwin")
    (message "Mac not supported. File a bug report or pull request."))
   ((string-equal system-type "gnu/linux")
    (let ((process-connection-type nil))
      (start-process "" nil "x-terminal-emulator"
                     (concat "--working-directory=" default-directory) )))))

(defun open-dot-emacs ()
  (interactive)
  (find-file "~/.emacs"))

(defun switch-to-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun display-startup-echo-area-message ()
  (message "%s" (emacs-init-time)))

(defun refresh-buffer ()
  "Refresh the buffer from the disk"
  (interactive)
  (revert-buffer t (not (buffer-modified-p)) t))

(defun revert-buffer-with-gbk ()
  (interactive)
  (let ((revert-without-query '("")))
    (revert-buffer-with-coding-system 'chinese-gbk)))

(defun insert-time-stamp ()
  (interactive)
  (insert (format-time-string "%a %Y-%m-%d %T")))

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

(global-set-key (kbd "C-c o") 'xah-open-in-external-app)
(global-set-key (kbd "C-c d") 'xah-open-in-desktop)
(global-set-key (kbd "C-c t") 'xah-open-in-terminal)
(global-set-key (kbd "C-c e") 'open-dot-emacs)
(global-set-key (kbd "C-c s") 'switch-to-scratch)
(global-set-key (kbd "C-c x") 'insert-time-stamp)
(global-set-key (kbd "C-c k") 'kill-other-buffers)
(global-set-key (kbd "C-c C-a k") 'kill-all-dired-buffers)
(global-set-key [f5] 'refresh-buffer)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-c c") 'revert-buffer-with-gbk)
(global-set-key (kbd "S-<return>")
                (lambda () (interactive) (move-end-of-line 1) (newline-and-indent)))
