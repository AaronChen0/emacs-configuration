(setq browse-url-chromium-program "/usr/bin/google-chrome")

(defun google (&rest args)
  (interactive "sGoogle: ")
  (cl-labels ((func (args)
		    (if (null (cdr args))
			args
		      (cons (car args)
			    (cons "+"
				  (func (cdr args)))))))
    (browse-url (concat "https://www.google.com/search?q="
			(apply #'concat (func args))))))

(defun wikipedia (&rest args)
  (interactive "sWikipedia: ")
  (cl-labels ((func (args)
		    (if (null (cdr args))
			args
		      (cons (car args)
			    (cons "_"
				  (func (cdr args)))))))
    (browse-url (concat "https://en.wikipedia.org/wiki/"
			(apply #'concat (func args))))))

(global-set-key (kbd "C-c C-u") 'browse-url-chromium)
(global-set-key (kbd "C-c C-w") 'wikipedia)
(global-set-key (kbd "C-c C-g") 'google)
