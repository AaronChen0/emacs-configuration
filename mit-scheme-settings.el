(add-to-list 'load-path "~/.emacs.d/emacs-extra-config/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;         MIT-scheme config                        ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is the place where you have installed scheme. Be sure to set
;; this to an appropriate value!!!
(setq scheme-program-name
      (concat
       "/usr/bin/mit-scheme "
       "--library " "/usr/lib/x86_64-linux-gnu/mit-scheme "
       "--band " "/usr/lib/x86_64-linux-gnu/mit-scheme/all.com "
       "-heap 10000"))

;; Use the Edwin-like MIT/Scheme interpreter:
(load "xscheme")

;; generic scheme completion
(require 'scheme-complete)
(autoload 'scheme-smart-complete "scheme-complete" nil t)
(autoload 'scheme-get-current-symbol-info "scheme-complete" nil t)
(setq lisp-indent-function 'scheme-smart-indent-function)

;; mit-scheme documentation
(require 'mit-scheme-doc)

;; Special keys in scheme mode. Use <tab> to indent scheme code to the
;; proper level, and use M-. to view mit-scheme-documentation for any
;; symbol.
(eval-after-load
 'scheme
 '(define-key scheme-mode-map "\t" 'scheme-complete-or-indent))

(eval-after-load
 'cmuscheme
 '(define-key inferior-scheme-mode-map "\t" 'scheme-complete-or-indent))

(eval-after-load
 'xscheme
 '(define-key scheme-interaction-mode-map "\t" 'scheme-complete-or-indent))

(eval-after-load
 'scheme
 '(define-key scheme-mode-map (kbd "M-.") 'mit-scheme-doc-lookup))

(eval-after-load
 'cmuscheme
 '(define-key inferior-scheme-mode-map (kbd "M-.")
    'mit-scheme-doc-lookup))

(eval-after-load
 'xscheme
 '(define-key scheme-interaction-mode-map (kbd "M-.")
    'mit-scheme-doc-lookup))


