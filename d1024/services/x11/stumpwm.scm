(define-module (d1024 services x11 stumpwm)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home-services-utils)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages wm)
  #:use-module (gnu services configuration)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 curried-definitions)
  #:use-module (ice-9 pretty-print)
  #:use-module (gnu services)
  #:use-module (d1024 config)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp))

(define (serialize-lisp-config val)
  (define (serialize-list-element elem)
    (cond
     ((gexp? elem)
      elem)
     (else
     #~(string-trim-right
	 (with-output-to-string
	   (lambda ()
	     ((@@ (ice-9 pretty-print) pretty-print)
	      '#$elem
	      #:max-expr-width 79)))
	 #\newline))))
  #~(string-append
     #$@(interpose
	 (map serialize-list-element val)
	 "\n" 'suffix)))

(define emacs-mode (list #~";;-*- mode: common-lisp; -*-\n"))

(define custom-functions
 `(,#~"(defun emacs ()
  (run-shell-command \"emacsclient -c\"))"
   (defcommand alacritty ()
     ()
     (run-shell-command "alacritty"))))

(define startup-message 
  '((setf *startup-message* "Hello")))

(define mode-line 
 `((stumpwm:toggle-mode-line (stumpwm:current-screen)
                 	     (stumpwm:current-head))
   ,#~"(setf stumpwm:*screen-mode-line-format*
    (list \"%w | \"
         \"%d\"))"))

(define startup-programs 
  '((run-shell-command "xsetroot -cursor_name left_ptr")
  (run-shell-command "/home/jake/.bin/wallpaper.sh draw")))

(define keybinds 
  '((stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "C-c") "alacritty")
   (stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "C-t") "send-raw-key")
   (stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "c") "alacritty")))

(define stumpwmrc
  (list 
   `("config/stumpwm/config"
     ,(mixed-text-file "stumpwmrc"
		  (serialize-lisp-config
		   (append
		   ;;		   mode-line
		   emacs-mode
		   custom-functions
		   startup-programs
		   keybinds))))))
   
(define-public stumpwm-services
  (list
   (simple-service 'stumpwm-config
		   home-files-service-type
		   stumpwmrc)))
		   
