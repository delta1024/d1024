(define-module (d1024 services stumpwm)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (guix gexp))

(define startup-message "\
(setf *startup-message* \"Hello\")
")
(define startup-programs "\
(run-shell-command \"xsetroot -cursor_name left_ptr\")
(run-shell-command \"picom\")
(run-shell-command \"polybar panel\")
(run-shell-command \"xmodmap .Xmodmap\")
(run-shell-command \"/home/jake/.scripts/wallpaper.sh draw\")
")
(define keymaps "\
(stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd \"C-c\") \"exec alacritty\")
(stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd \"c\") \"exec alacritty\")
")

(define stumpwmrc
  (list 
   `("config/stumpwm/config"
     ,(plain-file "stumpwmrc"
		  (string-append
		   startup-programs
		   keymaps)))))

(define-public stumpwm-services
  (list
   (simple-service 'stumpwm-service
		   home-files-service-type
		   stumpwmrc)))
		   
