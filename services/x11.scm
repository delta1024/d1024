(define-module (d1024 services x11)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages xorg)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp))

(define xmodmaprc
  (plain-file "xmodmap"
	      "\
clear lock
clear control
keycode 66 = Control_L
add control = Control_L
add Lock = Control_R
keycode  23 = BackSpace BackSpace BackSpace BackSpace
keycode  22 = Tab ISO_Left_Tab Tab ISO_Left_Tab"))

(define xmodmap-shepherd
  (shepherd-service
   (documentation "runs xmodmap once at login")
   (provision '(xmodmap))
   (one-shot? #t)
   (start #~(make-forkexec-constructor
	     (list #$(file-append xmodmap "/bin/xmodmap")
		   #$xmodmaprc)))))
(define-public xmodmap-services
  (list
   (simple-service 'xmodmap-service
		   home-shepherd-service-type
		   (list
		    xmodmap-shepherd))))

				  
		  
