(define-module (d1024 services x11)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages compton)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp))

;;;;;;;;;;;;;;;;;;;;xmodmap;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;Redshift;;;;;;;;;;;;;;;;;;;;;;;

(define redshift-conf
  (local-file (string-append (getenv "HOME") "/.system/d1024/d1024/files/xapps/redshift.conf")))

(define redshift-shepherd
  (shepherd-service
   (documentation "runs redshift to conftrol bluelight levels")
   (provision '(redshift))
   (start #~(make-forkexec-constructor
	     (list #$(file-append redshift "/bin/redshift"))))
   (stop #~(make-kill-destructor))))

(define-public redshift-services
  (list
   (simple-service 'redshift-file
		   home-files-service-type
		   (list
		    `("config/redshift.conf"
		      ,redshift-conf)))
   (simple-service 'redshift-service
		   home-shepherd-service-type
		   (list
		    redshift-shepherd))))
;;;;;;;;;;;;;;;;;xclip;;;;;;;;;;;;;;;;;
(define xclip-shepherd
  (shepherd-service
   (documentation "runs xclip in the background")
   (provision '(xclip))
   (start #~(make-forkexec-constructor
	     (list #$(file-append xclip "/bin/xclip"))))
   (stop #~(make-kill-destructor))))

(define-public xclip-services
  (list
   (simple-service 'xclip-service
		   home-shepherd-service-type
		   (list
		    xclip-shepherd))))

;;;;;;;;;;;;;;;polybar;;;;;;;;;;;;;;;;;;
(define polybar-shepherd
  (shepherd-service
   (documentation "runs polybar in background")
   (provision '(polybar))
   (start #~(make-forkexec-constructor
	      (list #$(file-append polybar "/bin/polybar")
		    "panel")))
   (stop #~(make-kill-destructor))))

(define polybar-conf
  (local-file (string-append
	       (getenv "HOME")
	       "/.system/d1024/d1024/files/xapps/polybar.config")))

(define-public polybar-services
  (list
   (simple-service 'polybar-conf-file
		   home-files-service-type
		   (list
		    `("config/polybar/config"
		      ,polybar-conf)))
   (simple-service 'polybar-service
		   home-shepherd-service-type
		   (list
		    polybar-shepherd))))
;;;;;;;;;;;;;;;;;;;;;;;picom;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define picom-conf
  (local-file (string-append
	       (getenv "HOME")
	       "/.system/d1024/d1024/files/xapps/picom.conf")))

(define picom-shepherd
   (shepherd-service
    (documentation "start picom in the background")
    (provision '(picom))
    (start #~(make-forkexec-constructor
	      (list #$(file-append picom "/bin/picom"))))
    (stop #~(make-kill-destructor))))

(define-public picom-services
  (list
   (simple-service 'picom-conf
		   home-files-service-type
		   (list
		    `("config/picom.conf"
		      ,picom-conf)))
   (simple-service 'picom-service
		   home-shepherd-service-type
		   (list
		    picom-shepherd))))

;;;;;;;;;;;;;;;;;;;xsetroot;;;;;;;;;;;;;;;;;;;;;;;;
(define-public xsetroot-services
  (list
   (simple-service 'xsetroot-service
		   home-shepherd-service-type
		   (list
		    (shepherd-service
		     (documentation "runs xsetroot for left pointer")
		     (provision '(xsetroot))
		     (one-shot? #t)
		     (start #~(make-forkexec-constructor
			       (list #$(file-append xsetroot "/bin/xsetroot")
				     "-cursor-name left_ptr"))))))))

(define-public xinitrc-personal
  (append
   xclip-services
   redshift-services
   polybar-services
   xmodmap-services
   xsetroot-services
   picom-services))
		  
