(define-module (d1024 systems wrath)
  #:use-module (gnu system)
  #:use-module (gnu system mapped-devices)
  #:use-module (gnu)
  #:use-module (gnu system keyboard)
  #:use-module (gnu services xorg)
  #:use-module (gnu packages wm)
  #:use-module (gnu system setuid)
  #:use-module (gnu packages admin)
  #:use-module (gnu system mapped-devices)
  #:use-module (gnu home-services state)
  #:use-module (gnu system file-systems)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 pretty-print)
  #:use-module (d1024 config)

  #:export (wrath-system)
  #:re-export (get-system-config
	       get-home-config))

(use-modules
 (gnu)
 (gnu home)
 (gnu home services)
 (gnu home-services state)
 (gnu system keyboard)
 (gnu services)
 (ice-9 regex)
 (guix gexp)
 (d1024 services symlinks)
 (d1024 services emacs)
 (d1024 services shells)
 (d1024 services packages)
 (d1024 services x11))

(define doas-config
  (plain-file "doas.conf"
	      "\
permit nopass jake as root cmd halt
permit nopass jake as root cmd loginctl
"))

(define channels
  (local-file 
   (canonicalize-path "d1024/channels.scm")))

(define guix-services
  (list
   (simple-service 'guix-files
		   home-files-service-type
		   (list
		    `("system/channels.scm"
		      ,channels)
		    `("config/guix/channels.scm"
		      ,channels)))))

(define %my-keyboard-layout
  (keyboard-layout "us" #:model "thinkpad"))

(define mapped-devices
    (list (mapped-device
           (source
            (uuid "6773b52e-1496-407e-b1d8-9a2ac7f7820f"))
           (target "system-root")
           (type luks-device-mapping))
          (mapped-device
           (source
            (uuid "08123a90-d66b-41ff-8f2c-4435292f7818"))
           (target "crypthome")
           (type luks-device-mapping))))

(define wrath-system
  (set-fields full-default-system
   ((get-os system-host) "wrath")
   ((get-os system-keyboard-layout) %my-keyboard-layout)
   ((get-os system-mapped-dev) mapped-devices)
   ((get-os system-file-system)
    (cons* (file-system
            (mount-point "/")
            (device "/dev/mapper/system-root")
            (type "ext4")
            (dependencies mapped-devices))
           (file-system
            (mount-point "/boot/efi")
            (device (uuid "4B6C-4B80" 'fat32))
            (type "vfat"))
           (file-system
            (mount-point "/home")
            (device "/dev/mapper/crypthome")
            (type "ext4")
            (dependencies mapped-devices))
           %base-file-systems))
   ((get-os system-system-services)
    (cons*
     (service slim-service-type (slim-configuration
                                 (auto-login? #t)
                                 (default-user "jake")
				 (auto-login-session
				  (file-append stumpwm "/bin/stumpwm"))
                                 (xorg-configuration
                                  (xorg-configuration
				   (keyboard-layout %my-keyboard-layout)))))
     (simple-service 'doas-conf
		     etc-service-type
		     (list `("doas.conf" ,doas-config)))
     (system-system-services (get-os full-default-system))))
   
   ((get-os system-swap)
    (list
     (swap-space
      (target "/tempSwap"))))
   ((get-os system-setuid)
    (append
     (list (setuid-program
	    (program (file-append opendoas "/bin/doas"))))
     (system-setuid (get-os full-default-system))))

   ((get-user user-packages)
    (append
              desktop-packages
              wrath-packages))
   ((get-user user-services)
    (append
     (list
      (simple-service 'home-state
		      home-state-service-type
		      (list
		       (state-git ".systems/d1024" "git@github.com:delta1024/d1024.git" )
		       (state-git "Pictures/Wallpapers/DtWallpapers" "https://gitlab.com/dwt1/wallpapers.git" )
		       (state-git ".emacs.d" "https://github.com/plexus/chemacs2.git"))))
     sym-services
     shell-services
     emacs-services
     guix-services
     xinitrc-personal))))

