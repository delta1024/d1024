(use-modules (gnu system)
	     (gnu system keyboard)
	     (gnu system mapped-devices)
	     (gnu home-services state)
	     (gnu system file-systems)
	     (d1024 base-system))

(define system
  (operating-system
   (inherit base-operating-system)
   (host-name "wrath")			

   (keyboard-layout (keyboard-layout "us" #:model "thinkpad"))

   (mapped-devices
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

   (file-systems
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
   (swap-devices
    (list
     (swap-space
      (target "/tempSwap"))))))

(use-modules
 (gnu)
 (gnu home)
 (gnu home services)
 (gnu home-services state)
 (gnu services)
 (ice-9 regex)
 (guix gexp)
 (d1024 services symlinks)
 (d1024 services x11 stumpwm)
 (d1024 services emacs)
 (d1024 services shells)
 (d1024 services packages)
 (d1024 services x11))

(define guix-services
  (list
   (simple-service 'guix-files
		   home-files-service-type
		   (list
		    `("system/channels.scm"
		      ,channels)
		    `("config/guix/channels.scm"
		      ,channels)))))


(define home
  (home-environment
   (packages (append
              desktop-packages
              wrath-packages))		
   (services
    (append
     (list
      (simple-service 'home-state
		      home-state-service-type
		      (list
		       (state-git ".systems/d1024" "git@github.com:delta1024/d1024.git" ))))
     sym-services
     shell-services
     emacs-services
     guix-services
     stumpwm-services
     xinitrc-personal))))
