(define-module (d1024 systems wrath)
  #:use-module (gnu services)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (guix gexp))

(define channels
  (plain-file "channels.scm"
	      "\
(list (channel
       (name 'guix)
       (url \"https://git.savannah.gnu.org/git/guix.git\")
       (introduction
        (make-channel-introduction
         \"9edb3f66fd807b096b48283debdcddccfea34bad\"
         (openpgp-fingerprint
          \"bbb0 2ddf 2cea f6a8 0d1d  e643 a2a0 6df2 a33a 54fa\"))))
      (channel
       (name 'nonguix)
       (url \"https://gitlab.com/nonguix/nonguix\")
       (introduction
        (make-channel-introduction
         \"897c1a470da759236cc11798f4e0a5f7d4d59fbc\"
         (openpgp-fingerprint
          \"2a39 3fff 68f4 ef7a 3d29  12af 6f51 20a0 22fb b2d5\"))))
      (channel
       (name 'rde)
       (url \"https://git.sr.ht/~abcdw/rde\")
       (introduction
        (make-channel-introduction
         \"257cebd587b66e4d865b3537a9a88cccd7107c95\"
         (openpgp-fingerprint
          \"2841 9ac6 5038 7440 c7e9  2ffa 2208 d209 58c1 deb0\"))))
      (channel
       (name 'flat)
       (url \"https://github.com/flatwhatson/guix-channel.git\")
       (introduction
        (make-channel-introduction
         \"33f86a4b48205c0dc19d7c036c85393f0766f806\"
         (openpgp-fingerprint
          \"736a c00e 1254 378b a982  7af6 9dbe 8265 81b6 4490\")))))"))

(define makefile
  (plain-file "Makefile"
	      "\
home:
	GUILE_LOAD_PATH=./ \\
	guix home reconfigure ./home.scm
system:
	guix home -L ./ reconfigure ./home.scm \\
	&& sudo -E guix system -L ../.config/guix/systems \\
	reconfigure ../.config/guix/system.scm
update-channel:
	guix describe -f channels > ./d1024/channel-lock.scm"))

(define base-system
  (local-file 
   (string-append (getenv "HOME") "/.system/d1024/base-system.scm")))

(define wrath-system
  (plain-file "wrath-system.scm"
	      "\
(use-modules (gnu system keyboard)
             (gnu system mapped-devices)
             (gnu system file-systems)
             (base-system))

(operating-system
 (inherit base-operating-system)
 (host-name \"wrath\")			

 (keyboard-layout (keyboard-layout \"us\" #:model \"thinkpad\"))

 (mapped-devices
  (list (mapped-device
         (source
          (uuid \"6773b52e-1496-407e-b1d8-9a2ac7f7820f\"))
         (target \"system-root\")
         (type luks-device-mapping))
        (mapped-device
         (source
          (uuid \"08123a90-d66b-41ff-8f2c-4435292f7818\"))
         (target \"crypthome\")
         (type luks-device-mapping))))

 (file-systems
  (cons* (file-system
          (mount-point \"/\")
          (device \"/dev/mapper/system-root\")
          (type \"ext4\")
          (dependencies mapped-devices))
         (file-system
          (mount-point \"/boot/efi\")
          (device (uuid \"4B6C-4B80\" 'fat32))
          (type \"vfat\"))
         (file-system
          (mount-point \"/home\")
          (device \"/dev/mapper/crypthome\")
          (type \"ext4\")
          (dependencies mapped-devices))
         
         %base-file-systems)))"))

(define wrath-user
  (plain-file "wrath-home.scm"
	      "\
(define-module (wrath)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (d1024 services symlinks)
  #:use-module (d1024 services stumpwm)
  #:use-module (d1024 services emacs)
  #:use-module (d1024 services shells)
  #:use-module (d1024 services packages)
  #:use-module (d1024 services x11)
  #:use-module (d1024 systems wrath)
  #:use-module (guix gexp))

(home-environment
 (packages (append
            desktop-packages
            wrath-packages))		
 (services
  (append
   sym-services
   shell-services
   emacs-services
   guix-services
   stumpwm-services
   xinitrc)))"))

(define-public guix-services
  (list
   (simple-service 'guix-files
		   home-files-service-type
		   (list
		    `("system/channels.scm"
		      ,channels)
		    `("system/home.scm"
		      ,wrath-user)
		    `("system/Makefile"
		      ,makefile)
		    `("config/guix/channels.scm"
		      ,channels)
		    `("config/guix/system.scm"
		      ,wrath-system)
		    `("config/guix/systems/base-system.scm"
		      ,base-system)))))
