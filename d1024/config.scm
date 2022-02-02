(define-module (d1024 config)
  #:use-module (gnu)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages admin)
  #:use-module (gnu services dbus)
  #:use-module (gnu system keyboard)
  #:use-module (gnu services pm)
  #:use-module (gnu home)
  #:use-module (gnu system setuid)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 curried-definitions)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-1)
  #:use-module (nongnu packages linux)

  #:export (full-default-system
	    default-user
	    get-os
	    get-user
	    system-host
	    system-local
	    system-time-zone
	    system-kernel
	    system-firmware
	    system-keyboard-layout
	    system-bootloader
	    system-mapped-dev
	    system-file-system
	    system-swap
	    system-users
	    system-packages
	    system-system-services
	    user-packages
	    user-services
	    default-system
	    system-setuid
	    get-system-config
	    get-home-config))
(use-service-modules
 cups
 nix
 desktop
 networking
 ssh
 xorg)

(define %backlight-udev-rule
  (udev-rule
   "90-backlight.rules"
   (string-append "ACTION==\"add\", SUBSYSTEM==\"backlight\", "
                  "RUN+=\"/run/current-system/profile/bin/chgrp video /sys/class/backlight/%k/brightness\""
                  "\n"
                  "ACTION==\"add\", SUBSYSTEM==\"backlight\", ""RUN+=\"/run/current-system/profile/bin/chmod g+w /sys/class/backlight/%k/brightness\"")))

(define %my-desktop-services
  ;; My personal Desktop configuration
  (modify-services %desktop-services
                   (elogind-service-type config =>
                                         (elogind-configuration
                                          (inherit config)
                                          (handle-lid-switch 'suspend)))
                   ;; (guix-service-type config =>
                   (udev-service-type config =>
                                      (udev-configuration (inherit config)
                                                          (rules (cons %backlight-udev-rule
                                                                       (udev-configuration-rules config)))))

		   (guix-service-type config =>
				      (guix-configuration
				       (inherit config)
				       (substitute-urls
					(append (list "https://substitutes.nonguix.org")
						%default-substitute-urls))
				       (authorized-keys
					(append (list (plain-file "non-guix.pub"
								  "(public-key 
 (ecc 
  (curve Ed25519)
  (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)
  )
 )"))
						%default-authorized-guix-keys))))
                   ))



(define-record-type <base-system>
  (base-system host local time-zone kernel firmware keyboard-layout bootloader mapped-devices file-system swap-devices users packages system-services setuid)
  base-system?
  (host system-host)
  (local system-local)
  (time-zone system-time-zone)
  (kernel system-kernel)
  (firmware system-firmware)
  (keyboard-layout system-keyboard-layout)
  (bootloader system-bootloader)
  (mapped-devices system-mapped-dev)
  (file-system system-file-system)
  (swap-devices system-swap)
  (users system-users)
  (packages system-packages)
  (system-services system-system-services)
  (setuid system-setuid))

(define-record-type <user-config>
  (user-config packages services)
  user-config?
  (packages user-packages)
  (services user-services))

(define-record-type <full-system>
  (full-system os user)
  full-system?
  (os get-os)
  (user get-user))

(define my-keyboard
  (keyboard-layout "us"))

(define default-system
  (base-system
   "FullMetalAlchemist"
   "en_CA.utf8"
   "America/Edmonton"
   linux
   (list linux-firmware)
   (keyboard-layout "us")
   (bootloader-configuration
    (bootloader grub-efi-bootloader)
    (targets '("/boot/efi"))
    (timeout 3)
    (keyboard-layout my-keyboard))
   '()
   (cons* (file-system
           (mount-point "/tmp")
           (device "none")
           (type "tmpfs")
           (check? #f))
          %base-file-systems)
   '()
   (cons* (user-account
           (name "jake")
           (comment "Jake Stannix")
           (group "users")
           (shell (file-append zsh "/bin/zsh"))
           (home-directory "/home/jake")
           (supplementary-groups
            '("wheel" "netdev" "audio" "video" "lp")))
          %base-user-accounts)

   (append
    (list (specification->package "emacs")
          ;; (specification->package "emacs-exwm")
          (specification->package "git")
          (specification->package "stow")
          (specification->package "neovim")
          (specification->package "sx")
          (specification->package "slock")
          (specification->package "gcc-toolchain")
          (specification->package "stumpwm")
	  (specification->package "make")
          (specification->package "gnupg")
          (specification->package "dmenu")
          (specification->package "opendoas")
          (specification->package "xauth")
          (specification->package "zsh")
	  ;; (specification->package "flatpak")
          (specification->package "curl")
          ;; (specification->package "system-config-printer")
          ;; (specification->package
          ;;  "emacs-desktop-enviroment")
          (specification->package "nss-certs"))
    %base-packages)

   (cons* ;; (service xfce-desktop-service-type)
    ;; (service cups-service-type)
    (service openssh-service-type)
    (service nix-service-type)
    (extra-special-file "/usr/bin/env"
                        (file-append coreutils "/bin/env"))
    (modify-services %my-desktop-services
                     (delete gdm-service-type)))
   %setuid-programs))

(define default-user
  (user-config '() '()))

(define full-default-system
  (full-system default-system default-user))


(define (get-system-config config)
  (let* ((os-config (get-os config))
	 (host (system-host os-config))
	 (local (system-local os-config))
	 (timezone (system-time-zone os-config))
	 (kernel (system-kernel os-config))
	 (firmware (system-firmware os-config))
	 (%my-keyboard-layout (system-keyboard-layout os-config))
	 (bootloader (system-bootloader os-config))
	 (mapped-dev (system-mapped-dev os-config))
	 (file-systems (system-file-system os-config))
	 (swap-devices (system-swap os-config))
	 (users (system-users os-config))
	 (packages (system-packages os-config))
	 (services (system-system-services os-config))
	 (set-uid (system-setuid os-config)))

    (operating-system
     (host-name host)
     (locale local)
     (timezone timezone)
     (kernel kernel)
     (firmware firmware)
     (keyboard-layout %my-keyboard-layout)
     (bootloader bootloader)
     (mapped-devices mapped-dev)
     (file-systems file-systems)
     (swap-devices swap-devices)
     (users users)
     (packages packages)
     (services services)
     (setuid-programs set-uid))))

(define (get-home-config config)
  (let* ((user-config (get-user config))
	 (packages (user-packages user-config))
	 (services (user-services user-config)))
    (home-environment
     (packages packages)
     (services services))))

