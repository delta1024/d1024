(define-module (base-system)
  #:use-module (gnu)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages wm)
  #:use-module (gnu services dbus)
  #:use-module (gnu services pm)
  #:use-module (srfi srfi-1)
  #:use-module (nongnu packages linux))
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
                   ))

(define-public base-operating-system
  (operating-system
   (host-name "FullMetalAlchemist")

   (locale "en_CA.utf8")
   (timezone "America/Edmonton")

   (kernel linux)
   (firmware (list linux-firmware))


   (keyboard-layout (keyboard-layout "us"))

(bootloader
 (bootloader-configuration
  (bootloader grub-efi-bootloader)
  (targets '("/boot/efi"))
  (timeout 3)
  (keyboard-layout keyboard-layout)))

(file-systems
 (cons* (file-system
         (mount-point "/tmp")
         (device "none")
         (type "tmpfs")
         (check? #f))
        %base-file-systems))

(users (cons* (user-account
               (name "jake")
               (comment "Jake Stannix")
               (group "users")
               (shell (file-append zsh "/bin/zsh"))
               (home-directory "/home/jake")
               (supplementary-groups
                '("wheel" "netdev" "audio" "video" "lp")))
              %base-user-accounts))

(packages
 (append
  (list (specification->package "emacs")
        (specification->package "emacs-exwm")
        (specification->package "git")
        (specification->package "stow")
        (specification->package "neovim")
        (specification->package "sx")
        (specification->package "gcc-toolchain")
        (specification->package "stumpwm")
	(specification->package "make")
        (specification->package "gnupg")
        (specification->package "xauth")
        (specification->package "zsh")
       ;; (specification->package "flatpak")
        (specification->package "curl")
        (specification->package "system-config-printer")
        (specification->package
         "emacs-desktop-environment")
        (specification->package "nss-certs"))
  %base-packages))

(services
 (cons* (service slim-service-type (slim-configuration
                                    (auto-login? #t)
                                    (default-user "jake")
                                    (xorg-configuration
                                     (xorg-configuration
                                      (keyboard-layout keyboard-layout)))))
        (service xfce-desktop-service-type)
        (service cups-service-type)
        (service openssh-service-type)
        (service nix-service-type)
        (extra-special-file "/usr/bin/env"
                            (file-append coreutils "/bin/env"))
        (modify-services %my-desktop-services
                         (delete gdm-service-type))))))
