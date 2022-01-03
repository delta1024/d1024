(define-module (d1024 services packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages dunst)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages compton)
  #:use-module (gnu packages video)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages web-browsers)
  #:use-module (gnu packages image-viewers)
  #:use-module (gnu packages mpd)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages compression)
  #:use-module (nongnu packages mozilla)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages emacs))

(define large-packages
  (list
   firefox))

(define wrath-packages-base
  (list glibc-locales
        font-abattis-cantarell
        font-fira-code
        neofetch
        xrandr
	lf
        zsh
        ispell
        password-store
        pinentry
        zip
        unzip
        htop
        nushell
        fontconfig
        neovim))

(define-public wrath-packages
  (append
   wrath-packages-base))
   ;; large-packages))

(define-public desktop-packages
   (list dunst
	 brightnessctl
	 perl-file-mimeinfo
	 network-manager
	 ;;flatpak
	 picom
	 qutebrowser
	 mpv
	 youtube-dl
	 alacritty
	 xmodmap
	 mpd
	 pavucontrol
	 xclip
	 polybar
	 xwallpaper
	 sxiv
	 redshift))
