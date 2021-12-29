(define-module (d1024 services packages)
#:use-module (gnu packages base)
#:use-module (gnu packages)
#:use-module (gnu packages fonts)
#:use-module (gnu packages fontutils)
#:use-module (gnu packages admin)
#:use-module (gnu packages xorg)
#:use-module (gnu packages disk)
#:use-module (gnu packages shells)
#:use-module (gnu packages aspell)
#:use-module (gnu packages password-utils)
#:use-module (gnu packages gnupg)
#:use-module (gnu packages compression)
#:use-module (gnu packages vim)
#:use-module (gnu packages emacs))

(define-public wrath-packages
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
