(define-module (d1024 services files)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (guix gexp))
(define-public my-zprofile
  (plain-file "zprofile"
	      "\
if [[ -d \"$HOME/.bin\" ]]; then
    export PATH=\"$PATH:$HOME/.bin\"
fi
. \"$HOME/.bin/guix-activate-profiles\"
. \"$HOME/.bin/guix-activate-xdg-data\"
export $(dbus-launch)"))

(define-public xmodmaprc
  (plain-file "xmodmap"
	      "\
clear lock
clear control
keycode 66 = Control_L
add control = Control_L
add Lock = Control_R
keycode  23 = BackSpace BackSpace BackSpace BackSpace
keycode  22 = Tab ISO_Left_Tab Tab ISO_Left_Tab"))

(define conf-files (string-append (getenv "HOME") "/.system/d1024/files/config/"))
(define-public alacritty-yml
  (local-file (string-append conf-files "alacritty/alacritty.yml")))
(define-public neofetch-conf
  (local-file (string-append conf-files "neofetch/config.conf")))
(define-public init-vim
  (local-file (string-append conf-files "nvim/init.vim")))
(define-public polybar-conf
  (local-file (string-append conf-files "polybar/config")))
(define-public picom-conf
  (local-file (string-append conf-files "picom.conf")))
(define-public redshift-conf
  (local-file (string-append conf-files "redshift.conf")))

