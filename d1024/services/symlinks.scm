(define-module (d1024 services symlinks)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (guix gexp))

(define conf-files (string-append (getenv "HOME") "/.system/d1024/files/"))
(define alacritty-yml
  (local-file (string-append conf-files "xapps/alacritty.yml")))
(define neofetch-conf
  (local-file (string-append conf-files "termapps/neofetch.conf")))
(define init-vim
  (local-file (string-append conf-files "termapps/init.vim")))

(define-public default-syms
  (list ;;`("Xmodmap"
	;;  ,xmodmaprc)
	`("config/alacritty/alacritty.yml"
	  ,alacritty-yml)
	`("config/neofetch/config.conf"
	  ,neofetch-conf)
	`("config/nvim/init.vim"
	  ,init-vim)))

(define-public sym-services
  (list
   (simple-service 'symlinks-service
		   home-files-service-type
		   default-syms)))

