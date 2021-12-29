(define-module (d1024 services symlinks)
  #:use-module (d1024 services files)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (guix gexp))

(define-public default-syms
  (list `("Xmodmap"
	  ,xmodmaprc)
	`("config/alacritty/alacritty.yml"
	  ,alacritty-yml)
	`("config/neofetch/config.conf"
	  ,neofetch-conf)
	`("config/nvim/init.vim"
	  ,init-vim)
	`("config/polybar/config"
	  ,polybar-conf)
	`("config/picom.conf"
	  ,picom-conf)
	`("config/redshift.conf"
	  ,redshift-conf)))

(define-public sym-services
  (list
   (simple-service 'symlinks-service
		   home-files-service-type
		   default-syms
			 )))

