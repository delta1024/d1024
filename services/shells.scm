(define-module (d1024 services shells)
  #:use-module (d1024 services files)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu services)
  #:use-module (guix gexp))

(define-public shell-services
  (list
    (service home-zsh-service-type
	     (home-zsh-configuration
	      (zprofile `(,my-zprofile))
	      (zshrc `(,(local-file (string-append (getenv "HOME") "/.system/d1024/files/config/zsh/zshrc"))))))

    (simple-service 'zsh-alias
		    home-files-service-type
		    (list `("config/zsh/aliasrc"
			    ,(local-file (string-append (getenv "HOME") "/.system/d1024/files/config/zsh/aliasrc")))))))
