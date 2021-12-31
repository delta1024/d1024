(define-module (d1024 services shells)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu services)
  #:use-module (guix gexp))

(define my-zprofile
  (plain-file "zprofile"
	      "\
if [[ -d \"$HOME/.bin\" ]]; then
    export PATH=\"$PATH:$HOME/.bin\"
fi
# . \"$HOME/.bin/guix-activate-profiles\"
# . \"$HOME/.bin/guix-activate-xdg-data\"
export $(dbus-launch)"))

(define-public shell-services
  (list
    (service home-zsh-service-type
	     (home-zsh-configuration
	      (zprofile `(,my-zprofile))
	      (zshrc `(,(local-file (string-append (getenv "HOME") "/.system/d1024/services/zsh/zshrc"))))))

    (simple-service 'zsh-alias
		    home-files-service-type
		    (list `("config/zsh/aliasrc"
			    ,(local-file (string-append (getenv "HOME") "/.system/d1024/services/zsh/aliasrc")))))))
