(define-module (d1024 services stumpwm)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages wm)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp))

(define emacs-mode "\
;;; -*- mode: common-lisp; -*-\n")

(define custom-functions "\
(defun emacs ()
   \"Spawns Emacs\"
   (run-shell-command \"emacsclient -c\"))
(defcommand alacritty ()
    ()
  (run-shell-command \"alacritty\"))\n")

(define startup-message "\
(setf *startup-message* \"Hello\")\n")

(define mode-line "\
(stumpwm:toggle-mode-line (stumpwm:current-screen)
                 	    (stumpwm:current-head))
(setf stumpwm:*screen-mode-line-format*
    (list \"%w | \"
          \"%d\"))\n")

(define startup-programs "\
(run-shell-command \"xsetroot -cursor_name left_ptr\")
(run-shell-command \"/home/jake/.scripts/wallpaper.sh draw\")\n")

(define keymaps "\
(stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd \"C-c\") \"alacritty\")
(stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd \"C-t\") \"send-raw-key\")
(stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd \"c\") \"alacritty\")\n")

(define stumpwmrc
  (list 
   `("config/stumpwm/config"
     ,(plain-file "stumpwmrc"
		  (string-append
		   ;;		   mode-line
		   emacs-mode
		   custom-functions
		   startup-programs
		   keymaps)))))
   
(define-public stumpwm-services
  (list
   (simple-service 'stumpwm-config
		   home-files-service-type
		   stumpwmrc)))
		   
