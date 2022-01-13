(define-module (d1024 services x11 polybar)
  #:use-module (guix gexp) 
  #:use-module (gnu services shepherd) 
  #:use-module (gnu packages wm)
  #:use-module (gnu services) 
  #:use-module (gnu services configuration) 
  #:use-module (gnu home services) 
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-26) 
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:use-module (gnu home services shepherd))

(define (serialize-pair value)
  (define (serialize-list-element elem)
	(string-append (car elem) " = " (cdr elem)))
	      ;;#:max-expr-width 79))
  #~(string-append
	 #$(serialize-list-element value)
	 "\n"))


(define list-of-variables? list?)
(define module-name? string?)

(define (serialize-list-of-variables field-name value)
  #~(string-append #$@(map serialize-pair
			   value)))

(define (serialize-module-name field-name value)
  #~(string-append "\n[" #$value "]\n"))

(define list-of-polybar-modules? list?)

(define (serialize-list-of-polybar-modules field-name value)
  #~(string-append #$@(map (cut serialize-configuration <>
				polybar-module-fields)
			   value)))

(define (serialize-polybar-configuration configuration)
  (mixed-text-file
   "polybar"
   #~(string-append ";; This file is generated from the module (d1024 services x11 polybar)\n;; please see commentary there.\n"
		    #$(serialize-configuration
		       configuration polybar-configuration-fields))))

(define-configuration polybar-module
  (name
   (module-name)
   "The name of the module")
  (args
   (list-of-variables '())
   "An list of cons pairs, the first element in each pair is the variable, the second it's value."))

(define-configuration polybar-configuration
  (modules
   (list-of-polybar-modules '())
   "list of polybar modules that define the configuration"))
   
(define my-polybar
  (polybar-configuration
   (modules
    (list
     (polybar-module
      (name "settings")
      (args
       '(("screenchange-reload" . "false"))))

     (polybar-module
      (name "global/wm")
      (args
       '(("margin-top" . "0")
	 ("margin-bottom" . "0"))))

     (polybar-module
      (name "colors")
      (args
       '(("background" . "#f0232635")
	 ("background-alt" . "#576075")
	 ("foreground" . "#A6Accd")
	 ("foreground-alt" . "#555")
	 ("primary" . "#ffb52a")
	 ("secondary" . "#e60053")
	 ("alert" . "#bd2c40")
	 ("underline-1" . "#c792ea"))))

     (polybar-module
      (name "bar/panel")
      (args
       '(("width" . "100%")
	 ("height" . "20")
	 ("offset-x" . "0")
	 ("offset-y" . "0")
	 ("fixed-center" . "true")
	 ("enable-ipc" . "true")

	 ("background" . "${colors.background}")
	 ("foreground" . "${colors.foreground}")

	 ("line-size" . "2")
	 ("line-color" . "#f00")

	 ("border-size" . "0")
	 ("border-color" . "#00000000")

	 ("padding-top" . "5")
	 ("padding-left" . "1")
	 ("padding-right" . "1")

	 ("module-margin" . "1")

	 ("font-0" . "\"Cantarell:size=14:weight=bold;2\"")
	 ("font-1" . "\"Font Awesome:size=12;2\"")
	 ("font-2" . "\"Material Icons:size=18;5\"")
	 ("font-3" . "\"Fira Mono:size=11;-3\"")

	 ("modules-right" . "cpu temperature battery date")

	 ("tray-position" . "right")
	 ("tray-padding" . "2")
	 ("tray-maxsize" . "28")

	 ("cursor-click" . "pointer")
	 ("cursor-scroll" . "ns-resize"))))
	 ;; ("# modules-left" . "exwm-workspace"))))

     ;; (polybar-module
     ;;  (name "module/exwm-workspace")
     ;;  (args
     ;;   '(("type" . "custom/ipc")
     ;; 	 ("hook-0" . "emacsclient -e \"exwm-workspace-current-index\" | sed -e 's/^\"//' -e 's/\"$//'")
     ;; 	 ("initial" . "1")
     ;; 	 ("format-underline" . "${colors.underline-1}")
     ;; 	 ("format-padding" . "\" \""))))

     (polybar-module
      (name "module/cpu")
      (args
       '(("type" . "internal/cpu")
	 ("interval" . "2")
	 ("format" . "<label> <ramp-coreload>")
	 ("format-underline" . "${colors.underline-1}")
	 ("click-left" . "emacsclient -e \"(proced)\"")
	 ("label" . "%percentage:2%%")
	 ("ramp-coreload-spacing" . "0")
	 ("ramp-coreload-0" . "▁")
	 ("ramp-coreload-0-foreground" . "${colors.foreground-alt}")
	 ("ramp-coreload-1" . "▂")
	 ("ramp-coreload-2" . "▃")
	 ("ramp-coreload-3" . "▄")
	 ("ramp-coreload-4" . "▅")
	 ("ramp-coreload-5" . "▆")
	 ("ramp-coreload-6" . "▇"))))

     (polybar-module
      (name "module/date")
      (args
       '(("type" . "internal/date")
	 ("interval" . "5")

	 ("date" . "\"%a %b %e\"")
	 ("date-alt" . "\"%A %B %d %Y\"")

	 ("time" . "%l:%M %p")
	 ("time-alt" . "%H:%M:%S")

	 ("format-prefix-foreground" . "${colors.foreground-alt}")
	 ("format-underline" . "${colors.underline-1}")

	 ("label" . "%date% %time%"))))

     (polybar-module
      (name "module/battery")
      (args
       '(("type" . "internal/battery")
	 ("battery" . "BAT0")
	 ("adapter" . "ADP1")
	 ("full-at" . "98")
	 ("time-format" . "%-l:%M")

	 ("label-charging" . "%percentage%% / %time%")
	 ("format-charging" . "<animation-charging> <label-charging>")
	 ("format-charging-underline" . "${colors.underline-1}")

	 ("label-discharging" . "%percentage%% / %time%")
	 ("format-discharging" . "<ramp-capacity> <label-discharging>")
	 ("format-discharging-underline" . "${self.format-charging-underline}")

	 ("format-full" . "<ramp-capacity> <label-full>")
	 ("format-full-underline" . "${self.format-charging-underline}")

	 ("ramp-capacity-0" . "")
	 ("ramp-capacity-1" . "")
	 ("ramp-capacity-2" . "")
	 ("ramp-capacity-3" . "")
	 ("ramp-capacity-4" . "")

	 ("animation-charging-0" . "")
	 ("animation-charging-1" . "")
	 ("animation-charging-2" . "")
	 ("animation-charging-3" . "")
	 ("animation-charging-4" . "")
	 ("animation-charging-framerate" . "750"))))

     (polybar-module
      (name "module/temperature")
      (args
       '(("type" . "internal/temperature")
	 ("thermal-zone" . "0")
	 ("warn-temperature" . "60")

	 ("format" . "<label>")
	 ("format-underline" . "${colors.underline-1}")
	 ("format-warn" . "<label-warn>")
	 ("format-warn-underline" . "${self.format-underline}")

	 ("label" . "%temperature-c%")
	 ("label-warn" . "%temperature-c%!")
	 ("label-warn-foreground" . "${colors.secondary}"))))))))

(define polybar-shepherd
  (shepherd-service
   (documentation "runs polybar in background")
   (provision '(polybar))
   (start #~(make-forkexec-constructor
	      (list #$(file-append polybar "/bin/polybar")
		    "panel")))
   (stop #~(make-kill-destructor))))

(define polybar-conf
  (local-file (string-append
	       (getenv "HOME")
	       "/.system/d1024/d1024/files/xapps/polybar.config")))

(define-public polybar-services
  (list
   (simple-service 'polybar-conf-file
		   home-files-service-type
		   (list
		    `("config/polybar/config"
		      ,(serialize-polybar-configuration my-polybar))))
   (simple-service 'polybar-service
		   home-shepherd-service-type
		   (list
		    polybar-shepherd))))
