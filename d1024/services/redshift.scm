(define-module (d1024 services redshift)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu services configuration)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1))

(define (uglify-field-name field-name)
  (let ((str (symbol->string field-name)))
        str));)
(define (serialize-string field-name value)
  #~(string-append #$(uglify-field-name field-name) "=" #$value "\n"))

(define (serialize-integer field-name value)
  (serialize-string field-name (number->string value)))

(define (serialize-list-manual-configuration field-name value)
  #~(string-append "\n[manual]\n"
		   #$@(map (cut serialize-configuration <>
                                manual-configuration-fields)
                           value)))

(define (serialize-redshift-configuration configuration)
   (mixed-text-file
      "contactrc"
      #~(string-append "[redshift]\n"
                       #$(serialize-configuration
			  configuration redshift-configuration-fields))))

(define list-manual-configuration? list?)

(define-maybe string)
(define-maybe integer)

(define-configuration manual-configuration
  (lat
   (string)
   "the latitude")
  (lon
   (string)
   "the longitude"))
(define-configuration redshift-configuration
  (dawn-time
   (string)
   "time to turn off redshift")
  (dusk-time
   (string)
   "time to turn on redshift")
  (location-provider
   (string)
   "the location provider to use")
  (manual
   (list-manual-configuration '())
   "the manual config to use"))

(define my-redshift
  (redshift-configuration
   (dawn-time "07:00")
   (dusk-time "20:00") 
   (location-provider "manual")
   (manual (list
	    (manual-configuration
	     (lat "53.544388")
	     (lon "-113.490929"))))))

(define (redshift-conf)
  (serialize-redshift-configuration my-redshift))

(define redshift-shepherd
  (shepherd-service
   (documentation "runs redshift to conftrol bluelight levels")
   (provision '(redshift))
   (start #~(make-forkexec-constructor
	     (list #$(file-append redshift "/bin/redshift"))))
   (stop #~(make-kill-destructor))))

(define-public redshift-services
  (list
   (simple-service 'redshift-file
		   home-files-service-type
		   (list
		    `("config/redshift.conf"
		      ,(redshift-conf))))
   (simple-service 'redshift-service
		   home-shepherd-service-type
		   (list
		    redshift-shepherd))))
