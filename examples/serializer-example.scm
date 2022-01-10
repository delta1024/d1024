(use-modules (gnu services)
	     (guix gexp)
	     (gnu home services)
	     (gnu services configuration)
	     (srfi srfi-26)
	     (srfi srfi-1))

;; Turn field names, which are Scheme symbols into strings
(define (uglify-field-name field-name)
  (let ((str (symbol->string field-name)))
    ;; field? -> is-field
    ;; (if (string-suffix? "?" str)
    ;;     (string-append "is-" (string-drop-right str 1))
        str));)

(define (serialize-string field-name value)
  #~(string-append #$(uglify-field-name field-name) " = " #$value "\n"))

(define (serialize-integer field-name value)
  (serialize-string field-name (number->string value)))

(define (serialize-boolean field-name value)
  (serialize-string field-name (if value "true" "false")))

(define (serialize-contact-name field-name value)
  #~(string-append "\n[" #$value "]\n"))


(define (serialize-list-of-contact-configurations field-name value)
  #~(string-append #$@(map (cut serialize-configuration <>
                                contact-configuration-fields)
                           value)))

(define (serialize-contacts-list-configuration configuration)
  ;; (let ((gexp-config (serialize-configuration
  ;; 		      configuration contacts-list-configuration-fields)))
   (mixed-text-file
      "contactrc"
      #~(string-append "[Owner]\n"
                       #$(serialize-configuration
			  configuration contacts-list-configuration-fields))))

(define list-of-contact-configurations? list?)
;;   (every contact-configuration? lst))

(define-maybe integer)
(define-maybe string)

(define-configuration contact-configuration
  (name
   (string)
   "The name of the contact."
   serialize-contact-name)
  (phone-number
   (maybe-integer 'disabled)
   "The person's phone number.")
  (email
   (maybe-string 'disabled)
   "The person's email address.")
  (married?
   (boolean #f)
   "Whether the person is married."))

(define-configuration contacts-list-configuration
  (name
   (string "")
   "The name of the owner of this contact list."
   serialize-contact-name)
  (email
   (maybe-string 'disabled)
   "The owner's email address.")
  (contacts
   (list-of-contact-configurations '())
   "A list of @code{contact-configuation} records which contain
     information about all your contacts."))


(define my-contact
  (contacts-list-configuration
   (name "Alice")
   (email "alice@example.org")
   (contacts
    (list (contact-configuration
           (name "Bob")
           (phone-number 1234)
           (email "bob@gnu.org")
           (married? #f))
          (contact-configuration
           (name "Charlie")
           (phone-number 0000)
           (married? #t))))))

(define-public test-service
  (list
   (simple-service 'redshift-service-test
		   home-files-service-type
		   (list
		    `("config/test.conf"
		      ,(serialize-contacts-list-configuration my-contact))))))
