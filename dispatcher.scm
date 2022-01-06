(use-modules (gnu services)
	     (gnu home)
	     (gnu home services)
	     (guix gexp))

(define make-target (getenv "D1024_TARGET"))

(define channels
  (local-file 
   (string-append (getenv "HOME") "/.system/d1024/d1024/channels.scm")))

(load (string-append (getenv "HOME") "/.system/d1024/d1024/systems/wrath.scm"))


(use-modules (ice-9 match))
(define (dispatch)
    (match make-target
      ("home-config" wrath-home)
      ("sys-config" wrath-system)))

(dispatch)

