(use-modules (gnu services)
	     (gnu home)
	     (gnu home services)
	     (guix gexp)
	     (d1024 systems wrath)) 	;; See d1024/systems/wrath.scm for a working example

(define make-target (getenv "D1024_TARGET"))
(define system wrath-system)

(use-modules (ice-9 match))
(define (dispatch)
    (match make-target
      ("home-config" (get-home-config system))
      ("sys-config" (get-system-config system))))
(dispatch)

