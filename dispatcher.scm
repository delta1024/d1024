(use-modules (gnu services)
	     (gnu home)
	     (gnu home services)
	     (guix gexp))

(define make-target (getenv "D1024_TARGET"))
(define system )

(use-modules (ice-9 match))
(define (dispatch)
    (match make-target
      ("home-config" (get-home-config ))
      ("sys-config" (get-system-config ))))
(dispatch)

