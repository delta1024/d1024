(define-module (d1024 packages srwm)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses)
  #:use-module (gnu packages gawk))

(define-public srwm
  (package
   (name "srwm")
   (version "2.10")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/hello/hello-" version
                                ".tar.gz"))
            (sha256
             (base32
              "0ssi1wpaf7plaswqqjwigppsg5fyh99vdlb9kzl7c9lng89ndq1i"))))
   (build-system carg-build-system)
   (arguments '(#:configure-flags '("--enable-silent-rules")))
   (inputs (list gawk))
   (synopsis "Hello, GNU world: An example GNU package")
   (description "Guess what GNU Hello prints!")
   (home-page "https://www.gnu.org/software/hello/")
   (license gpl3+)))
