(define-module (d1024 services emacs)
  #:use-module (d1024 services emacs init)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home-services emacs)
  #:use-module (srfi srfi-11)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix channels)
  #:use-module (guix transformations)
  #:use-module (rde packages)
  #:use-module (gnu home-services emacs)
  ;; #:use-module (guix home-services files)
  #:use-module (guix download))
(define packages
  (map (compose specification->package)
		      '("emacs-guix"
		       "emacs-org-bullets"
		       "emacs-ace-window"
		       "emacs-org-roam"
		       "emacs-use-package"
		       "emacs-no-littering"
		       "emacs-evil"
		       "emacs-evil-collection"
		       "emacs-general"
		       "emacs-all-the-icons-dired"
		       "emacs-lsp-mode"
		       "emacs-rustic"
		       "emacs-visual-fill-column"
		       "emacs-markdown-mode"
		       "emacs-vertico"
		       "emacs-orderless"
		       "emacs-selectrum"
		       "emacs-consult"
		       "emacs-marginalia"
		       "emacs-ivy"
		       "emacs-ivy-rich"
		       "emacs-projectile"
		       "emacs-counsel-projectile"
		       "emacs-magit"
		       ;;"emacs-pass"
		       "emacs-pinentry"
		       "emacs-helpful"
		       "emacs-doom-themes"
		       "emacs-all-the-icons"
		       "emacs-doom-modeline"
		       "emacs-rainbow-delimiters"
		       "emacs-which-key"
		       "emacs-swiper"
                       "emacs-pdf-tools"
		       "emacs-perspective")))
(define-public emacs-services
  (list 
   (service  home-emacs-service-type
	     (home-emacs-configuration
	      (elisp-packages
	       packages)
	      (server-mode? #t)
	      (early-init-el
	       early-init)
	      (init-el
	       init)))))
	
