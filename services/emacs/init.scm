(define-module (d1024 services emacs init)
  #:use-module (d1024 services emacs keyboard) 
  #:use-module (d1024 services emacs dired)
  #:use-module (d1024 services emacs org)
  #:use-module (d1024 services emacs dev)
  #:use-module (d1024 services emacs misc)
  #:use-module (gnu services)
  #:use-module (guix gexp))

(define-public early-init
  (plain-file "early-init.el"
	      "\
(setq package-enable-at-startup nil)
(tool-bar-mode -1)          ; Disable the toolbar
(load-theme 'deeper-blue)"))

(define emacs-variables
  "\
(defvar my/org-font \"DejaVu Serif\" \"org-mode's variable pitched font name\")
(defvar my/user-font \"Fira Code\" \"emacs's fixed width font\")
(defvar my/font-size 150 \"font size for emacs\")
(defvar my/emacs-file (expand-file-name  \".dotfiles/Emacs.org\" (getenv \"HOME\")) \"emacs configuration file name\")
(defvar my/guix-file (expand-file-name  \".dotfiles/System.org\" (getenv \"HOME\")) \"GNU Guix configuration file\")
(defvar my/alpha-value '(90 . 90) \"EXWM default alpha value\")
")

(define starup-optimization
  "\
(defvar emacs-startup-time 
  (format \"%.2f seconds\"
          (float-time
           (time-subtract after-init-time before-init-time))) \"Emacs start up time\")
(defvar emacs-startup-gc
  gcs-done \"Number of garbage collections done at statup\")
(defun my/display-startup-time ()
  (message \"Emacs loaded in %s.\"
           emacs-startup-time
           ))
  
(add-hook 'emacs-startup-hook #'my/display-startup-time)\n")

(define straight-use-package
  "\
(add-to-list 'load-path \"~/.config/emacs/my-packages\")
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name \"straight/repos/straight.el/bootstrap.el\" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         \"https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el\"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(require 'use-package)\n")

(define basic-ui
  "\
(setq inhibit-startup-message t)

;; Redirect custom output

(setq custom-file (expand-file-name \"emacs-custom.el\" user-emacs-directory))

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)          ; Disable the menu bar

(setq emacs-theme '(doom-one doom-henna))
;; sets fixed-width font
(set-face-attribute 'default nil :font my/user-font :height my/font-size :weight 'regular)


;; Disables the visual bell
(setq visible-bell t)
(column-number-mode)
(global-display-line-numbers-mode t)
(dolist (mode '(org-mode-hook
                term-mode-hook
                markdown-mode
                eshell-mode-hook
                dired-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))\n")

(define no-littering
  "\
(use-package no-littering
  :config
  (setq auto-save-file-name-transforms
        `((\".*\" ,(no-littering-expand-var-file-name \"auto-save/\") t))))\n")

(define basic-init
  (string-append "\
(customize-set-variable 'native-comp-async-report-warnings-errors nil)\n"
		 emacs-variables
		 starup-optimization 
		 straight-use-package
		 basic-ui
		 no-littering))

(define runtime-optimization
  "\
  (defun my/post-config () \"Sets the `gc-cons-threshold' to a sane value and loads the custom file, among other things\"
         (require 'org)
         (setq gc-cons-threshold (* 2 1000 1000))
         (load custom-file :noerror)
         (setq my/post-config t))
  
    (my/post-config)")

(define-public init
  (plain-file "init.el"
	      (string-append
	       basic-init
	       keyboard
	       dired
	       org-mode
	       development
	       misc
	       runtime-optimization)))
