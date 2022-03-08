(customize-set-variable 'native-comp-async-report-warnings-errors nil)
(require 'my-config)

(add-hook 'emacs-startup-hook #'my/display-startup-time)

(setq tramp-default-method "ssh")

(customize-set-variable 'projectile-dynamic-mode-line 'nil "Interferes with Tramp")

(electric-pair-mode t)

(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(require 'my-setup)


;; Redirect custom output
(setq custom-file (expand-file-name "emacs-custom.el" user-emacs-directory))

(setq emacs-theme  'doom-acario-dark)

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
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setup (:require no-littering)
  (:option auto-save-file-name-transforms
	   `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(setup dired
  (:autoload dired-jump "dired-x")
  (:bind "C-x C-j" dired-jump)
  (:also-load dired-x
	      ;; dired-single
	      all-the-icons-dired
	      dired-hide-dotfiles)
  (:hook dired-hide-details-mode
	 dired-hide-dotfiles-mode
	 all-the-icons-dired-mode)

  (:option dired-always-read-filesystem t
	   dired-listing-switches "-AGgD --group-directories-first"
	   dired-kill-when-opening-new-dired-buffer t))

(setup (:straight (dired-hide-dotfiles
		   :type git
		   :host github
		   :repo "mattiasb/dired-hide-dotfiles"))
  (:bind-into dired "C-k" dired-hide-dotfiles-mode))
;; (:with-mode dired
;;   (:evil-collection normal "C-k" dired-hide-dotfiles-mode)))

;; (setup (:straight (dired-single
;; 		   :type git
;; 		   :host github
;; 		   :repo "crocket/dired-single"))
;;   (:with-mode dired (:remap-key dired-up-directory dired-single-up-directory
;; 				dired-find-file dired-single-buffer)))

(setup (:straight dired-open)
  (:load-after dired)
  (:option dired-open-extensions '(("png" . "sxiv")
				   ("mkv" . "mpv")
				   ("webm" . "mpv")
				   ("odt" . "libreoffice -o"))))

(define-prefix-command 'my-leader-command 'my-leader-mode-map "Shortcuts")
(global-set-key (kbd "C-c c") #'my-leader-command)


(setup keys
  (:my-leader 
   "d"     ((lambda () (interactive) (dired "~/")) "dired ~") 
   "p"     ((lambda () (interactive) (dired "~/Projects")) "")
   "P"     ((lambda () (interactive) (dired "~/Projects/Code/The C Programing Language")) "")
   ";"     execute-extended-command                                
   "w"     delete-frame                                           
   "b"     consult-buffer                                          
   "C-s"   ((lambda () (interactive) (guix)) "guix")
   "C"     my/reconfigure-system
   "c"     org-capture)
  (:my-leader-group "f" "File Related Stuff"
		    '(("f" find-file)
		      ("s" save-buffer)
		      ("r" (lambda (file)
			     (interactive "FFile to open as root:")
			     (let ((sudo-path (format "/sudo::%s" (expand-file-name file))))
			       (find-file "/sudo::")))
		       "Opens a file as root"))
		    "h" "System Config"
		    '(("e" (lambda () (interactive)
			     (dired "~/.system/d1024/d1024/services/emacs")) "Emacs Configuration Dir")
		      ("s" (lambda () (interactive)
			     (dired "~/.system/d1024/d1024/systems")) "Systems Configuration Dir")
		      ("h" (lambda () (interactive)
			     (dired "~/.system/d1024/d1024/services")) "Services Configuration dir")
		      ("d" (lambda () (interactive)
			     (find-file (expand-file-name "stumpwm" "~/.system/d1024/d1024/services/x11")))
		       "Open Stumpwm Config"))
		    "o" "Org Mode"
		    '(("f" my/org-open-file)
		      ("a" org-agenda))))

(require 'my-org)
(setup (:require rainbow-delimiters)
  (:hook-into prog-mode))  

(setup magit
  (:my-leader
   "g" magit))

(setup (:require helpful)
  (:global "C-h f" helpful-callable
	   "C-h v" helpful-variable
	   "C-h k" helpful-key))

(define-prefix-command 'my-eglot-prefix 'my-eglot-map "eglot LSP bindings")
(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "C-c l") #'my-eglot-prefix))
(define-key my-eglot-map (kbd "f") #'eglot-format)
(define-key my-eglot-map (kbd "F") #'eglot-format-buffer)
(define-key my-eglot-map (kbd "o") #'eglot-code-action-organize-imports)
(define-key my-eglot-map (kbd ".") #'xref-find-definitions)

(setup c-mode
  (:hook eglot-ensure)
  (:option eglot-send-changes-idle-time 0.25))

(auto-insert-mode 1)

;;;;;; Makefile ;;;;;;;

(define-auto-insert
  '("header\\.mk" . "Makefile Header")
  '(nil
    "CC = " (skeleton-read "Compiler (default 'gcc'): ") | "gcc" \n
    "CFLAGS = " (skeleton-read "CFlags (default '-Wall -g -o'): ") | "-Wall -g -o" \n
    "IN = " (skeleton-read "Source Directory (default 'src'): ") | "src" \n
    "OUT = " (skeleton-read "Output Directory (default 'bin'):") | "bin" \n
    "TARGETS = " (skeleton-read "Targets: ") \n
    "TARGETS := $(addprefix $(OUT)/,$(TARGETS))"))

(define-auto-insert
  '("main\\.mk" . "Main Executable Makefile")
  '("Executable Name: "
    \n \n
    "$(OUT)/" str ": $(IN)/main.c $(DEPENDENCIES) | $(OUT)" \n
    "	$(CC) $^ $(CFLAGS) $@"))

(define-auto-insert
  "Makefile"
  '(nil
    "include mk/header.mk"\n \n
    "define FILES" \n
    "main.mk" \n
    "endef" \n 
    "FILES := $(addprefix mk/,$(FILES))" \n \n
    ".PHONY: clean all view" \n \n
    "include $(FILES)" \n \n
    "all: $(TARGETS) | $(OUT)" \n \n
    "$(OUT):" \n
    "	mkdir $@" \n \n
    "include $(FILES)"\n \n
    "clean:" \n
    "	rm -rf $(OUT)" \n \n
    "view:" \n
    "    @echo -e \"\\n\" | cat mk/header.mk - Makefile - $(FILES) | less"
    (mkdir "mk")))

(define-skeleton makefile-skel-add-binary
  "inserts a binary definition"
  "Binary Name: "
  str \n
  "$(OUT)/" str ": $(IN)/$(basename " str ").c | $(OUT)" \n
  "	$(CC) $^ $(CFLAGS) $@")

(define-skeleton makefile-skel-add-dependency
  "inserts a dependency block to your makefile"
  "Dependency base name: "
  - str \n
  "$(OUT)/" str ": $(IN)/$(basename " str ").c"
  (when (y-or-n-p "Is there a header file?")
    (concat " $(IN)/$(basename " str ").h"))
  " | $(OUT) " \n
  "	$(CC) $< -c $(CFLAGS) $@")

(define-prefix-command 'makefile-skel-prefix)
(define-key makefile-skel-prefix (kbd "d") #'makefile-skel-add-dependency)
(define-key makefile-skel-prefix (kbd "b") #'makefile-skel-add-binary)

(with-eval-after-load 'make-mode
  (define-key makefile-mode-map (kbd "C-c i") #'makefile-skel-prefix))

(setup visual-fill-column
  (:load-after org)
  (:option visual-fill-column-center-text t)
  (:with-mode org-mode
    (:hook visual-fill-column-mode))
  (:with-mode markdown-mode
    (:hook visual-fill-column-mode)))

(setup markdown-mode
  (:autoload gfm-mode "markdown-mode")
  (:with-mode gfm-mode
    (:file-match "README\\.md\\'"))
  (:file-match "\\.md\\'"
	       "\\.markdown\\'")
  (:option markdown-command "multimarkdown"))

(setup vertico
  (:option vertico-cycle t
	   vertico-resize t)
  (:bind-map vertico-map "C-j" vertico-nextk
	     vertico-map "C-k" vertico-previous))
(add-hook 'my/config-hook #'vertico-mode)

(setup (:require embark)
  (:autoload embark-prefix-help-command "embark")
  (:global "C-." embark-act
	   "M-." embark-dwim
	   "C-h B" embark-bindings)
  (:option prefix-help-command #'embark-prefix-help-command))

(setup (:require orderless)
  (:option completion-styles '(orderless)
	   completion-category-defaults nil
	   completion-category-overrides '((file (styles partial-completion)))
	   selectrum-highlight-caneidates-function #'orderless-highlight-matches))

(setup (:require savehist))
(add-hook 'my/config-hook (lambda ()
			    (savehist-mode 1)))
(setup (:require selectrum))

(setup (:require consult)
  (:global "C-s" consult-line))

(setup marginalia
  (:bind-map minibuffer-local-map "M-A" marginalia-cycle))
(add-hook 'my/config-hook (lambda ()
			    (marginalia-mode 1)))
;; (setup (:require lispy)
;;   (:hook-into emacs-lisp-mode
;; 	      lisp-mode
;; 	      scheme-mode))

(setup (:require pass))

(setup (:require pinentry))
(add-hook 'my/config-hook #'pinentry-start)

(setup (:require doom-themes))

;; NOTE: The first time you load your configuration on a new machine,
;; you'll need to run the following command interactively so that mode
;; line icons display correctly:
;;
;; M-x all-the-icons-install-fonts
(setup (:require all-the-icons))
(setup (:require doom-modeline)
  (:option doom-modeline-mode t
	   doom-mode-line-height 13))
(setup (:require company)
  (add-hook 'after-init-hook 'global-company-mode)
  (:option company-selection-wrap-around t))

(setq site-skeletons-map-key (kbd "C-c s"))

(autoload #'site-skeletons-prefix "site-skeletons" nil t 'keymap)
(global-set-key (kbd "C-c s") #'site-skeletons-prefix)

;; (setup (:require swiper))

;;(setup (:require perspective))
;;(add-hook 'my/config-hook (lambda ()) persp-mode 1)

(defun my/post-config-hook () "loads the custom file"
       (load custom-file :noerror)
       (setq my/post-config-hook t)
       (disable-theme 'deeper-blue)
       (load-theme emacs-theme t))

(add-hook 'my/config-hook #'my/post-config-hook 100)
(run-hooks 'my/config-hook)
(require 'org)
