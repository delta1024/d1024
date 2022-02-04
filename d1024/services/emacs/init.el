(customize-set-variable 'native-comp-async-report-warnings-errors nil)
(require 'my-config)
(customize-set-variable 'cursor-type 'bar)
(add-hook 'emacs-startup-hook #'my/display-startup-time)
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
(setq inhibit-startup-message t)

;; Redirect custom output
(setq custom-file (expand-file-name "emacs-custom.el" user-emacs-directory))

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)          ; Disable the menu bar

(setq emacs-theme 'doom-henna)
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
  ;; (:load-after evil-collection)
  (:autoload dired-jump "dired-x")
  (:bind "C-x C-j" dired-jump)
  (:also-load dired-x
	      dired-single
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

(setup (:straight (dired-single
		   :type git
		   :host github
		   :repo "crocket/dired-single"))
  (:with-mode dired (:remap-key dired-up-directory dired-single-up-directory
				dired-find-file dired-single-buffer)))


(setup (:straight dired-open)
  (:load-after dired)
  (:option dired-open-extensions '(("png" . "sxiv")
				   ("mkv" . "mpv")
				   ("webm" . "mpv")
				   ("odt" . "libreoffice -o"))))

(define-prefix-command 'my-leader-command 'my-leader-mode-map "Shortcuts")
(global-set-key (kbd "C-c k") #'my-leader-command)

(setup keys
  (:my-leader 
   "d"     ((lambda () (interactive) (dired "~/")) "dired ~") 
   ;; "a"     (lambda () (interactive) (start-process-shell-command "alacritty" nil "alacritty --working-directory ~/ -e nu"))
   ;; "A"     (lambda () (interactive) (start-process-shell-command "alacritty" nil "alacritty -e nu"))
   " ;"    execute-extended-command                                
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


(setup visual-fill-column
  (:load-after org)
  (:with-mode org-mode
    (:hook my/org-mode-visual-fill))
  (:with-mode markdown-mode
    (:hook my/org-mode-visual-fill)))

(setup markdown-mode
  (:autoload gfm-mode "markdown-mode")
  (:with-mode gfm-mode
    (:file-match "README\\.md\\'"))
  (:file-match "\\.md\\'"
	       "\\.markdown\\'")
  (:option markdown-command "multimarkdown"))

(setup visual-fill-column
  (:load-after org)
  (:with-mode org-mode
    (:hook my/org-mode-visual-fill))
  (:with-mode markdown-mode
    (:hook my/org-mode-visual-fill)))


(setup vertico
  (:option vertico-cycle t
	   vertico-resize t)
  (:bind-map vertico-map "C-j" vertico-nextk
	     vertico-map "C-k" vertico-previous
	     minibuffer-local-map "<backspace>" my/minibuffer-backward-kill))


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

(setup (:require selectrum))

(setup (:require consult)
  (:global "C-s" consult-line))

(setup marginalia
  (:bind-map minibuffer-local-map "M-A" marginalia-cycle))

(setup (:require lispy)
  (:hook-into emacs-lisp-mode
	      lisp-mode
	      scheme-mode))

;; (setup (:require lispyville)
;;   (:hook-into lispy-mode
;; 	      emacs-lisp-mode
;; 	      lisp-mode
;; 	      scheme-mode))

(setup (:require pass))

(setup (:require pinentry))


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
  (add-hook 'after-init-hook 'global-company-mode))

(setup (:require which-key)
  (:hide-mode which-key)
  (:option which-key-idle-delay 1))

(setup (:require swiper))

(setup (:require perspective))

(defun my/mode-hook ()
  "enables any minor modes that should be loaded after user
settings have been applied"
  (require 'org)
  (vertico-mode)
  ;; (evil-mode 1)
  ;; (evil-collection-init)
  (persp-mode 1)
  (which-key-mode 1)
  (savehist-mode 1)
  (marginalia-mode 1)
  (pinentry-start)
  (disable-theme 'deeper-blue)
  (load-theme emacs-theme t))

(defun my/post-config-hook () "loads the custom file"
       (load custom-file :noerror)
       (setq my/post-config-hook t))

(add-hook 'my/config-hook #'my/mode-hook 99)
(add-hook 'my/config-hook #'my/post-config-hook 100)
(run-hooks 'my/config-hook)
