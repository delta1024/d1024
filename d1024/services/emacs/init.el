(customize-set-variable 'native-comp-async-report-warnings-errors nil)
(defvar my/org-font "DejaVu Serif" "org-mode's variable pitched font name")
(defvar my/user-font "Fira Code" "emacs's fixed width font")
(defvar my/font-size 150 "font size for emacs")

(defvar emacs-startup-time 
  (format "%.2f seconds"
	  (float-time
	   (time-subtract after-init-time before-init-time))) "Emacs start up time")

(defvar emacs-startup-gc
  gcs-done "Number of garbage collections done at statup")

(defun my/display-startup-time ()
  (message "Emacs loaded in %s."
	   emacs-startup-time))

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

(require 'setup)
(setup-define :auto-mode
  (lambda (list-alist)
    `(add-to-list 'auto-mode-alist
		  ',list-alist))
  :documentation "adds pair to auto-mode-alist")

(setup-define :bind-map
  (lambda (map key func)
    `(define-key ,map ,(kbd key) #',func))
  :documentation "Defines key on custom map without setups context"
  :after-loaded t)

(setup-define :hide-mode
  (lambda (&optional mode)
    (let* ((mode (or mode (setup-get 'mode)))
	   (mode (if (string-match-p "-mode\\'" (symbol-name mode))
		     mode
		   (intern (format "%s-mode" mode)))))
      `(setq minor-mode-alist
	     (delq (assq ',mode minor-mode-alist)
		   minor-mode-alist))))
  :documentation "Hide the mode-line lighter of the current mode.
Alternatively, MODE can be specified manually, and override the
current mode."
  :after-loaded t)

(setup-define :evil-collection
  (lambda (evil-mode key binding &optional no-mode?)
    (let* ((active-mode (setup-get 'mode))
	   (active-map (if no-mode? active-mode
	    (if (string-match-p "-mode\\'" (symbol-name active-mode))
			   active-mode
			 (intern (format "%s-mode" active-mode))))))
      `(with-eval-after-load 'evil-collection
	 (evil-collection-define-key ',evil-mode
	   ',(intern (format "%s-map" active-map)) ,(kbd key) ',binding))))
  :documentation "creaes evil collection binding if no-mode? is t then `-mode' will be omitted from map name")

(setup-define :autoload
  (lambda (&rest load-function)
    `(autoload ',(car load-function)
       ,(nth 1 load-function) nil t))
  :documentation "declare autoload for `function` from \"file\"")

(setup-define :load-after
  (lambda (&rest features)
    (let ((body `(require ',(setup-get 'feature))))
      (dolist (feature (nreverse features))
	(setq body `(with-eval-after-load ',feature ,body)))
      body))
  :documentation "Load the current feature after FEATURES.")

(setup-define :straight
  (lambda (recipe)
    `(unless (straight-use-package ',recipe)
       ,(setup-quit)))
  :documentation
  "Install RECIPE with `straight-use-package'.
This macro can be used as HEAD, and will replace itself with the
first RECIPE's package."
  :repeatable t
  :shorthand (lambda (sexp)
	       (let ((recipe (cadr sexp)))
		 (if (consp recipe)
		     (car recipe)
		   recipe))))
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
  (:load-after evil)
  (:autoload dired-jump "dired")
  (:bind "C-x C-j" dired-jump)
  (:also-load dired-x)
  (:also-load dired-single)
  (:also-load all-the-icons-dired)
  (:also-load dired-hide-dotfiles)
  (:hook dired-hide-details-mode)
  (:hook all-the-icons-dired-mode)
  (:hook dired-hide-dotfiles-mode)

  (:option dired-always-read-filesystem t
	   dired-listing-switches "-AGgD --group-directories-first"
	   dired-kill-when-opening-new-dired-buffer t))

(setup (:straight (dired-hide-dotfiles
		   :type git
		   :host github
		   :repo "mattiasb/dired-hide-dotfiles"))
  (:with-mode dired
    (:evil-collection normal "H" dired-hide-dotfiles-mode)))

(setup (:straight (dired-single
		   :type git
		   :host github
		   :repo "crocket/dired-single"))
  (:with-mode dired
    (:evil-collection normal "h" dired-single-up-directory)
    (:evil-collection normal "l" dired-single-buffer)))

(setup (:straight dired-open)
  (:load-after dired)
  (:option dired-open-extensions '(("png" . "sxiv")
				   ("mkv" . "mpv")
				   ("webm" . "mpv")
				   ("odt" . "libreoffice -o"))))

(load (expand-file-name "src/keybindings.el" user-emacs-directory))
(load (expand-file-name "src/org.el" user-emacs-directory))

(setup (:require rainbow-delimiters)
  (:hook-into prog-mode))  

(setup magit
  (:load-after evil))
(define-key my-leader-mode-map (kbd "g") #'magit)

(setup (:require helpful)
  (:global "C-h f" helpful-callable)
  (:global "C-h v" helpful-variable)
  (:global "C-h k" helpful-key))

(defun my/org-mode-visual-fill () 
  (setq visual-fill-column-width 115
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(setup visual-fill-column
  (:load-after org)
  (:with-mode org-mode
    (:hook my/org-mode-visual-fill))
  (:with-mode markdown-mode
    (:hook my/org-mode-visual-fill)))

(setup markdown-mode
  (:autoload gfm-mode "markdown-mode")
  (:auto-mode ("README\\.md\\'" . gfm-mode))
  (:auto-mode ("\\.md\\'" . markdown-mode))
  (:auto-mode ("\\.markdown\\'" . markdown-mode))
  (:option markdown-command "multimarkdown"))

(defun my/minibuffer-backward-kill (arg)
  "When minibuffer is completing a file name delete up to parent
    folder, otherwise delete a character backward"
  (interactive "p")
  (if minibuffer-completing-file-name
      ;; Borrowed from https://github.com/raxod502/selectrum/issues/498#issuecomment-803283608
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
    (delete-backward-char arg)))

(setup vertico
  (:option vertico-cycle t
	   vertico-resize t)
  (:bind-map vertico-map "C-j" vertico-next)
  (:bind-map vertico-map "C-k" vertico-previous)
  (:bind-map minibuffer-local-map "<backspace>" my/minibuffer-backward-kill))

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

(setup (:require which-key)
  (:hide-mode which-key)
  (:option which-key-idle-delay 1))

(setup (:require swiper))

(setup (:require perspective))

(defun configure-emacs ()
  (require 'evil)
  (require 'evil-collection)
  (require 'org)
  (vertico-mode)
  (evil-mode 1)
  (evil-collection-init)
  (persp-mode 1)
  (which-key-mode 1)
  (savehist-mode 1)
  (marginalia-mode 1)
  (pinentry-start)
  (disable-theme 'deeper-blue)
  (load-theme emacs-theme t))

(defun my/post-config () "loads the custom file"
       (load custom-file :noerror)
       (setq my/post-config t))

(configure-emacs)
(my/post-config)


