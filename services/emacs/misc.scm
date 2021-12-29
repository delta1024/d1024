(define-module (d1024 services emacs misc))
(define visual-fill-colum
  "\
(use-package visual-fill-column
  :after org
  :config
  (defun my/org-mode-visual-fill () 
    (setq visual-fill-column-width 115
          visual-fill-column-center-text t)
    (visual-fill-column-mode 1))
  :hook (org-mode . my/org-mode-visual-fill)
  (markdown-mode . my/org-mode-visual-fill))\n")

(define markdown-mode
  "\
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode ((\"README\\\\.md\\\\'\" . gfm-mode)
         (\"\\\\.md\\\\'\" . markdown-mode)
         (\"\\\\.markdown\\\\'\" . markdown-mode))
  :init (setq markdown-command \"multimarkdown\"))\n")

(define vertico-minibuffer
  "\
(defun my/minibuffer-backward-kill (arg)
    \"When minibuffer is completing a file name delete up to parent
    folder, otherwise delete a character backward\"
    (interactive \"p\")
    (if minibuffer-completing-file-name
        ;; Borrowed from https://github.com/raxod502/selectrum/issues/498#issuecomment-803283608
        (if (string-match-p \"/.\" (minibuffer-contents))
            (zap-up-to-char (- arg) ?/)
          (delete-minibuffer-contents))
      (delete-backward-char arg)))\n")

(define vertico-core
  "\
(use-package vertico
  :init
  (vertico-mode)
  (setq vertico-cycle t)
  (setq vertico-resize t)
  :bind
  (:map vertico-map
        (\"C-j\" . vertico-next)
        (\"C-k\" . vertico-previous))
  (:map minibuffer-local-map
        (\"<backspace>\" . my/minibuffer-backward-kill)))\n")

(define vertico-orderless
  "\
(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))
        selectrum-highlight-candidates-function #'orderless-highlight-matches))

;; Persist history over Emacs restarts. Vertico sorts by history position. 
(use-package savehist
  :init
  (savehist-mode))\n")

(define vertico
  (string-append
   vertico-minibuffer
   vertico-core
   vertico-orderless))

(define selectrum
  "\
(use-package selectrum)\n")

(define consult
  "\
(use-package consult
  :bind
  (\"C-s\" . consult-line))\n")

(define marginalia
  "\
(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (:map minibuffer-local-map
              (\"M-A\" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))\n")

(define pass
  "\
(use-package pass)
(use-package pinentry
  :config
  (pinentry-start))\n")

(define doom-themes
  "\
(use-package doom-themes
  :init
  (disable-theme 'deeper-blue)
  (load-theme (car emacs-theme) t))\n")

;; NOTE: The first time you load your configuration on a new machine,
;; you'll need to run the following command interactively so that mode
;; line icons display correctly:
;;
;; M-x all-the-icons-install-fonts
(define doom-modeline
  "\
(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode t)
  :custom ((doom-mode-line-height 13)))\n")

(define doom
  (string-append
   doom-themes
   doom-modeline))

(define which-key
  "\
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))\n")

(define swiper
  "\
(use-package swiper)\n")

(define perspective
  "\
(use-package perspective
  :init
  (persp-mode))\n")

(define-public misc
  (string-append
   visual-fill-colum
   markdown-mode
   vertico
   selectrum
   consult
   marginalia
   pass
   doom
   which-key
   swiper
   perspective))
