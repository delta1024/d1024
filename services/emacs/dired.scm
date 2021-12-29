(define-module (d1024 services emacs dired))

(define dired-base
  "\
(use-package dired
  :after evil
  :demand t
  :commands (dired dired-jump)
  :hook (dired-mode . dired-hide-details-mode)
  :bind ((\"C-x C-j\" . dired-jump))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    \"h\" 'dired-up-directory
    \"l\" 'dired-find-file)
  (setq dired-always-read-filesystem t)
  :custom ((dired-listing-switches \"-AGgD --group-directories-first\")
           (dired-kill-when-opening-new-dired-buffer t)))\n")

(define single-icons-dotfiles
  "\
(use-package dired-single
  :straight t)

(evil-collection-define-key 'normal 'dired-mode-map
  \"h\" 'dired-single-up-directory
  \"l\" 'dired-single-buffer)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-hide-dotfiles
  :straight t
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    \"H\" 'dired-hide-dotfiles-mode))\n")

(define dired-open
  "\
(use-package dired-open
  :straight t
  :config
  ;; Doesn't work as expected!
  ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
  ;; -- OR! --
  (setq dired-open-extensions '((\"png\" . \"sxiv\")
                                (\"mkv\" . \"mpv\")
                                (\"webm\" . \"mpv\")
                                (\"odt\" . \"libreoffice -o\"))))\n")
(define-public dired
  (string-append
   dired-base
   single-icons-dotfiles
   dired-open
   ))
