(define-module (d1024 services emacs dev))

(define lsp
  "\
 (use-package lsp-mode
   :commands (lsp lsp-defered)
   :init
   (setq lsp-keymap-prefix \"C-SPC m\")
   :custom
   (lsp-rust-analyzer-store-path (concat (getenv \"HOME\") \"/.nix-profile/bin/rust-analyzer\"))
   :config
   (lsp-enable-which-key-integration t))\n")

(define rustic
  (string-append
  "\
(use-package rustic
  :custom
  (rustic-analyzer-command '(\"rust-analyzer\"))
  (rustic-rustfmt-bin (concat (getenv \"HOME\") \"/.cargo/bin/rustfmt\"))
  (rustic-cargo-bin (concat (getenv \"HOME\") \"/.nix-profile/bin/cargo\")))\n"
;; In order for emacs to see the cargo binary we need to add
;; $HOME/.nix-profile/bin/ to our load path
  "\
(customize-set-variable 'exec-path (add-to-list
                                      'exec-path
                                      (concat (getenv \"HOME\") \"/.nix-profile/bin\")))\n"))

(define rainbow-delim
  "\
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))\n")

(define projectile
  "\
(use-package projectile
  :diminish projectile-mode
  ;;:custom ((projectile-completion-system 'ivy))
  :bind-keymap
  (\"C-c p\" . projectile-command-map))
;; NOTE: Set this to the folder where you keep your Git repos!\n")

(define magit
  "\
(use-package magit
  :config (evil-collection-magit-setup)
  :general
  (:prefix-map 'my-leader-map
               \"g\" '(magit :which-key \"Status\")))\n")
(define helpful
  "\
(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))\n")
(define-public development
  (string-append
   lsp
   rustic
   rainbow-delim
   projectile
   magit
   helpful))
  
