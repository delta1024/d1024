(define-module (d1024 services emacs keyboard)
  #:use-module (guix gexp))

(define evil
  `(,#~"(use-package evil"
		 :demand t
		 :init
		 (setq evil-want-integration t)
		 (setq evil-want-keybinding 'nil)
		 (setq evil-vsplit-window-right t)
		 (setq evil-split-window-below t)
		 :config
		 (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
		 (define-key evil-visual-state-map (kbd "C-g") 'evil-normal-state)
		 (define-key evil-replace-state-map (kbd "C-g") 'evil-normal-state)
		 (evil-mode)
		 :bind
		 ,#~"([remap evil-search-forward] . swiper)"
		 ,#~"([remap evil-search-backward] . swiper-backward))"))

(define evil-collection
  '((use-package evil-collection
		 :after evil
		 :config
		 (evil-collection-init))))

(define general
  '((use-package general
		 :after evil
		 :config
		 (general-evil-setup t)
		 (global-set-key (kbd "C-c k") 'my-leader-command))
    (general-create-definer my/leader-def
			    :keymaps '(normal insert visual emacs)
			    :prefix "C-SPC"
			    :global-prefix "C-SPC"
			    :prefix-command 'my-leader-command
			    :prefix-map 'my-leader-map))) 
(define general-leader
  '((my/leader-def
     "f"     '(nil                                                     :wk "file system")
     "f f"   '(find-file                                               :wk "save-file")
     "f s"   '(save-buffer                                             :wk "save file")
     "f r"   '((lambda () (interactive) (find-file "/sudo::"))         :wk "open file as root")

     "h"     '(nil                                                     :wk "config options")
     "h e"   '((lambda () (interactive)
		       (find-file (expand-file-name "Emacs.org" "~/.dotfiles")))
               :wk "emacs configuration")
     "h s" '((lambda () (interactive)
		     (find-file my/guix-file))
             :wk "system configuration")
     "h d" '((lambda () (interactive)
		     (find-file (expand-file-name "Desktop.org" "~/.dotfiles")))
             :wk "desktop configuration")
     "h z" '((lambda () (interactive)
		     (find-file (expand-file-name "Environment.org" "~/.dotfiles")))
             :wk "environment configuration")
     "h r" '((lambda () (interactive)
		     (find-file (expand-file-name "README.org" "~/.dotfiles")))
             :wk "README")
     "h w" '((lambda () (interactive)
		     (find-file (expand-file-name "Workflow.org" "~/.dotfiles")))
             :wk "Org Configruation")

     "d"     '((lambda () (interactive) (dired "~/")) :wk "Dired home")
     "a"     '((lambda () (interactive) (start-process-shell-command "alacritty" nil "alacritty --working-directory ~/ -e nu"))
               :wk "nu ~")
     "A"     '((lambda () (interactive) (start-process-shell-command "alacritty" nil "alacritty -e nu"))
               :wk "nu")
     ";"     '(execute-extended-command                                :wk "M-x")
     "w f"   '(delete-frame                                            :wk "delete fram")
     "b"     '(consult-buffer                                          :wk "switch buffers with preview")
     ;;"M-b"   '(ivy-switch-buffer                                       :wk "switch buffer")
     "C-s"   '((lambda () (interactive) (guix))                        :wk "Guix")
     "o"     '(nil                                                     :wk "org")
     "o f"   '(my/org-open-file                                        :wk "open org file")
     "o a"   '(org-agenda                                              :wk "org agenda")
     "c"     '(org-capture                                             :wk "change directory"))))

(define-public keyboard
  (append
   evil
   evil-collection
   general
   general-leader))
