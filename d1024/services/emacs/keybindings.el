;; (setup evil
;;   (:option evil-want-integration t
;; 	   evil-want-keybinding 'nil
;; 	   evil-vsplit-window-right t
;; 	   evil-split-window-below t)
;;   (:bind-map evil-insert-state-map "C-g" evil-normal-state)
;;   (:bind-map evil-visual-state-map "C-g" evil-normal-state)
;;   (:bind-map evil-replace-state-map "C-g" evil-normal-state))

(define-prefix-command 'my-leader-command 'my-leader-mode-map "Shortcuts")
(global-set-key (kbd "C-c C-k") #'my-leader-command)
(setup-define :my-leader
  (lambda (key binding)
    `(define-key my-leader-mode-map ,(kbd key) #',binding))
  :documentation "binds KEY to BINDING on `my-leader-mode-map'"
  :repeatable t)

(setup keys
  (:my-leader 
    "f f"   find-file                                               
    "f s"   save-buffer                                             
    "f r"   (lambda () (interactive) (find-file "/sudo::"))

    "h e"   (lambda () (interactive)
		(dired "~/.system/d1024/d1024/services/emacs"))

    "h s" (lambda () (interactive)
	      (dired "~/.system/d1024/d1024/systems"))

    "h h" (lambda () (interactive)
	      (dired "~/.system/d1024/d1024/services"))

    "h d" (lambda () (interactive)
	      (find-file (expand-file-name "stumpwm.scm" "~/.system/d1024/d1024/services/x11")))

    "d"     (lambda () (interactive) (dired "~/")) 
    "a"     (lambda () (interactive) (start-process-shell-command "alacritty" nil "alacritty --working-directory ~/ -e nu"))
	    
    "A"     (lambda () (interactive) (start-process-shell-command "alacritty" nil "alacritty -e nu"))
	    
    " ;"    execute-extended-command                                
    "w f"   delete-frame                                           
    "b"     consult-buffer                                          
    ;;"M-b"   '(ivy-switch-buffer                                       :wk "switch buffer")
    "C-s"   (lambda () (interactive) (guix))                        
    "o f"   my/org-open-file                                        
    "o a"   org-agenda                                              
    "c"     org-capture))

;; (setup (:require general)	     ;
;;   ;; (:load-after evil)
;;   ;; (:option general-evil-setup t)
;;   (:global "C-c C-k" 'my-leader-command)
;;   (general-create-definer my/leader-def
;;     ;; :keymaps '(normal insert visual emacs)
;;     :prefix "C-c C-k"
;;     :global-prefix "C-c C-k"
;;     :prefix-command 'my-leader-command
;;     :prefix-map 'my-leader-mode-map)
;;   (my/leader-def

 
