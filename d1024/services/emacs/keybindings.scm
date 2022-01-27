(define keybindings
  `((setup evil
	   (:option evil-want-integration t
	    evil-want-keybinding 'nil
	    evil-vsplit-window-right t
	    evil-split-window-below t)
	   (:bind-map evil-insert-state-map "C-g" evil-normal-state)
	   (:bind-map evil-visual-state-map "C-g" evil-normal-state)
	   (:bind-map evil-replace-state-map "C-g" evil-normal-state))

    (setup general
	   (:load-after evil)
	   (:option general-evil-setup t)
	   (:global "C-c k" 'my-leader-command)
	   (general-create-definer my/leader-def
				   :keymaps '(normal insert visual emacs)
				   :prefix "C-SPC"
				   :global-prefix "C-SPC"
				   :prefix-command 'my-leader-command
				   :prefix-map 'my-leader-mode-map)
	   (my/leader-def
	    "g"     '(magit :wk "Magit")
	    "f"     '(nil                                                     :wk "file system")
	    "f f"   '(find-file                                               :wk "save-file")
	    "f s"   '(save-buffer                                             :wk "save file")
	    "f r"   '((lambda () (interactive) (find-file "/sudo::"))         :wk "open file as root")

	    "h"     '(nil                                                     :wk "config options")
	    "h e"   '((lambda () (interactive)
			      (dired "~/.system/d1024/d1024/services/emacs"))
		      :wk "emacs configuration")
	    "h s" '((lambda () (interactive)
			    (dired "~/.system/d1024/d1024/systems"))
		    :wk "system configuration")
	    "h h" '((lambda () (interactive)
			    (dired "~/.system/d1024/d1024/services"))
		    :wk "home services")
	    "h d" '((lambda () (interactive)
			    (find-file (expand-file-name "stumpwm.scm" "~/.system/d1024/d1024/services/x11")))
		    :wk "desktop configuration")

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
	    "c"     '(org-capture                                             :wk "change directory")))
    (require 'evil)
    (require 'evil-collection)))
