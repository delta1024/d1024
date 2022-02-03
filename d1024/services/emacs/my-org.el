(add-hook 'my/config-hook (lambda ()
			    (require 'org)))
(defun my/org-open-file (a)  "Opens the file in `org-directory'"
       (interactive (list (read-file-name "What File? " org-directory)))
       (find-file  a))


(setup (:straight (org-appear
		   :type git
		   :host github
		   :repo "awth13/org-appear")))

(setup (:straight org)
  (:also-load org-habit
              org-bullets)
  (:bind "C-c o" consult-outline)
  (:rebind "Tab" org-cycle)
  (:hook my/org-mode-setup
         org-appear-mode)
  (:option org-ellipsis " ▾"
	   org-hide-emphasis-markers t
	   org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")
           org-directory "~/Documents/org/"
	   org-default-notes-file (expand-file-name "Notes.org" org-directory)
	   org-agenda-files '("Task.org" "Appointment.org" "Work.org" "Habits.org")
	   org-log-done 'time
	   org-log-into-drawer t
	   org-structure-template-alist (cons '("S" . "src emacs-lisp") org-structure-template-alist)
	   org-archive-location ".archive"
	   org-babel-load-languages '((emacs-lisp . t) (scheme . t))
	   org-timer-default-timer "00:20:00")
  (:option org-agenda-custom-commands '(("y" alltodo "System"
					 ((org-directory "~/.dotfiles") (org-agenda-files '("Emacs.org"
											    "System.org"
											    "Desktop.org"
											    "Workflow.org"
											    "Environment.org"))))
					("d" "Dashboard"
					 ((agenda "")
					  (todo ""
						((org-agenda-overriding-header "All Tasks")))))))

  (:option org-todo-keywords'((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d)")
			      (sequence "HOLD(h@)" "|" "COMPLETED(c)" "DROED(D@)")
			      (sequence "NOT_BOOKED" "|" "BOOKED(@)")
			      (sequence "MAYBE" "|" "DEAD(@)")))
  (:option org-capture-templates '(("t" "TODO")
				   ("tg" "General" entry (file+olp "~/Documents/org/Task.org" "General")
				    "* TODO %^{Title}\n %?")
				   ("th" "House" entry (file+olp "~/Documents/org/Task.org" "Household")
				    "* TODO %^{Title}\n")
				   ("tm" "Medical" entry (file+olp "~/Documents/org/Task.org" "Medical")
				    "* %^{Status|NOT_BOOKED|BOOKED} %?\nDoctor: %^{Doctor|Mc'G|Lewis|Shell}\nDate: ")

				   ("c" "Configs")
				   ("ce" "Emacs")
				   ("ceo" "Org" entry (file+olp "~/.dotfiles/Emacs.org" "Inbox" "Org")
				    "* TODO %^{Title}\nDescription: %?")
				   ("cee" "Emacs" entry (file+olp "~/.dotfiles/Emacs.org" "Inbox" "General")
				    "* %^{Title}\n%?")

				   ("cd" "Desktop")
				   ("cdk" "Keybindings" entry (file+olp "~/.dotfiles/Desktop.org" "Inbox" "Keybindings")
				    "* TODO %^{Function: }\nBinding: =%^{Binding}=\nMap: %^{Keymap: }")
				   ("cdw" "Windows" entry (file+olp "~/.dotfiles/Desktop.org" "Inbox" "Windows")
				    "* TODO %^{Window}\nDesired Behaviour:%?")
				   ("cdg" "General" entry (file+olp "~/.dotfiles/Desktop.org" "Inbox" "General")
				    "* TODO %?")

				   ("cs" "System")
				   ("cso" "Os" entry (file+olp "~/.dotfiles/System.org" "Inbox" "Os")
				    "* TODO %^{Title}\n%?")
				   ("csm" "Manifests" entry (file+olp "~/.dotfiles/System.org" "Inbox" "Manifests" "Inbox")
				    "* %^{Package name: }\nManifest: %^{Manifest: }")
				   ("csg" "General" entry (file+olp "~/.dotfiles/System.org" "Inbox" "General")
				    "* TODO %^{Title}")

				   ("cz" "Shells")
				   ("czz" "Zsh" entry (file+olp "~/.dotfiles/Environment.org" "Inbox" "ZSH")
				    "* TODO %^{Title}")

				   ("i" "issues" entry (file "~/Documents/org/Issue.org")
				    "* %^{Issue: }%?"))))
(defun my/org-font-setup ()
  (dolist (face '((org-level-1 . 1.2)
		  (org-level-2 . 1.1)
		  (org-level-3 . 1.05)
		  (org-level-4 . 1.0)
		  (org-level-5 . 1.1)
		  (org-level-6 . 1.1)
		  (org-level-7 . 1.1)
		  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font my/org-font :weight 'regular :height (cdr face)))
  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(defun my/org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode 1))

(setup (:straight emacsql))
(setup (:straight emacsql-sqlite))

(setup (:straight org-roam)
  (:load-after org-mode)
  (:option org-roam-v2-ack t)
  (:when-loaded
    (:option org-roam-directory (expand-file-name "roam" org-directory))
    (org-roam-db-autosync-mode))
  (:bind "C-c n l" org-roam-buffer-toggle
	 "C-c n f" org-roam-node-find
	 "C-c n i" org-roam-node-insert)
  (:bind-into org-mode "C-M-i" completion-at-point))

(setup org-roam-dailies
  (:load-after org-roam))
;; (use-package org-roam
;;   :straight t
;;   :init
;;   (setq org-roam-v2-ack t)
;;   :custom
;;   (org-roam-directory (expand-file-name \"roam\" org-directory))
;;   :bind ((\"C-c n l\" . org-roam-buffer-toggle)
;; 	 (\"C-c n f\" . org-roam-node-find)
;; 	 (\"C-c n i\" . org-roam-node-insert)
;; 	 :map org-mode-map
;; 	 (\"C-M-i\" . completion-at-point)
;; 	 :map org-roam-dailies-map
;; 	 (\"Y\" . org-roam-dailies-capture-yesterday)
;; 	 (\"T\" . org-roam-dailies-capture-tomorrow))
;;   :bind-keymap
;;   (\"C-c n d\" . org-roam-dailies-map)
;;   (\"C-c n d\" . org-roam-dailies-map)
;;   :config
(provide 'my-org)
