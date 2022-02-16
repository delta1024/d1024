;;; my-org.el --- ’org-mode’ configuration all in one place

;; Copyright (C) 2022 Jacob Stannix

;; Author: Jacob Stannix
;; Created: 27 Jan 2022

;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public
;; License along with this file. If not, see
;; https://www.gnu.org/licenses/.

;;; Commentary:

;; ’org-mode’ configuration

;;; Code:

(setup (:straight (org-appear
		   :type git
		   :host github
		   :repo "awth13/org-appear")))

(setup (:straight org))
(setup org
 (:also-load org-habit
              org-bullets)
  (:bind "C-c o" consult-outline)
  ;; (:rebind "TAB" org-cycle)
  (:hook my/org-mode-setup
   org-bullets-mode
   org-appear-mode)
  (:when-loaded
    (:option org-ellipsis my/org-ellipsis
	     org-hide-emphasis-markers t
	     org-bullets-bullet-list my/org-bullets
	     org-directory "~/Documents/org/"
	     org-default-notes-file (expand-file-name "Notes.org" org-directory)
	     org-agenda-files '("Task.org" "Appointment.org" "Work.org" "Habits.org")
	     org-log-done 'time
	     org-log-into-drawer t
	     (prepend org-structure-template-alist) '("S" . "src emacs-lisp")  
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
				      "* %^{Issue: }%?")))))

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

(provide 'my-org)
;;; my-org.el ends here
