(define-module (d1024 services emacs org)
  #:use-module (guix gexp)
  #:use-module (d1024 services emacs org capture)
  #:use-module (d1024 services emacs org use-package)
  #:use-module (d1024 services emacs org packages))

(define org-variables
  '((customize-set-variable 'org-directory "~/Documents/org/")
    (setq org-default-notes-file (expand-file-name "Notes.org" org-directory))
    (setq org-agenda-files '("Task.org" "Appointment.org" "Work.org" "Habits.org"))
    (setq org-log-done 'time)
    (setq org-log-into-drawer t)
    (with-eval-after-load 'org
			  (customize-set-variable 'org-structure-template-alist (cons '("S" . "src emacs-lisp") org-structure-template-alist))
			  (customize-set-variable 'org-archive-location ".archive::")
			  (customize-set-variable 'org-babel-load-languages '((emacs-lisp . t) (scheme . t)))
			  (customize-set-variable 'org-timer-default-timer "00:20:00")
			  (customize-set-variable 'org-agenda-custom-commands
						  '(("y" alltodo "System"
						     ((org-directory "~/.dotfiles") (org-agenda-files '("Emacs.org"
													"System.org"
													"Desktop.org"
													"Workflow.org"
													"Environment.org"))))
						    ("d" "Dashboard"
						     ((agenda "")
						      (todo ""
							    ((org-agenda-overriding-header "All Tasks"))))))))))

(define todo-keywords
  '((setq org-todo-keywords
	  '((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d)")
            (sequence "HOLD(h@)" "|" "COMPLETED(c)" "DROED(D@)")
            (sequence "NOT_BOOKED" "|" "BOOKED(@)")
            (sequence "MAYBE" "|" "DEAD(@)")))))

(define org-open-file
  `((defun my/org-open-file (a)  "Opens the file in `org-directory'"
      (interactive (list (read-file-name "What File? " org-directory)))
      (find-file  a))
    ,#~";; (find-file (expand-file-name (concat a \".org\") org-directory))"))

(define-public org-mode
  (append
   org-variables
   todo-keywords
   org-capture
   org-open-file
   org-use-package
   org-packages))
