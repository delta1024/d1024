(define-module (d1024 services emacs org packages))

(define org-bullets
  '((use-package org-bullets
		 :after org
		 :hook (org-mode . org-bullets-mode)
		 :custom
		 (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))))

(define org-roam
  '((use-package emacsql
		 :straight t)
    (use-package emacsql-sqlite
		 :straight t)
    (use-package org-roam
		 :straight t
		 :init
		 (setq org-roam-v2-ack t)
		 :custom
		 (org-roam-directory (expand-file-name "roam" org-directory))
		 :bind (("C-c n l" . org-roam-buffer-toggle)
			("C-c n f" . org-roam-node-find)
			("C-c n i" . org-roam-node-insert)
			:map org-mode-map
			("C-M-i" . completion-at-point)
			:map org-roam-dailies-map
			("Y" . org-roam-dailies-capture-yesterday)
			("T" . org-roam-dailies-capture-tomorrow))
		 :bind-keymap
		 ("C-c n d" . org-roam-dailies-map)
		 ("C-c n d" . org-roam-dailies-map)
		 :config
		 (require 'org-roam-dailies)
		 (org-roam-db-autosync-mode))))

(define org-appear
  '((use-package org-appear
		 :straight '(org-appear
			     :type git
			     :host github
			     :repo "awth13/org-appear")
		 :hook (org-mode . org-appear-mode))))

(define-public org-packages
  (append
   org-bullets
   org-roam
   org-appear))
