(defvar my/org-font "DejaVu Serif" "org-mode's variable pitched font name")
(defvar my/user-font "Fira Code" "emacs's fixed width font")
(defvar my/font-size 150 "font size for emacs")
(defvar emacs-startup-time 
  (format "%.2f seconds"
	  (float-time
	   (time-subtract after-init-time before-init-time))) "Emacs start up time")

(defvar emacs-startup-gc
  gcs-done "Number of garbage collections done at statup")

(defun my/display-startup-time ()
  (message "Emacs loaded in %s."
	   emacs-startup-time))

(defun my/reconfigure-system (args)
  (interactive "P")
  (let ((default-directory (expand-file-name "" "~/.system")))
    (unless args
      (compile "make -k" t))
    (if (equal args '(4))
	(compile "make sys" t))
    (if (equal args '(16))
	(compile "make build" t))
    ;; (pcase args
    ;;   ((pred (lambda (item)
    ;; 	       (equal item '(16))))
    ;;    (progn
    ;; 	 (eshell-command "make system")
    ;; 	 (with-current-buffer "*Eshell Command Output*"
    ;; 	   (special-mode)))))
    ))

(defun my/org-mode-visual-fill ()  
  (setq visual-fill-column-width 115
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(defun my/minibuffer-backward-kill (arg)
  "When minibuffer is completing a file name delete up to parent
    folder, otherwise delete a character backward"
  (interactive "p")
  (if minibuffer-completing-file-name
      ;; Borrowed from https://github.com/raxod502/selectrum/issues/498#issuecomment-803283608
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
    (delete-backward-char arg)))

(defun my/org-open-file (a)  "Opens the file in `org-directory'"
       (interactive (list (read-file-name "What File? " org-directory)))
       (find-file  a))(defun my/org-font-setup ()
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
  (org-bullets-mode)
  (visual-line-mode 1))
(defvar my/org-ellipsis " ▾")
(defvar my/org-bullets '("◉" "○" "●" "○" "●" "○" "●"))
(provide 'my-config)
