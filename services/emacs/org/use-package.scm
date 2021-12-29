(define-module (d1024 services emacs org use-package))

(define org-font
  "\
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
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))\n")

(define org-setup
  "\
(defun my/org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode 1))
(setq org-ellipsis \" ▾\")
(setq org-hide-emphasis-markers t)
(setq org-confirm-babel-evaluate nil)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)))\n")

(define config
  (string-append
   org-font
   org-setup))

(define-public org-use-package
  (string-append
   "\
(use-package org
  :straight t
  :no-require t
  :init
  (require 'org-habit)
  :bind ((:map org-mode-map
               (\"C-c o\" . consult-outline)))
  ([remap evil-jump-forward] . org-cycle)
  :hook (org-mode . my/org-mode-setup)
  :config\n"
   config
   "(my/org-font-setup))"))
	    
