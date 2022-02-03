(require 'setup)

(setup-define :my-leader
  (lambda (key binding)
    (if (commandp binding)
	`(define-key my-leader-mode-map ,(kbd key) #',binding)
      `(let* ((command ,(pop binding))
	      (comment ,(pop binding)))
	 (define-key my-leader-mode-map ,(kbd key) `(,comment . ,command)))))
  :documentation "binds KEY to BINDING on `my-leader-mode-map'"
  :repeatable t)

(setup-define :my-leader-group
  (lambda (prefix-key description key-group)
    `(progn 
       (define-prefix-command ',(intern (format "my-%s-map" prefix-key)))
       (define-key my-leader-mode-map ,(kbd prefix-key) '(,description . ,(intern (format "my-%s-map" prefix-key))))
       (dolist (keys ,key-group)
	 (unless (nth 2 keys)
	   (define-key ,(intern (format "my-%s-map" prefix-key)) (nth 0 keys) (nth 1 keys)))
	 (if (= 3 (length keys))
	     (define-key ,(intern (format "my-%s-map" prefix-key)) (nth 0 keys) `(,(nth 2 keys) . ,(nth 1 keys) ))))))
  
  :repeatable t
  :documentation "@example
(:my-leader-group \"f\" \"File Related Stuff\"
                         '((\"f\" find-file)
                            (\"s\" save-buffer)
                            (\"r\" (lambda () (interactive) (find-file \"/sudo::\")) \"Opens a file as root\"))
@end example ")

(setup-define :remap-key
  (lambda (old-command new-command)
    (let* ((mode (setup-get 'mode))
	   (active-mode (if (string-match-p "-mode\\'" (symbol-name mode))
			    mode
			  (intern (format "%s-mode" mode))))
	   (mode-map (intern (format "%s-map" active-mode))))
      `(with-eval-after-load ',(setup-get 'mode)
	 (define-key ,mode-map [remap ,old-command] ',new-command))))
  :documentation "Remaps old-command to NEW-COMMAND with the current mode map"
  :repeatable t)


(setup-define :bind-map
  (lambda (map key func)
    `(define-key ,map ,(kbd key) #',func))
  :documentation "Defines key on custom map without setups context"
  :repeatable t
  :after-loaded t)

(setup-define :hide-mode
  (lambda (&optional mode)
    (let* ((mode (or mode (setup-get 'mode)))
	   (mode (if (string-match-p "-mode\\'" (symbol-name mode))
		     mode
		   (intern (format "%s-mode" mode)))))
      `(setq minor-mode-alist
	     (delq (assq ',mode minor-mode-alist)
		   minor-mode-alist))))
  :documentation "Hide the mode-line lighter of the current mode.
Alternatively, MODE can be specified manually, and override the
current mode."
  :after-loaded t)

(setup-define :evil-collection
  (lambda (evil-mode key binding &optional no-mode?)
    (let* ((active-mode (setup-get 'mode))
	   (active-map (if no-mode? active-mode
			 (if (string-match-p "-mode\\'" (symbol-name active-mode))
			     active-mode
			   (intern (format "%s-mode" active-mode))))))
      `(with-eval-after-load 'evil-collection
	 (evil-collection-define-key ',evil-mode
				     ',(intern (format "%s-map" active-map)) ,(kbd key) #',binding))))
  :repeatable t
  :documentation "creaes evil collection binding if no-mode? is t then `-mode' will be omitted from map name")

(setup-define :autoload
  (lambda (&rest load-function)
    `(autoload ',(car load-function)
       ,(nth 1 load-function) nil t))
  :documentation "declare autoload for `function` from \"file\"")

(setup-define :load-after
  (lambda (&rest features)
    (let ((body `(require ',(setup-get 'feature))))
      (dolist (feature (nreverse features))
	(setq body `(with-eval-after-load ',feature ,body)))
      body))
  :documentation "Load the current feature after FEATURES.")

(setup-define :straight
  (lambda (recipe)
    `(unless (straight-use-package ',recipe)
       ,(setup-quit)))
  :documentation
  "Install RECIPE with `straight-use-package'.
This macro can be used as HEAD, and will replace itself with the
first RECIPE's package."
  :repeatable t
  :shorthand (lambda (sexp)
	       (let ((recipe (cadr sexp)))
		 (if (consp recipe)
		     (car recipe)
		   recipe)))) 
(provide 'my-setup)
