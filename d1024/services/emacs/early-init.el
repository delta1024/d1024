(setq package-enable-at-startup nil)
(setq cursor-type 'bar)
(setq emacs-theme 'deeper-blue)
(add-to-list 'load-path (expand-file-name "src" user-emacs-directory))
(tool-bar-mode -1)
(load-theme emacs-theme)
(customize-set-variable 'cursor-type 'bar)
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)          ; Disable the menu bar
(setq inhibit-startup-message t)
