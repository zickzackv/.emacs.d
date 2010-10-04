;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mac OS X 

;; Display
;; Use MS Consolas Font :-(
(set-default-font "consolas" 't)
(setq inhibit-startup-screen  t)
(tool-bar-mode               -1)
(set-default 'cursor-type  'bar)
(blink-cursor-mode            t)

;; Smooth scrolling
(setq mouse-wheel-scroll-amount '(3 ((shift) . 3))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq scroll-conservatively 10000)

(global-linum-mode t)
(setq linum-format "%5d")				; 5 digits for linenumbers

(show-paren-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Colors
(require 'color-theme)
(load "~/.emacs.d/color-theme-tangotango.el")
(color-theme-tangotango)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rename buffers uniquly with trailing path names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
