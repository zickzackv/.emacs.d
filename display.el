;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Display
(setq inhibit-startup-screen  t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-default 'cursor-type 'bar)
(blink-cursor-mode t)

;; Smooth scrolling
(setq mouse-wheel-scroll-amount '(3 ((shift) . 3))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq scroll-conservatively 10000)

(global-linum-mode t)
(setq linum-format "%5d")       ; 5 digits for linenumbers

(defcustom linum-disabled-modes-list '(eshell-mode 
                     wl-summary-mode
                     compilation-mode 
                     org-mode
                     text-mode
                     dired-mode
										 rcirc-mode)
  "* List of modes disabled when global linum mode is on"
  :type '(repeat (sexp :tag "Major mode"))
  :tag " Major modes where linum is disabled: "
  :group 'linum
  )

(defcustom linum-disable-starred-buffers 't
  "* Disable buffers that have stars in them like *Gnu Emacs*"
  :type 'boolean
  :group 'linum)

(defun linum-on ()
  "* When linum is running globally, disable line number in modes
defined in `linum-disabled-modes-list'. Changed by linum-off.
Also turns off numbering in starred modes like *scratch*"

  (unless (or (minibufferp)
        (member major-mode linum-disabled-modes-list)
              (string-match "*" (buffer-name)))
    (linum-mode 1)))

(show-paren-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Colors
(require 'color-theme)
(color-theme-initialize)
;(load "~/.emacs.d/color-theme-tangotango/color-theme-tangotango.el")
(load "~/.emacs.d/color-theme-classic-mac/classic-mac.el")
(color-theme-mac-classic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rename buffers uniquly with trailing path names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

