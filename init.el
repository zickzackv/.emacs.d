;;;; This file controlls the initial setup of Aquamacs.
;;;; Old School Emacs load ~/.emacs.el or ~/.emacs.d/init.el.
;;;; Aquamacs loads ~/.emacs. 
;;;; This file is a symbolic link to ~/.emacs.d/init.el under 
;;;; the name ~/.emacs . 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Encoding and Input

(setq default-input-method              "german-postfix") ;toggle with C-\
(setq default-buffer-file-coding-system 'utf-8)
(prefer-coding-system                   'utf-8)
(set-default-coding-systems             'utf-8)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load Path
;;
;; add all modes in ~/.emacs.d/modes to load-path
(setq my-mode-dirs '("color-theme/"
                     "slime" 
					 "org-mode/lisp"
					 "haskell-2.7.0/"
					 "anything/"
                     "mog-git-blame"
					 "egg"
					 "eproject"
					 "rhtml"))

(let* ((modes-path (expand-file-name "~/.emacs.d/modes/"))
       (dirs       (mapcar '(lambda (d)
			      (concat modes-path d))
 			   my-mode-dirs)))
  (add-to-list 'load-path modes-path)
  (mapcar '(lambda (x) 
	     (add-to-list 'load-path x))
	  dirs))

(add-to-list 'exec-path "/opt/local/bin/"); add ports binary dir

;; advices kill-region and kill-ring-save
(load "~/.emacs.d/functions.el")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sending mail from within emacs to google
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq starttls-use-gnutls t)
(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials
      '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials      (expand-file-name "~/.authinfo")
      ;;smtpmail-auth-credentials '(("smtp.gmail.com" 587 "fabian.otto@gmail.com" "star -tv"))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-debug-info nil)
(require 'smtpmail)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Eproject and its proeject definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'eproject)

(define-project-type rails (generic)
  (look-for "Rakefile")
  :relevant-files ("\\.rb$" "\\.erb$" "\\.yml$" "\\.css$")
  :irrelevant-files ("vendor/.*" "tmp/.*" "doc/.*" "log/.*" "script/.*"))

(define-project-type diplom (generic)
  (look-for "Diplom.org")
  :relevant-files ("\\.tex$" "\\.bib$" "\\.hs$"))
      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIT using magit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'magit)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Haskell mode
(load "~/.emacs.d/modes/haskell-mode-2.7.0/haskell-site-file.el")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;; (turn-on-haskell-indentation)		

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown mode
(autoload 'markdown-mode "markdown.el" "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.markdown" . markdown-mode) auto-mode-alist))

;; do not show the markdown window when opening the 
;; preview in the browser
;; uses a advice so that the actual funtion does not have to be
;; modified.
(defadvice markdown-preview (after delete-markdown-buffer ())
  (delete-other-windows))
(ad-activate 'markdown-preview)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ruby mode
;; 
(add-to-list 'auto-mode-alist '("\\.rb" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.builder" . ruby-mode))


(setq-default tab-width 4) ; or any other preferred value

(setq cua-auto-tabify-rectangles nil)
(defadvice align (around smart-tabs activate)
  (let ((indent-tabs-mode nil)) ad-do-it))
(defadvice align-regexp (around smart-tabs activate)
  (let ((indent-tabs-mode nil)) ad-do-it))
(defadvice indent-relative (around smart-tabs activate)
  (let ((indent-tabs-mode nil)) ad-do-it))
(defadvice indent-according-to-mode (around smart-tabs activate)
  (let ((indent-tabs-mode indent-tabs-mode))
    (if (memq indent-line-function
              '(indent-relative
                indent-relative-maybe))
        (setq indent-tabs-mode nil))
 ad-do-it))

(defmacro smart-tabs-advice (function offset)
  (defvaralias offset 'tab-width)
  `(defadvice ,function (around smart-tabs activate)
     (cond
      (indent-tabs-mode
       (save-excursion
         (beginning-of-line)
         (while (looking-at "\t*\\( +\\)\t+")
           (replace-match "" nil nil nil 1)))
       (setq tab-width tab-width)
       (let ((tab-width fill-column)
                 (,offset fill-column))
             ad-do-it))
      (t
       ad-do-it))))

(add-hook 'ruby-mode-hook (lambda ()
			    (setq ruby-insert-encoding-magic-comment nil)))

(smart-tabs-advice ruby-indent-line ruby-indent-level)
(setq ruby-indent-tabs-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YAML mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml" . yaml-mode))
(add-hook 'yaml-mode-hook '(lambda ()
                             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SLIME
(setq inferior-lisp-program "/Users/zickzackv/Source/ccl/dx86cl64")
(require 'slime)
(slime-setup '(slime-repl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Anything 
(require 'anything-config)
(require 'anything-match-plugin)
(require 'anything-gtags)

(defvar anything-c-source-eproject-files
  '((name . "Files in eProject")
    (init . (lambda () (if (buffer-file-name)
			   (setq anything-eproject-root-dir (eproject-maybe-turn-on))
			 (setq anything-eproject-root-dir 'nil)
			 )))
    (candidates . (lambda () (if anything-eproject-root-dir
				 (eproject-list-project-files anything-eproject-root-dir))))
    (type . file)
    )
  "Search for files in the current eProject.")

(defvar anything-c-source-eproject-projects
  '((name . "Projects")
    (candidates . (lambda ()
                    (mapcar (lambda (item)
                              (car item))
                            prj-list)))
    (action ("Open Project" . (lambda (cand)
                                (eproject-open cand)))
            ("Close projcet" . (lambda (cand)
                                 (eproject-close)))))
  "Open or close eProject projects.")

(setq-default anything-for-files-prefered-list 
	      '(
                anything-c-source-ffap-line
		anything-c-source-ffap-guesser
		anything-c-source-file-cache
		anything-c-source-files-in-current-dir+
		anything-c-source-find-files
		anything-c-source-recentf
                ;; anything-c-source-eproject-buffers 
		anything-c-source-buffers+
		anything-c-source-create))

(defun anything-find-my-files ()
  "Preconfigured `anything' for `find-file'."
  (interactive)
  (let* ((fap    (ffap-guesser))
         (file-p (and fap (file-exists-p fap)))
         (tap    (thing-at-point 'filename))
         (input  (if file-p 
		     (expand-file-name tap) 
		   fap))) 
    (anything anything-for-files-prefered-list
	      (or input 
		  (expand-file-name default-directory))
              "Find Files or Url: " nil nil "*Anything Find Files*")))


;;; allows creating of new buffers
(defun anything-for-buffers ()
  "Preconfigured `anything' for buffer."
  (interactive)
  (anything-other-buffer '(anything-c-source-buffers+
                           anything-c-source-buffer-not-found)
			 "*anything for buffers*"))


;; only files and symbols
(defun anything-project ()
  (interactive)
  (anything-at-point '(anything-c-source-imenu
		       anything-c-source-gtags-select
                       anything-c-source-eproject-files
		       anything-c-source-org-headline)))

(defun anything-kill-current-buffer ()
  "Preconfigured `anything' to kill buffer ."
  (interactive)
  (anything
   '(((name . "Kill Buffers")
      (candidates . anything-c-buffer-list)
      (action
       ("Kill Buffer" . (lambda (candidate)
                          (kill-buffer candidate)
                          )))))
   (buffer-name) "buffer: "))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode

;; ;; setting agenda directory and matching regexp
(setq org-agenda-files '("~/Documents/Todo/"))
(setq org-agend-file-regexp "\\`[^.].*\\.org\\'")
(setq org-enforce-todo-dependencies t)  ; headlines with TODO are not DONE
(setq org-hide-leading-stars t)
(setq org-odd-levels-only t)

;; ; use keys to switch state and do not trigger action when using S-<right>
(setq org-use-fast-todo-selection t) 	
(setq org-treat-S-cursor-todo-selection-as-state-change nil)
(setq org-todo-keywords '((sequence "NEXT(n)" "TODO(t)" "STARTED(s!)" "|" "DONE(d!/!)")
 			  (sequence "WAITING(w@/!)" "SOMEDAY(S!)" "|" "CANCELLED(c@/!)")))

(setq org-todo-keyword-faces '(("NEXT" :foreground "white" :background "red" :weight bold)
			       ("TODO" :foreground "red" :weight bold)
			       ("STARTED" :foreground "blue" :weight bold)
			       ("DONE" :foreground "forest green" :weight bold)
			       ("WAITING" :foreground "orange" :weight bold)
			       ("SOMEDAY" :foreground "magenta" :weight bold)
			       ("CANCELLED" :foreground "forest green" :weight bold)))

(setq org-tag-alist '(("@Work" . ?w) ("@Home" . ?h) ("@Uni" . ?u) ("Email" . ?e) ("Telefon" . ?t)))

;; ;; Exporting to ical 
(setq org-combined-agenda-icalendar-file "~/Documents/Todo/org-mode.ics")

;; starting week with mondy
;; (setq org-agenda-start-on-weekday 't)	;
;; (setq calendar-week-start-day 0)	; calendar weeks start on mondays

;; use german postfix  as default input for org mod
(add-hook 'org-load-hook '(lambda ()
			    (set-input-method "german-postfix")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; YaSnippet
(require 'yasnippet-bundle)
(setq yas/root-directory "~/emacs.d/snippets")

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard
(require 'sequential-command)
;; repeated invocation of this command iterates its command list
(define-sequential-command my-end        end-of-line seq-return)
(define-sequential-command my-beginning  back-to-indentation beginning-of-line seq-return)

(global-set-key (kbd "s-§")          'other-frame); additional to s-`
(global-set-key (kbd "s-<return>")   'execute-extended-command)
(global-set-key (kbd "s-<kp-enter>") 'execute-extended-command)
;; using cursor keys to switch between open windows and frames
;; left, right keys switch between frames
;; up, down keys switch to open windows over all frames
(global-set-key (kbd "s-<down>")     'next-multiframe-window)
(global-set-key (kbd "s-<up>")       'previous-multiframe-window)
(global-set-key (kbd "C-e")          'my-end)
(global-set-key (kbd "C-a")          'my-beginning)
								     
;; anything 					     
(global-set-key (kbd "C-x C-z")	     'anything-project) ; anything backup call. E.g. in org-mode
(global-set-key (kbd "s-t")		     'anything-project)
(global-set-key (kbd "C-x k")	     'anything-kill-current-buffer)
(global-set-key (kbd "C-x b")	     'anything-for-buffers) 
(global-set-key (kbd "C-x C-b")	     'anything-for-buffers) ; backup
(global-set-key (kbd "C-x C-f")	     'anything-find-my-files) 

(define-key global-map [menu-bar tools remember] '("Remember Something" . remember-other-frame))

;; org
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)

(global-set-key (kbd "<C-f2>") 'bm-toggle)
(global-set-key (kbd "<f2>") 'bm-next)
(global-set-key (kbd "<S-f2>") 'bm-previous)

(global-set-key (kbd "<f12>") 'magit-status)



(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(make-backup-files nil)  ; do not write backup files ( ./bli.foo~) 
 '(truncate-lines t)
 '(blink-cursor-mode t)
 '(column-number-mode t)
 '(recentf-max-saved-items 40)
 '(recentf-mode t)
 '(recentf-save-file "~/.emacs.d/recent-files")
 '(ruby-insert-encoding-magic-comment nil)
 '(show-paren-mode t))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "blue" :foreground "black")))))

