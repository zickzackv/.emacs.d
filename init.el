;;;; This file controlls the initial setup of Aquamacs.
;;;; Old School Emacs load ~/.emacs.el or ~/.emacs.d/init.el.
;;;; Aquamacs loads ~/.emacs. 
;;;; This file is a symbolic link to ~/.emacs.d/init.el under 
;;;; the name ~/.emacs . 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load Path
;;
;; add all modes in ~/.emacs.d/modes to load-path
(progn 
  (add-to-list 'exec-path "/opt/local/bin/"); add ports binary dir
  (add-to-list 'exec-path "/usr/local/bin/"); add aspell binary
  (let* ((my-mode-dirs '("color-theme/"
						 "yasnippet"
						 "slime" 
						 "org-mode/lisp"
						 "haskell-2.7.0/"
						 "anything/"
						 "magit/"
						 "eproject/"
						 "rhtml/"
						 "rvm/"
						 "rspec/"
						 "cucumber/"))
		 (modes-path (expand-file-name "~/.emacs.d/modes/")))
	(add-to-list 'load-path modes-path)
	(add-to-list 'load-path "~/.emacs.d/")
	(mapc '(lambda (dir) 
			   (add-to-list 'load-path (concat modes-path dir)))
			my-mode-dirs)))


(setq ispell-program-name "aspell")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loading configuration from file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-library "my-encoding.el")
(load-library "functions.el")
(load-library "my-anything.el")
(load-library "my-eproject.el")
(load-library "display.el")
(load-library "keyboard.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sending mail from within emacs to google
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq starttls-use-gnutls t
	  send-mail-function             'smtpmail-send-it
      message-send-mail-function     'smtpmail-send-it
      smtpmail-starttls-credentials  '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials      (expand-file-name "~/.authinfo")
      smtpmail-default-smtp-server   "smtp.gmail.com"
      smtpmail-smtp-server           "smtp.gmail.com"
      smtpmail-smtp-service          587
      smtpmail-debug-info            nil)

(autoload 'compose-mail "smtpmail" "Toggle whitespace visualization." t)

(add-hook 'message-setup-hook '(lambda ()
								 (set-input-method "my-german-postfix")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Autopair
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'autopair-global-mode "autopair"  nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ack
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(autoload 'ack-find-same-file "full-ack" nil t)
(autoload 'ack-find-file "full-ack" nil t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIT using magit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'magit)
(require 'magit-svn)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Haskell mode
(autoload 'haskell-mode "~/.emacs.d/modes/haskell-mode-2.7.0/haskell-site-file.el" 
  "Loading Haskell mode only when needed." t)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;; (turn-on-haskell-indentation)		
(setq auto-mode-alist
      (cons '("\\.hs" . haskell-mode) auto-mode-alist))

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
			    (setq ruby-insert-encoding-magic-comment 't)))

(smart-tabs-advice ruby-indent-line ruby-indent-level)
(setq ruby-indent-tabs-mode t)

(require 'align)

;; Alignments for ruby code
(add-to-list 'align-rules-list
             '(ruby-comma-delimiter
               (regexp . ",\\(\\s-*\\)[^# \t\n]")
               (repeat . t)
               (modes  . '(ruby-mode))))
(add-to-list 'align-rules-list
             '(ruby-hash-literal
               (regexp . "\\(\\s-*\\)=>\\s-*[^# \t\n]")
               (repeat . t)
               (modes  . '(ruby-mode))))
(add-to-list 'align-rules-list
             '(ruby-assignment-literal
               (regexp . "\\(\\s-*\\)=\\s-*[^# \t\n]")
               (repeat . t)
               (modes  . '(ruby-mode))))
(add-to-list 'align-rules-list          ;TODO add to rcodetools.el
             '(ruby-xmpfilter-mark
               (regexp . "\\(\\s-*\\)# => [^#\t\n]")
               (repeat . nil)
               (modes  . '(ruby-mode))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YAML mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.yml" . yaml-mode))
(add-hook 'yaml-mode-hook '(lambda ()
                             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
(autoload  'yaml-mode "yaml-mode.el" "YAML Mode" t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RVM / Rspec / Cucumber / Feature mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

(set-default 'feature-default-i18n-file "~/.emacs.d/modes/cucumber/i18n.yml")
(add-hook 'feature-mode-hook '(lambda ()
							 (setq feature-default-i18n-file
								   "~/.emacs.d/modes/cucumber/i18n.yml")))
(autoload 'feature-mode "feature-mode.el" "Cucumber Feature Mode" t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SLIME
(setq inferior-lisp-program "/Users/zickzackv/Source/ccl/dx86cl64")
;(slime-setup '(slime-repl))
(autoload 'slime "slime" "Interactive Lisp Development Environment" t)
(require 'slime-autoloads)
(slime-setup '(slime-repl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paredit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       (lambda () (paredit-mode +1)))
(add-hook 'lisp-mode-hook             (lambda () (paredit-mode +1)))
(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode

;; ;; setting agenda directory and matching regexp
(setq org-directory "~/Dropbox/Todo/")
(setq org-agenda-files '("~/Dropbox/Todo/"))
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
							(setq indent-tabs-mode nil)
							(set-input-method "my-german-postfix")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; YaSnippet
(require 'yasnippet-bundle) ;; not yasnippet-bundle
(setq yas/root-directory "~/.emacs.d/snippets")
;; Load the snippets
(yas/load-directory yas/root-directory)
(yas/load-directory "~/.emacs.d/modes/yasnippet/snippets")

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(blink-cursor-mode t)
 '(column-number-mode t)
 '(global-hl-line-mode t)
 '(make-backup-files nil)
 '(recentf-max-saved-items 40)
 '(recentf-mode t)
 '(recentf-save-file "~/.emacs.d/recent-files")
 '(ruby-insert-encoding-magic-comment nil)
 '(safe-local-variable-values (quote ((Package . User) (Syntax . Common-Lisp) (Package . CCL) (encoding . utf-8))))
 '(show-paren-mode t)
 '(truncate-lines t))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t :inherit nil :stipple nil :background "#2e3434" :foreground "#eeeeec" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :)))
 '(anything-file-name ((t (:foreground "White"))))
 '(font-lock-string-face ((t (:foreground "#ad7fa8" :slant normal))))
 '(hl-line ((t (:background "black")))))
