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
(global-set-key (kbd "s-T") 		 'anything-symbol)
(global-set-key (kbd "C-x k")	     'anything-kill-current-buffer)
(global-set-key (kbd "C-x b")	     'anything-for-buffers) 
(global-set-key (kbd "C-x C-b")	     'anything-for-buffers) ; backup
(global-set-key (kbd "s-b")		     'anything-buffers-list)
(global-set-key (kbd "C-x C-f")	     'anything-find-files)
(global-set-key (kbd "s-o")	         'anything-for-files)
(global-set-key (kbd "s-V")        'anything-show-kill-ring)


;; ack
(global-set-key (kbd "s-F") 'ack)

(define-key global-map [menu-bar tools remember] '("Remember Something" . remember-other-frame))

;; org
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)

(global-set-key (kbd "<C-f2>") 'bm-toggle)
(global-set-key (kbd "<f2>") 'bm-next)
(global-set-key (kbd "<S-f2>") 'bm-previous)

(global-set-key (kbd "<f12>") 'magit-status)
(global-set-key (kbd "<f11>") 'deft)


;; yasnippet
(global-set-key (kbd "s-/") 'yas/expand)


(cua-selection-mode +1)
