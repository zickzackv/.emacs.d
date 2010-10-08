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