;; Simple modification: When no selection is active 
;; delete the whole line. Behavior is stolen from IntelliJ ;-)
;; FIXME: mark-active muss einmal gerufen worden sein!
;; -- Fabian Otto
;; Emacs with cocoa does not uses clipboard-kill-region but instead 
;; kill-region for s-x and C-w
(defun clipboard-kill-region (beg end)
  "Kill the region, and save it in the X clipboard."
  (interactive "r")
  (if (or (not transient-mark-mode) mark-active)
      (let ((x-select-enable-clipboard t))
	(kill-region beg end))
    (kill-whole-line)))

;; Delete current line if region is not activated
(defadvice kill-region (before slickcut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;; Copy current line if region is not activated
(defadvice kill-ring-save (before slickcopy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;; Creating ETags file
(defun create-tags (dir-name)
    "Create tags file."
    (interactive "DDirectory: ")
    (shell-command
     (format "%s -f %s/TAGS -e -R %s" path-to-ctags dir-name dir-name)))
