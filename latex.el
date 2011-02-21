
(require 'tex-site)

(setq-default TeX-view-program-list '(("Preview.app" "open -a Preview.app %o")))
(setq-default TeX-view-program-selection '(((output-dvi style-pstricks) "Preview.app") 
																					 (output-dvi "open") 
																					 (output-pdf "Preview.app") 
																					 (output-html "open")))

(add-hook 'TeX-mode-hook (lambda ()
													 (setq-default TeX-master nil) ; Query for master file.
													 (TeX-fold-mode 1)
													 (TeX-PDF-mode 1)
													 (reftex-mode)
													 (local-set-key "\M-n" 'forward-paragraph)
													 (local-set-key "\M-p" 'backward-paragraph)
													 ))

	
