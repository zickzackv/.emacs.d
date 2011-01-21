(when (require 'tex-site)
	(add-hook 'TeX-mode-hook (lambda ()
														 (TeX-fold-mode 1)
														 (TeX-PDF-mode 1)
														 (reftex-mode)
														 ))
 
	)
