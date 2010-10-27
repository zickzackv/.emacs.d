;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Encoding and Input

;; Inserting Euro Symbol by e~
(quail-define-package
 "my-german-postfix" "German" "DE<" t
 "German (Deutsch) input method
ae  -> ä
aee -> ae
oe  -> ö
oee -> oe
ue  -> ü (not after a/e/q)
uee -> ue
sz  -> ß
szz -> sz
e~  -> €
"
 nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("AE" ?Ä)
 ("ae" ?ä)
 ("OE" ?Ö)
 ("oe" ?ö)
 ("UE" ?Ü)
 ("ue" ?ü)
 ("sz" ?ß)
 ("e~" ?€)
 

 ("AEE" ["AE"])
 ("aee" ["ae"])
 ("OEE" ["OE"])
 ("oee" ["oe"])
 ("UEE" ["UE"])
 ("uee" ["ue"])
 ("szz" ["sz"])
 ("ge"  ["ge"])
 ("e~~" ["e~"])
 ("eue" ["eue"])
 ("Eue" ["Eue"])
 ("aue" ["aue"])
 ("Aue" ["Aue"])
 ("que" ["que"])
 ("Que" ["Que"]) 
)

(setq default-input-method              "my-german-postfix") ;toggle with C-\
(setq default-buffer-file-coding-system 'utf-8)
(prefer-coding-system                   'utf-8)
(set-default-coding-systems             'utf-8)

