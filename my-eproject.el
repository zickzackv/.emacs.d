
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Eproject and its proeject definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'eproject)
(require 'eproject-extras)

(define-project-type rails (generic)
  (look-for "Rakefile")
  :relevant-files ("\\.rb$" "\\.erb$" "\\.yml$" "\\.css$")
  :irrelevant-files ("vendor/.*" "tmp/.*" "doc/.*" "log/.*" "script/.*" "public/extjs/.*"))

(define-project-type diplom (generic)
  (look-for "Diplom.org")
  :relevant-files ("\\.tex$" "\\.bib$" "\\.hs$"))
