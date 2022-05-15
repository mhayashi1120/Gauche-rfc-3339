;;
;; Package Gauche-rfc-3339
;;

(define-gauche-package "Gauche-rfc-3339"
  ;; 
  :version "1.3.4"

  ;; Description of the package.  The first line is used as a short
  ;; summary.
  :description "Parse/Construct rfc-3339 date.\n\
                Parse and construct date string like \"2014-01-02T03:04:05Z\" \"2014-01-02T03:04:05+09:01\" . "

  ;; List of dependencies.
  ;; Example:
  ;;     :require (("Gauche" (>= "0.9.5"))  ; requires Gauche 0.9.5 or later
  ;;               ("Gauche-gl" "0.6"))     ; and Gauche-gl 0.6
  :require (("Gauche" (>= "0.9.11-p1")))

  ;; List of providing modules
  ;; NB: This will be recognized >= Gauche 0.9.7.
  ;; Example:
  ;;      :providing-modules (util.algorithm1 util.algorithm1.option)
  :providing-modules (
                      rfc.3339
                      )
  
  ;; List name and contact info of authors.
  ;; e.g. ("Eva Lu Ator <eval@example.com>"
  ;;       "Alyssa P. Hacker <lisper@example.com>")
  :authors ("Masahiro Hayashi <mhayashi1120@gmail.com>")

  ;; List name and contact info of package maintainers, if they differ
  ;; from authors.
  ;; e.g. ("Cy D. Fect <c@example.com>")
  :maintainers ()

  ;; List licenses
  ;; e.g. ("BSD")
  :licenses ("BSD")

  ;; Homepage URL, if any.
  ; :homepage "http://example.com/Gauche-rfc-3339/"

  ;; Repository URL, e.g. github
  ; :repository "http://example.com/Gauche-rfc-3339.git"
  )
