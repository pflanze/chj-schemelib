(include "cj-standarddeclares-1--include.scm")

;; Make overridden standard ops still be overridden using namespacing
;; hack:

(##namespace ("cj-struct#" vector?))

