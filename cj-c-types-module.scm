;; for dev/testing purposes only--usually the cj-c-types.scm file is meant to be include'd.
(namespace "")
(requires
 ;; the user including cj-c-types.scm must add these manually to his own module requirements!
 cj-env
 cj-gambit-sys
 )
(compile #t)
