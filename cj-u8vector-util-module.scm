(requires
 gambit-interpreter-env
 cj-math ;; quotient/ceiling
 cj-env ;; let-named*
 (cj-test TEST)
 )

(exports
 ;;digit->hexchar
 u8vector->hex-string
 u8vector->hex-string-lc
 u8vector->alphanumeric-string
 u8vector->number
 u8vector->number-string
 u8vector->string
 string->u8vector
 string->u8vector0

 write-u8vector
 read-u8vector
 read-u8vector-from-file
)
