(requires
 cj-env ;;  symbol-append
 (cj-gambit-sys max-fixnum min-fixnum)
 cj-list-util ;; map-with-iota
 (srfi-1 cons*)
 (cj-string-flatten flat-append-strings)
 )

(compile #f);; since it's only a macro.

(exports-macros
 define-constant-from-c
 maybe-define-constant-from-c
 HACK_maybe-define-constant-from-c
 define-struct-field-accessors
 define-struct-accessors
 define-struct-from-c
 )

