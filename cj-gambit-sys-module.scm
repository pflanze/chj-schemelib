;;(namespace "")
(requires
 cj-test
 cj-env)

(compile #t)

(exports
 addressword-peek
 body-address
 body-addressword
 ;;check-mem-allocated
 max-fixnum
 min-fixnum
 mem-address
 mem-addressword
 mem-allocated?
 ;;mk-addressword->address
 subtype
 word-size
 word-width
 head-tag

 still-object?
 vector-like?
 mem-bytes

 ;; utilities:
 vectorlike-bytecopy!
 vectorlike-byteequal?
 vectorlike-byteref
 vectorlike-byteset!
 vectorlike-bytefill!
 )

(exports-on-request
 check-mem-allocated
 check-vector-like
 @vectorlike-bytecopy!
 @vectorlike-byteequal?
 @vectorlike-byteref
 @vectorlike-byteset!
 @vectorlike-bytefill!
 )