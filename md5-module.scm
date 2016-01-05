(requires
 ;;gambit-interpreter-env
 cj-u8vector-util
 cj-test
 )

(exports
 md5:digest
 (digest md5-digest)
 )

(exports-on-request
 (make-md5-context* make-md5-context)
 (starts md5-init)
 (update md5-update)
 (finish md5-finish)
 )

;(namespace "")

(compile #t)

(cc-opts "-O3") ; over the default -O1, for a few percent more speed
