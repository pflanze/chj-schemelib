
;; from lib/_kernel#.scm 144b72bf174d1f4baa17aaa11ca5909726076d50

(define-library-type-of-exception heap-overflow-exception
  id: d69cd396-01e0-4dcb-87dc-31acea8e0e5f
  constructor: #f
  opaque:
)

(define-library-type-of-exception stack-overflow-exception
  id: f512c9f6-3b24-4c5c-8c8b-cabd75b2f951
  constructor: #f
  opaque:
)

(define-library-type-of-exception nonprocedure-operator-exception
  id: f39d07ce-436d-40ca-b81f-cdc65d16b7f2
  constructor: #f
  opaque:

  (operator  unprintable: read-only:)
  (arguments unprintable: read-only:)
  (code      unprintable: read-only:)
  (rte       unprintable: read-only:)
)

(define-library-type-of-exception wrong-number-of-arguments-exception
  id: 2138cd7f-8c42-4164-b56a-a8c7badf3323
  constructor: #f
  opaque:

  (procedure unprintable: read-only:)
  (arguments unprintable: read-only:)
)

(define-library-type-of-exception keyword-expected-exception
  id: 3fd6c57f-3c80-4436-a430-57ea4457c11e
  constructor: #f
  opaque:

  (procedure unprintable: read-only:)
  (arguments unprintable: read-only:)
)

(define-library-type-of-exception unknown-keyword-argument-exception
  id: 3f9f8aaa-ea21-4f2b-bc06-f65950e6c408
  constructor: #f
  opaque:

  (procedure unprintable: read-only:)
  (arguments unprintable: read-only:)
)

(define-library-type-of-exception cfun-conversion-exception
  id: 9f09b552-0fb7-42c5-b0d4-212155841d53
  constructor: #f
  opaque:

  (procedure unprintable: read-only:)
  (arguments unprintable: read-only:)
  (code      unprintable: read-only:)
  (message   unprintable: read-only:)
)

(define-library-type-of-exception sfun-conversion-exception
  id: 54dfbc02-718d-4a34-91ab-d1861da7500a
  constructor: #f
  opaque:

  (procedure unprintable: read-only:)
  (arguments unprintable: read-only:)
  (code      unprintable: read-only:)
  (message   unprintable: read-only:)
)

(define-library-type-of-exception multiple-c-return-exception
  id: 73c66686-a08f-4c7c-a0f1-5ad7771f242a
  constructor: #f
  opaque:
)

(define-library-type-of-exception number-of-arguments-limit-exception
  id: f9519b37-d6d4-4748-8eb1-a0c8dc18c5e7
  constructor: #f
  opaque:

  (procedure unprintable: read-only:)
  (arguments unprintable: read-only:)
)

(define-library-type-of-exception type-exception
  id: cf06eccd-bf2c-4b30-a6ce-394b345a0dee
  constructor: #f
  opaque:

  (procedure unprintable: read-only:)
  (arguments unprintable: read-only:)
  (arg-num   unprintable: read-only:)
  (type-id   unprintable: read-only:)
)

(define-library-type-of-exception os-exception
  id: c1fc166b-d951-4871-853c-2b6c8c12d28d
  constructor: #f
  opaque:

  (procedure unprintable: read-only:)
  (arguments unprintable: read-only:)
  (message   unprintable: read-only:)
  (code      unprintable: read-only:)
)

(define-library-type-of-exception no-such-file-or-directory-exception
  id: 299ccee1-77d2-4a6d-ab24-2ebf14297315
  constructor: #f
  opaque:

  (procedure unprintable: read-only:)
  (arguments unprintable: read-only:)
)

