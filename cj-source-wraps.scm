;; again. lost on tie

(define (source-wrap-1 fn)
  (lambda (x)
    (fn (source-code x))))

(define (source-wrap-n fn)
  (lambda xs
    (apply fn (map source-code xs))))

(define (source-wrap-1+ fn)
  (lambda (x . rest)
    (apply fn (source-code x) rest)))

(define (source-wrap-_1-n fn)
  (lambda (a . rest)
    (apply fn a (map source-code rest))))

(define source.symbol-append (source-wrap-n symbol-append))
;; XX keep source information? rarely used for symbols though.

(define source.length (source-wrap-1 length))
(define source.car (source-wrap-1 car))
(define source.cdr (source-wrap-1 cdr))
(define source.pair? (source-wrap-1 pair?))
(define source.symbol? (source-wrap-1 symbol?))
(define source.string? (source-wrap-1 string?))

;; hm move to another lib?
(define symbol->keyword (compose string->keyword symbol->string))
(define source.symbol->keyword (source-wrap-1 symbol->keyword))

(define source.symbol->string (source-wrap-1 symbol->keyword))

;; from cj-source-util-2.scm
(define source.map source-map)
;; from predicates.scm
(define source.improper*-map (source-wrap-_1-n improper*-map))

