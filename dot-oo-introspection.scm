;;; Copyright 2017-2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require dot-oo
         cj-env
         define-macro-star
         cj-typed
         C
         slib-sort ;; for show-method-statistics
         (stream list-group)
         ;; not sure if used anymore:
         cj-functional-2
         cj-functional
         fixnum-more
         list-util-3
         (list-util-1 reverse-map) 
         (cj-symbol symbol<?)
         srfi-1
         table-1
         (oo-vector-lib sum)
         debuggable-promise)

(export (macros method-table-for
                show-methods
                show-method-location)
        dot-oo:all-generics-sorted
        show-method-statistics
        show-generics
        show-generics*
        #!optional
        dot-oo:method-location
        dot-oo:show-method-location
        show-all-generics
        show-generics-for)

"Introspection facilities for method dispatch. Also see `doc/OO.md`."

(include "cj-standarddeclares.scm")

(possibly-use-debuggable-promise)


(define-macro* (method-table-for generic-name-sym)
  (generic-name-string.method-table-name
   (symbol->string (source-code generic-name-sym))))

(define-macro* (show-methods generic-name-sym)
  `(dot-oo:show-method-table (method-table-for ,generic-name-sym)))



(define-typed (dot-oo:method-location [methodtable? tbl]
                                      [symbol? typ])
  -> (maybe location?)
  (dot-oo:method-table-maybe-ref-prefix-columnS
   tbl typ (dot-oo#col:location)))

(define-typed (dot-oo:show-method-location tbl typ) -> (either void? symbol?)
  (cond ((dot-oo:method-location tbl typ) => show-location-location)
        (else
         'no-such-method)))

(define-macro* (show-method-location . args)
  (def (cont typ meth)
       (if (string-starts-with? (symbol.string meth) ".")
           `(dot-oo:show-method-location (method-table-for ,meth)
                                         ',typ)
           (raise-source-error stx "method argument doesn't start with a dot"
                               meth)))
  (case (length args)
    ((1) (assert* symbol? (first args)
                  (lambda_
                   (letv ((typ meth) (dot-oo:split-typename.methodname
                                      (symbol.string _)))
                         (cont (string.symbol typ)
                               (string.symbol meth))))))
    ((2) (assert* symbol? (first args)
                  (lambda (meth)
                    (assert* symbol? (second args)
                             (lambda (typ)
                               (cont typ meth))))))
    (else
     (raise-source-error stx ($ "need either 1 argument, type.method, "
                                "or 2 arguments, .method type")))))



(define-typed (dot-oo:all-generics-sorted) -> (list-of symbol?)
  (sort (map car (table->list dot-oo:genericname->method-table))
        (on symbol->string string<?)))


(define dot-oo:show-method-statistics-method-table-entry?
  (inhomogenous-list-of (either exact-natural0? table?)
                        symbol?))

;; longest function name ever?
(define-typed (dot-oo:show-method-table-entry->show-method-statistics-method-table-entry
               [dot-oo:show-method-table-entry? l])
  -> dot-oo:show-method-statistics-method-table-entry?
  (let-list ((typename pred implementor stat) l)
            (list stat typename)))


(define dot-oo:statistics-entry?
  (inhomogenous-list-of exact-natural0?
                        symbol? ;; the generics name
                        (list-of dot-oo:show-method-statistics-method-table-entry?)))

(define-typed (show-method-statistics) -> (list-of dot-oo:statistics-entry?)
  (define (stat-count l) (list-ref l 3))
  (define (stat-count* s)
    ;; after mapping
    (let ((v (car s)))
      (if (table? v)
          (sum (map car (table-values v)))
          v)))
  (=>> (table->list dot-oo:genericname->method-table)
       (map (lambda (genericname.method-table)
              (let-pair
               ((genericname method-table) genericname.method-table)
               (let* ((tableshown
                       (=>> (dot-oo:show-method-table method-table)
                            (filter (compose not (both fixnum-natural0? zero?) stat-count))
                            (map dot-oo:show-method-table-entry->show-method-statistics-method-table-entry)
                            ((flip sort) (on stat-count* <))))
                      (tot (sum (map stat-count* tableshown))))
                 (list tot
                       genericname
                       tableshown)))))
       (filter (compose not zero? car))
       ((flip sort) (on car <))))



(define dot-oo:show-all-generics-entry?
  ;; genericname and the types of the methods it defines
  (pair-of symbol?
           (list-of symbol?)))

(define-typed (show-all-generics) -> (list-of dot-oo:show-all-generics-entry?)
  (=>> (dot-oo:all-generics-sorted)
       (map (lambda (genname)
              (cons genname
                    (=>> (table-ref
                          dot-oo:genericname->method-table
                          genname)
                         ;; XX optimize the following ?
                         dot-oo:show-method-table
                         (map (lambda (entry)
                                (let-list ((typename
                                            _pred
                                            _implementor
                                            _stat)
                                           entry)
                                          typename)))))))))


(define-typed (show-generics-for obj)
  -> (list-of dot-oo:can.-show-generic-entry?)
  (=>> (dot-oo:all-generics-sorted)
       (filter-map (C dot-oo:can.-show-generic _ obj))))



(define-typed (dot-oo:_show-generics-for-multi objs)
  -> (list-of dot-oo:can.-show-generic-entry?)
  (let ((numobjs (length objs)))
    (=>> (map show-generics-for objs)
         (apply list-union (on car symbol<?))
         (list-group equal?)
         (filter-map (lambda (group)
                       (if (= (length group) numobjs)
                           (first group)
                           #f))))))


;; same for multiple objects (showing the intersection of those which
;; work for all objs), except when no argument given, then call
;; show-all-generics. Note that the result types are different in the
;; two cases.
(define-typed (show-generics . objs)
  -> (either (list-of dot-oo:show-all-generics-entry?)
             (list-of dot-oo:can.-show-generic-entry?))
  (if (null? objs)
      (show-all-generics)
      (dot-oo:_show-generics-for-multi objs)))



;; same as |show-generics| but grouped by type if any arguments are
;; given (if none given, still calls show-all-generics). NOTE: even
;; though in both cases the same result predicate matches, the roles
;; of the symbols in the first vs. second sublist positions is
;; reversed.
(define-typed (show-generics* . objs)
  -> (list-of dot-oo:show-all-generics-entry?)
  (if (null? objs)
      (show-all-generics)
      (=>> (dot-oo:_show-generics-for-multi objs)
           dot-oo:_show-generics*-group-by-type)))

(define dot-oo:_show-generics*-group-by-type
  (=>>* ((flip sort) (on cadr symbol<?))
        (list-group (on cadr eq?))
        (map (lambda (group)
               (cons (cadar group)
                     ;; since outer sort and list-group were stable,
                     ;; don't need to re-sort here, just:
                     (reverse-map car group))))))

(TEST
 > (equal? (show-all-generics) (show-generics))
 #t
 > (define l '(a b))
 > (equal? (show-generics* l) (show-generics l))
 #f
 > (equal? (show-generics l) (show-generics l l l))
 #t
 > (equal? (show-generics l) (show-generics l ""))
 #f)


