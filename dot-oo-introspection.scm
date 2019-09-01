;;; Copyright 2013-2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require cj-env
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
         (cj-symbol symbol<?))

(export (macro method-table-for)
        (macro show-methods)
        dot-oo:all-generics-sorted
        show-method-statistics
        show-generics
        show-generics-for
        show-generics-for*)

"Introspection facilities for method dispatch. Also see `doc/OO.md`."


(define-macro* (method-table-for generic-name-sym)
  (generic-name-string.method-table-name
   (symbol->string (source-code generic-name-sym))))

(define-macro* (show-methods generic-name-sym)
  `(dot-oo:show-method-table (method-table-for ,generic-name-sym)))


(define-typed (dot-oo:all-generics-sorted) -> (list-of symbol?)
  (sort (map car (table->list dot-oo:genericname->method-table))
        (on symbol->string string<?)))


(define dot-oo:show-method-statistics-method-table-entry?
  (inhomogenous-list-of exact-natural0?
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
  (define stat-count* car) ;; after mapping
  (=>> (table->list dot-oo:genericname->method-table)
       (map (lambda (genericname.method-table)
              (let-pair
               ((genericname method-table) genericname.method-table)
               (let* ((tableshown
                       (=>> (dot-oo:show-method-table method-table)
                            (filter (compose not zero? stat-count))
                            (map dot-oo:show-method-table-entry->show-method-statistics-method-table-entry)
                            ((flip sort) (on car <))))
                      (tot (apply + (map stat-count* tableshown))))
                 (list tot
                       genericname
                       tableshown)))))
       (filter (compose not zero? car))
       ((flip sort) (on car <))))



(define dot-oo:show-generics-entry?
  ;; genericname and the types of the methods it defines
  (inhomogenous-list-of symbol?
                        (list-of symbol?)))

(define-typed (show-generics) -> (list-of dot-oo:show-generics-entry?)
  (=>> (dot-oo:all-generics-sorted)
       (map (lambda (genname)
              (list genname
                    (=>> (table-ref dot-oo:genericname->method-table genname)
                         ;; XX optimize the following ?
                         dot-oo:show-method-table
                         (map (lambda (entry)
                                (let-list ((typename _pred _implementor _stat)
                                           entry)
                                          typename)))))))))


(define-typed (show-generics-for obj) -> (list-of dot-oo:can.-show-generic-entry?)
  (=>> (dot-oo:all-generics-sorted)
       (filter-map (C dot-oo:can.-show-generic _ obj))))

;; same but grouped by type
(define-typed (show-generics-for* obj)
  -> (list-of (inhomogenous-list-of symbol?
                                    (list-of symbol?)))
  (=>> (show-generics-for obj)
       ((flip sort) (on cadr symbol<?))
       (list-group (on cadr eq?))
       (map (lambda (group)
              (list (cadar group)
                    ;; since outer sort and list-group were stable,
                    ;; don't need to re-sort here, just:
                    (reverse-map car group))))))

