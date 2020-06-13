;;; Copyright 2016-2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require table-1
         dot-oo
         (cj-functional compose)
         show
         (predicates alist?)
         ;; already loaded later anyway, so, OK?:
         Result
         test
         cj-cmp
         srfi-1)

(export table-sorted-keys
        table-sorted-values
        ;; re-export essentials from table-1
        table _table
        table*
        table-of
        itable-of
        ;; accessors:
        (method table.null?
                table.test
                table.hash
                ;; XX warning, .weak-keys and .weak-values not working
                table.init
                table.show
		table.ref
                table.Maybe-ref
                table.maybe-ref
                table.contains-key?
                table.set! 
                table.copy
                table.delete!
		table.push!
                table.pop!
                table.Maybe-pop!)
        
        ;; utilities:
        (method table.list
                table.keys
                table.sorted-keys
                table.values
                table.sorted-values)
        alist.table-maybe-function
        alist.table)


;; dependent on cj-cmp:

(define (table-sorted-keys t)
  (cmp-sort (table-keys t) generic-cmp))

(define (table-sorted-values t #!optional (cmp generic-cmp))
  (cmp-sort (table-values t) cmp))



;; dependent on dot-oo:

(define. table.null? (compose zero? table-length))

(define. table.list table->list)
(define. table.ref table-ref)
(define. table.set! table-set!)
(define. table.copy table-copy)

(define. table.test table-test)
(define. table.hash table-hash)
(define. table.weak-keys table-weak-keys)
(define. table.weak-values table-weak-values)
(define. table.init table-init)

(define. table.keys table-keys)
(define. table.sorted-keys table-sorted-keys)
(define. table.values table-values)
(define. table.sorted-values table-sorted-values)
(define. alist.table-maybe-function list->table-maybe-function)
(define. alist.table list->table)

(define. table.delete! table-delete!)
(define. table.push! table-push!)
(define. table.pop! table-pop!)



(define. (table.show t show)
  (define (get key op dflts)
    (let ((v (op t)))
      (if (any (lambda (dflt _) ;; _ is to prevent the absent value from
                           ;; interfering with function arg checks .
                 (eq? dflt v))
               dflts (make-list (length dflts) #t))
          '()
          `(,key ,(show v)))))
  `(table
    ,@(get test: table.test (list ##equal? equal?))
    ,@(get hash: table.hash
           (let ((t (table.test t)))
             (cond ((or (eq? t ##eq?)
                        (eq? t eq?))
                    (list ##eq?-hash eq?-hash))
                   ((or (eq? t ##equal?)
                        (eq? t equal?))
                    (list ##equal?-hash equal?-hash))
                   ((or (eq? t =)
                        (eq? t ##=))
                    ;; Hm, does that mean it's slow? No, still hashes?
                    (list ##generic-hash))
                   (else
                    (list ##equal?-hash equal?-hash)))))
    ,@(get weak-keys: table.weak-keys (list #f))
    ,@(get weak-values: table.weak-values (list #f))
    ,@(get init: table.init (list table:absent))
    ,@(map show (cmp-sort (table->list t) (on car generic-cmp)))))

(TEST
 > (show (list->table '((a . 1) (b . 2))))
 (table (cons 'a 1) (cons 'b 2))
 > (show (list->table '((a . 1) (b . 2)) test: eq?))
 (table test: eq? (cons 'a 1) (cons 'b 2))
 > (show (list->table '((a . 1) (b . 2)) test: equal?))
 (table (cons 'a 1) (cons 'b 2))
 > (show (list->table '((2 . a) (1 . b)) test: =))
 (table test: = (cons 1 'b) (cons 2 'a))

 ;;XX hm these don't work
 ;; > (show (list->table '(("a" . 1) ("b" . 2)) weak-keys: #t))
 ;; (table weak-keys: #t (cons "a" 1) (cons "b" 2))
 ;; > (show (list->table '(("a" . 1) ("b" . 2)) weak-values: #t))
 ;; (table weak-values: #t (cons "a" 1) (cons "b" 2))
 ;; > (show (list->table '(("a" . 1) ("b" . 2)) weak-keys: #t weak-values: #t))
 ;; (table weak-keys: #t weak-values: #t (cons "a" 1) (cons "b" 2))

 > (show (list->table '((a . 1) (b . 2)) init: 123))
 (table init: 123 (cons 'a 1) (cons 'b 2))
 )

;; table, table-of
(TEST
 > (.ref (table (cons 'fo 1) (cons 'bar 2)) 'bar)
 2
 > ((table-of symbol? integer?) (table (cons 'fo 1) (cons 'bar 2)))
 #t
 > ((table-of symbol? integer?) (table (cons 'fo 1) (cons 'bar "2")))
 #f
 > ((table-of symbol? integer?) (table (cons fo: 1) (cons 'bar 2)))
 #f
 > ((table-of symbol? integer?) (table))
 #t)


;; Like table-of but avoiding the need to generate a full intermediary
;; list. Only tests up to 2 key/value pairs!
(define (itable-of key? val?)
  (lambda (v)
    (and (table? v)
         (continuation-capture
          (lambda (ret)
            (let ((count 0))
              (table-for-each
               (lambda (key val)
                 (if (and (key? key)
                          (val? val))
                     (if (< count 2)
                         (inc! count)
                         (continuation-return ret #t))
                     (continuation-return ret #f)))
               v)
              #t))))))

(TEST
 > ((itable-of symbol? integer?) (table (cons 'fo 1) (cons 'bar 2)))
 #t
 > ((itable-of symbol? integer?) (table (cons 'fo 1) (cons 'bar "2")))
 #f
 > ((itable-of symbol? integer?) (table (cons fo: 1) (cons 'bar 2)))
 #f
 > ((itable-of symbol? integer?) (table))
 #t
 > ((itable-of symbol? integer?) (table (cons 'fo 1) (cons 'bar 2) (cons 'baz 3)))
 #t)



;; table*
(TEST
 > (show (table* init: 123))
 (table weak-keys: '? weak-values: '? init: 123)
 > (show (table* "b" 2 'a 1)) ;; don't use all string keys or TABLE? matches
 (table (cons 'a 1) (cons "b" 2))
 > (show (table* init: 123 "b" 2 'a 1))
 (table init: 123 (cons 'a 1) (cons "b" 2)))



(TEST
 > (define t (make-table))
 > (.push! t "a" 1)
 > (.push! t "b" 1)
 > (.push! t "a" 2)
 > (.ref t "a")
 (2 1)
 > (.pop! t "a")
 2
 > (.ref t "a")
 (1)
 > (%try (.pop! t "y"))
 (exception text: "table-pop!: key not found: \"y\"\n")
 > (.pop! t "a" 'n)
 1
 > (.pop! t "a" 'n)
 n
 > (.list t)
 (("b" 1)))



(def. (table.Maybe-ref t key)
  (let (v (table-ref t key table:nothing))
    (if (eq? v table:nothing)
        (Nothing)
        (Just v))))

(def. (table.maybe-ref t key)
  (table-ref t key #f))

(def. (table.contains-key? t key)
  (let (v (table-ref t key table:nothing))
    (not (eq? v table:nothing))))



;; adapted from table-pop! (table-1.scm)
(def. (table.Maybe-pop! t key
                        #!optional
                        (clean? #t))
  (let ((l (table-ref t key table:nothing)))
    (if (eq? l table:nothing)
        (Nothing)
        (if (null? l)
            (Nothing)
            (let-pair ((v r) l)
                      (if (and clean? (null? r))
                          (table-set! t key)
                          (table-set! t key r))
                      (Just v))))))

(TEST
 > (def t (table))
 > (.Maybe-pop! t 'foo)
 [(Nothing)]
 > (.push! t 'foo 3)
 > (.Maybe-ref t 'foo)
 [(Just) (3)]
 > (.Maybe-ref t 'bar)
 [(Nothing)]
 > (.push! t 'foo 4)
 > (.Maybe-pop! t 'foo)
 [(Just) 4]
 > (.Maybe-pop! t 'foo)
 [(Just) 3]
 > (.Maybe-pop! t 'foo)
 [(Nothing)])

