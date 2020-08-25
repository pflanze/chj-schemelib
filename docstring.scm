;;; Copyright 2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy-2
         oo-util-lazy
         scheme-parse
         docstring-base
         (cj-gambit-sys decompile)
         test)

(export (macro docstring-from)
        docstring)

(include "cj-standarddeclares.scm")

"Docstring handling, user-interfacing part. For the syntactical
part (using docstrings in code) see `docstring-base.scm`."

"Todo: module docstrings. First do module info."


;; XX lib: move to scheme-parse.scm, or cj-typed or corescheme ?

(##namespace ("docstring#"
              parse-lambda:body
              parse-body:docstring-from
              repeatedly
              parse-body:docstring))

;; These return #f on parse failure, (pair-of T? list?) on success
;; where T? is whatever value they found.

(def (parse-lambda:body [ilist? exprs]) -> (maybe (pair-of list? ilist?))
     "return lambda body and the remainder after the lambda"
     (if-let-pair ((expr exprs*) exprs)
                  (if (scheme-parse:lambda? expr)
                      (if-let-pair ((argS r*) (rest expr))
                                   ;; XX handle cj-typed `->`
                                   ;; esp. expanded one, ugh.
                                   (cons r* exprs*)
                                   #f)
                      #f)
                  #f))

(def (extract-quoted qv)
     (assert (and ((list-of-length 2) qv)
                  (eq? (first qv) 'quote)))
     (second qv))

(def (parse-body:docstring-from [ilist? exprs])
     -> (maybe (pair-of symbol? ilist?))
     (if-let-pair
      ((expr exprs*) exprs)
      (if (and (pair? expr)
               (eq? (car expr) 'docstring-from))
          ;; unexpanded
          (cons (cadr expr) exprs*)

          (mcase
           expr
           (`(begin docstring-from: `quoted-fnname #!void)
            ;; expanded but not flattened
            (cons (extract-quoted quoted-fnname)
                  exprs*))
           (else
            (if (eq? expr 'docstring-from:)
                ;; expanded and flattened
                (if-let-list ((quoted-fnnam _void . exprs**) exprs*)
                             (begin (assert (eq? _void #!void))
                                    (cons (extract-quoted quoted-fnnam)
                                          exprs**))
                             #f)
                #f))))
      #f))

(TEST
 ;; original (decompile of compiled code):
 > (docstring#parse-body:docstring-from
    '((docstring-from foo) "a" "b" "c"))
 (foo "a" "b" "c")
 ;; expanded (decompile of interpreted code):
 > (docstring#parse-body:docstring-from
    '((begin docstring-from: 'foo #!void) "a" "b" "c"))
 (foo "a" "b" "c")
 ;; expanded (decompile of interpreted code in other cases, oh Gambit):
 > (docstring#parse-body:docstring-from
    '(docstring-from: 'foo #!void "a" "b" "c"))
 (foo "a" "b" "c"))


(def ((repeatedly [procedure? parse]) [ilist? exprs]) -> (pair-of list? ilist?)
     (let lp ((vs '())
              (exprs exprs))
       (if-let-pair ((v exprs*) (parse exprs))
                    (lp (cons v vs) exprs*)
                    (cons (reverse vs) exprs))))

;; hmm evil, mixed with runtime business here:

(def. (procedure.decompile-body fn) -> list?
  (let (code (procedure.getcode fn))
    (if-let-pair ((body exprs*) (parse-lambda:body (list code)))
                 (begin
                   (assert (null? exprs*))
                   body)
                 (error "can't parse body from" code))))


(def (parse-body:docstring [ilist? exprs]) -> (maybe (pair-of string? ilist?))
     "Return the *first* docstring in exprs if any (but can be
indirectly via `docstring-from`)."
     (if-let-list
      ((expr0 expr1 . _exprs*) exprs)
      (if (string? expr0)
          exprs
          (if-let-pair
           ((fname exprs*) (parse-body:docstring-from exprs))
           ;; careful about the continuation: returned explicitly as
           ;; the remaining exprs to parse; but can't just prepend
           ;; the remainder of body to exprs*, have to parse right
           ;; away.
           (let (ds+rest (append (docstrings (eval (-> symbol? fname)))
                                 exprs*))
             (parse-body:docstring ds+rest))
           #f))
      #f))

'(TEST
 > (docstring#parse-body:docstring
    '(docstring-from: 'docstring #!void "hmm" 'hi))
 ("hmm" 'hi) ;; WRONG
 > (docstring#parse-body:docstring
    '((begin docstring-from: 'docstring #!void) 'hi))
 XXX
 )


(def (docstrings [procedure? fn]) -> (ilist-of string?)
     "Retrieve docstrings of fn (at runtime)."
     (let (body (procedure.decompile-body fn))
       (let-pair ((docstrings _) ((repeatedly parse-body:docstring) body))
                 docstrings)))

(def (docstring [procedure? fn]) -> (maybe string?)
     "Retrieve docstrings of fn (at runtime) joined into one, return
#f if none."
     (let (ss (docstrings fn))
       (and (pair? ss)
            (=> ss
                (.map string-trim-right)
                (strings-join "\n\n")))))


'(TEST
 > (define (t a b)
     "Hi there"
     (docstring-from docstring))
 > (docstring t)
 "Hi there" ;;XXX bug?
 > (define (t a b)
     "Hi there"
     (docstring-from docstring)
     'hi)
 > (docstring t)
 "Hi there

docstring:

Retrieve docstring of fnname (at runtime), return #f if none."
 > (docstring (lambda (x) "hi"))
 #f
 > (docstring (lambda (x) "hi" "there"))
 "hi"
 > (define (t2 a b)
     "Hi there"
     (docstring-from docstring)
     "hi again"
     "and more"
     "And the result")
 > (docstring t2)
 XXX
 )

