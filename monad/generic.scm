;;; Copyright 2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         monad/monad-ops)

(export (macro use-monad-for)
        (macro in-monad-for)
        (macro mdo-for)
        (macro mlet-for)
        ;; re-export
        (class monad-ops))


;; Explanation see monad/syntax.scm


(defmacro (use-monad-for a)
  (with-gensyms
   (A OPS)
   `(begin
      (define >> #f)
      (define >>= #f)
      (define return #f)
      (let* ((,A ,a)
             (,OPS (.monad-ops ,A)))
        (let-monad-ops ((_>> _>>= _return) ,OPS)
                       (set! >> _>>)
                       (set! >>= _>>=)
                       (set! return _return)))
      (##define-syntax
       >>-function
       (lambda (stx)
         (##sourcify-deep
          (apply
           (lambda (_name)
             '>>)
           (##source-code stx))
          stx)))

      (##define-syntax
       >>=-function
       (lambda (stx)
         (##sourcify-deep
          (apply
           (lambda (_name)
             '>>=)
           (##source-code stx))
          stx)))

      (##define-syntax
       return-function
       (lambda (stx)
         (##sourcify-deep
          (apply
           (lambda (_name)
             'return)
           (##source-code stx))
          stx))))))

(defmacro (in-monad-for a . body)
  (with-gensyms
   (A OPS)
   `(let* ((,A ,a)
           (,OPS (.monad-ops ,A)))

      (let-monad-ops ((>> >>= return) ,OPS)

                     (##define-syntax
                      >>-function
                      (lambda (stx)
                        (##sourcify-deep
                         (apply
                          (lambda (_name)
                            '>>)
                          (##source-code stx))
                         stx)))

                     (##define-syntax
                      >>=-function
                      (lambda (stx)
                        (##sourcify-deep
                         (apply
                          (lambda (_name)
                            '>>=)
                          (##source-code stx))
                         stx)))

                     (##define-syntax
                      return-function
                      (lambda (stx)
                        (##sourcify-deep
                         (apply
                          (lambda (_name)
                            'return)
                          (##source-code stx))
                         stx)))

                     (let ()
                       ,@body)))))


(defmacro (mdo-for a . exprs)
  `(in-monad-for ,a
                 (mdo ,@exprs)))

(defmacro (mlet-for a bindS expr)
  `(in-monad-for ,a
                 (mlet ,bindS ,expr)))

