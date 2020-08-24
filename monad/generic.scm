;;; Copyright 2019-2020 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy-2
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
      (define unwrap #f)
      (let* ((,A ,a)
             (,OPS (.monad-ops ,A)))
        (let-monad-ops ((_>> _>>= _return _unwrap) ,OPS)
                       (set! >> _>>)
                       (set! >>= _>>=)
                       (set! return _return)
                       (set! unwrap _unwrap)))
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

      (##define-syntax
       unwrap-function
       (lambda (stx)
         (##sourcify-deep
          (apply
           (lambda (_name)
             'unwrap)
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



;; Haskell's `sequence` takes a traversable. This one just for lists,
;; a flipped .sequence-in for others. Ok?

(def.* (monad-ops.rsequence m l)
  "(Traversable t, Monad m) => t (m a) -> m (t a)

Returns the resulting list (t a) in reversed order. See sequence-in
for a non-reversed variant.
"
  (let lp ((l l)
           (res '()))
    (if-let-pair ((a r) l)
                 (>>= a
                      (lambda (a*)
                        (lp r (cons a* res))))
                 (return res))))


(def.* (monad-ops.sequence m l)
  "(Traversable t, Monad m) => t (m a) -> m (t a)

See .rsequence-in for a variant that returns the resulting list
reversed.
"
  ;; COPY-PASTE with reverse added
  (let lp ((l l)
           (res '()))
    (if-let-pair ((a r) l)
                 (>>= a
                      (lambda (a*)
                        (lp r (cons a* res))))
                 (return (reverse res)))))

