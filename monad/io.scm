;;; Copyright 2010-2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
         monad/syntax)


;; IO monad


;; XX define an interface that we'll implement here


(defclass (IO)

  (defclass (IO>> [IO? a]
                  [IO? b])
    (defmethod (run s)
      (.run a)
      (.run b)))
  

  (defclass (IO>>= [IO? a]
                   [function? r])
    (defmethod (run s)
      (.run (-> IO? (r (.run a))))))


  (defclass (IOReturn val)
    (defmethod (run s)
      val))


  (defclass (IOProc proc args)
    (defmethod (run s)
      (apply proc args)))


  (defclass (IONoop)
    (defmethod (run s)
      (void)))

  
  (defmethod >> IO>>)
  (defmethod >>= IO>>=)
  (defmethod return IOReturn))


(def io:return IOReturn)


;; for monad/syntax
(def IO->> IO>>)
(def IO->>= IO>>=)
(def IO-return IOReturn)


(defmacro (ioproc proc . args)
  `(IOProc ,proc (list ,@args)))


;; a small library

(def (io:print str)
     (ioproc print str))

(def (io:println str)
     (ioproc println str))

(def (io:read-line)
     (ioproc read-line))

(def io:read-line*
     (io:read-line))

(def io:noop
     (IONoop))

;; or, same thing,
(def io:noop*
     (io:return (void)))

;; run an IO monad:

(def (runio [IO? m])
     (.run m))


(TEST
 > (def a (mdo-in IO (io:print "Hello ") (io:println "world!")))
 > (.show a)
 (IO>> (IOProc print (list "Hello ")) (IOProc println (list "world!")))
 > (.show (%with-output-to-string (.run (eval #))))
 (values "Hello world!\n" (void))
 > (.show (%with-output-to-string (.run a)))
 (values "Hello world!\n" (void))
 > (.show (%with-output-to-string (.run (mdo-in IO (io:print "hi") (return "there")))))
 (values "hi" "there")
 )

