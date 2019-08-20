
(require clojure)

(use-clojure)


(defn constr [dflt keys]
  (apply hash-map
         (reduce (fn [l key]
                     (conj l dflt key))
                 '()
                 keys)))

(TEST
 > (.show (constr 1 '(:a :b :c :d :d :a)))
 (table (list ':a 1) (list ':b 1) (list ':c 1) (list ':d 1)))


(defn fib-seq
  "Returns a lazy sequence of Fibonacci numbers"
  ([]
   (fib-seq 0 1))
  ([a b]
   (lazy-seq
    (cons b (fib-seq b (+ a b))))))

(TEST
 > (use-clojure)
 > (define TEST:equal? =)
 > (take 10 (fib-seq))
 (1 1 2 3 5 8 13 21 34 55))



