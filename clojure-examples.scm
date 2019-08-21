
(require clojure)

(use-clojure)


(defn constr [dflt keys]
  (apply hash-map
         (reduce (fn [l key]
                     (conj l dflt key))
                 '()
                 keys)))

(TEST
 > (.show-clojure (constr 1 '(:a :b :c :d :d :a)))
 (hash-map ':a 1 ':b 1 ':c 1 ':d 1))


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



