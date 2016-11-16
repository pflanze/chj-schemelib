(require easy
	 (list-util-1 map/iota)
	 (stream stream-map/iota)
	 test)


;; abstraction over multiple kinds of sequences


;; only evaluating immediate position:

(def (_list? v)
     (or (pair? v)
	 (null? v)))

(def (_stream? v)
     (and (promise? v)
	  (_list? (force v))))

(def. _list.map/iota (flip map/iota))
(def. _stream.map/iota (flip stream-map/iota))

(def. _list.list identity)
(def. _stream.list stream->list)

(TEST
 > (.map/iota (list 10 20 30) +)
 (10 21 32)
 > (def s (.map/iota (delay (list 10 20 30)) +))
 > (promise? s)
 #t
 > (stream->list s)
 (10 21 32)
 > (.list s)
 (10 21 32))

