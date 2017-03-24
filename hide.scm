;;; Copyright 2014 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 oo-vector-lib ;; for .u8vector in u8vector.hide, right one?
		       ;; also: string.u8vector, u8vector-map, u8vector.string
	 stream)

(def (8bits x)
     (bitwise-and x 255))

(def (hide-stream fm x y)
     (let rec ((x x))
       (delay
	 (let ((x* (* x y)))
	   (cons (fm x*)
		 (rec x*))))))

(def hide-extract (comp 8bits floor))


(def. (u8vector.hide v x y)
  (u8vector-map
   bitwise-xor
   v
   (.u8vector
    (F (stream-take (hide-stream hide-extract x y)
		    (.length v))))))

(def. (string.hide str x y)
  (u8vector.hide (string.u8vector str) x y))

(def (mangle str x y)
     (u8vector.string (string.hide str x y)))

(def (hide str x y)
     (.hex-string (.hide str x y)))

(def (unhide str x y)
     (u8vector.string (.hide (string.parse-hex str) x y)))


(def (hide-code #(string? str) #(natural? x) #(exact-number? y))
     `(unhide ,(hide str x y) ,x ,y))

(TEST
 > (.hide "abc@fooooooooooooo" 1000 9/7)
 #u8(100 23 46 236 223 202 192 68 239 88 145 219 21 173 8 161 102 99)
 > (mangle "abc@fooooooooooooo" 1000 9/7)
 "d\27.\354\337\312\300D\357X\221\333\25\255\b\241fc"
 > (mangle # 1000 9/7)
 "abc@fooooooooooooo"
 > (hide "abc@fooooooooooooo" 1000 9/7)
 "64172EECDFCAC044EF5891DB15AD08A16663"
 > (unhide # 1000 9/7)
 "abc@fooooooooooooo")


