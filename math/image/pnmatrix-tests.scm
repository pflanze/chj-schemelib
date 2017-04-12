(TEST
 > (define m (ppm8:create "foo" 1000 100))
 > (.vector (.ref m 0 0))
 #(0
   0
   0)
 > (.set! m 10 10  0 0 255)
 0
 > (.set! m 10 11  0 0 255)
 0
 > (.set! m 10 12  0 0 255)
 0
 > (.set! m 11 10  255 255 255)
 0
 > (.vector (.ref m 10 10))
 #(0
   0
   255)
 > (.ref m 10 9 vector)
 #(0
   0
   0)
 > (.ref m 10 11 vector)
 #(0
   0
   255)
 > (.ref m 10 12 vector)
 #(0
   0
   255)
 > (.ref m 10 13 vector)
 #(0
   0
   0)
 > (.ref m 11 10 vector)
 #(255
   255
   255)
 > (.ref m 11 11 vector)
 #(0
   0
   0)
 > (.ref m 11 11 vector)
 #(0 0 0)
 > (.ref m 11 10 vector)
 #(255 255 255)
 )

