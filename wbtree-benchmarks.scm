(declare (standard-bindings)
         (extended-bindings)
         (block)
	 ;;(not safe)
         )

;; tests and benchmarks:

;; NOTE: I did put the benchmark runs into a TEST form even though at
;; that time it was basically just a way to comment them out. The idea
;; is not to |run-tests|, but to run them manually in a way that makes
;; sense to whoever runs them. 

; (define top #f)
; (set! top 100000)
; GRRRR only works if you have that define-if-not-defined GRRR
(define top 10000000)

(define-macro* (next n step cont) ;;macro is unsafeunhyg,hack.
  `(let ((n* (+ ,n ,step)))
     (,cont (modulo n* top)
	   (+ ,step 1))))

(IF #t
    (begin ;; use wbwbtree
      (define (make-empty-wbtree)
	empty-wbtree))
    (begin ;; use hash tables
      (define (wbtree:add t n)
	(table-set! t n #t)
	t)
      (define (wbtree:member? t k)
	(table-ref t k #f))
      (define (make-empty-wbtree)
	(make-table
	 ;;test: =
	 ))))
      
      

(define (create n)
  (let lp ((t (make-empty-wbtree))
	   (i n)
	   (n 0)
	   (step 1))
    (if (zero? i)
	t
	(next n step
	      (lambda (n step)
		(lp (wbtree:add* t n wbtreeparameter-number)
		    (dec i)
		    n
		    step))))))

(define (fetch t n start)
  (let lp ((tot 0)
	   (i n)
	   (n start)
	   (step 1))
    (if (zero? i)
	tot
	(next n step
	      (lambda (n step)
		(lp (if (wbtree:member?* t n wbtreeparameter-number)
			(inc tot)
			tot)
		    (dec i)
		    n
		    step))))))


(define-macro* ($ . body)
  `(let (($param wbtreeparameter-number))
     ,@body))

(define-macro* ($define var body)
  `(define ,var
     (let (($param wbtreeparameter-number))
       ,body)))

(TEST
 > (set! top 100000)
 > ($(car (reverse (wbtree:members(create 2000)))))
 99925
 > ($(car (reverse (wbtree:members(create 1000)))))
 99925
 ;;heh ?
> ($(wbtree:max (create 401)))
80601
> ($(wbtree:max (create 501)))
99681
> ($(wbtree:max (create 601)))
99681
> ($(wbtree:max (create 701)))
99681
> ($(wbtree:max (create 801)))
99925
> ($(wbtree:size (create 801)))
798
> ($(wbtree:size (create 1000)))
994
> ($(wbtree:size (create 2000)))
1961
> ($define t (create 4000))
> ($(fetch t 4000 0))
4000
> ($(fetch t 4000 1))
145
> ($(fetch t 4000 2))
122
> ($(fetch t 8000 2))
231
> ($define t (time (create 20000)))
; (time (create 20000))
;     5002 ms real time
;     4992 ms cpu time (4980 user, 12 system)
;     44 collections accounting for 611 ms real time (592 user, 4 system)
;     877064688 bytes allocated
;     673 minor faults
;     no major faults
; (time (create 20000))
;     274 ms real time
;     272 ms cpu time (272 user, 0 system)
;     6 collections accounting for 26 ms real time (32 user, 0 system)
;     30716320 bytes allocated
;     1582 minor faults
;     no major faults
; (time (create 20000))
;     362 ms real time
;     356 ms cpu time (352 user, 4 system)
;     5 collections accounting for 19 ms real time (20 user, 0 system)
;     28064320 bytes allocated
;     731 minor faults
;     no major faults
; (time (create 20000))
;     238 ms real time
;     232 ms cpu time (228 user, 4 system)
;     6 collections accounting for 23 ms real time (20 user, 0 system)
;     28064320 bytes allocated
;     1383 minor faults
;     no major faults
;performacne, wieder auf original
; (time (create 20000))
;     171 ms real time
;     168 ms cpu time (168 user, 0 system)
;     6 collections accounting for 24 ms real time (28 user, 0 system)
;     30716320 bytes allocated
;     1010 minor faults
;     no major faults
> ($(wbtree:size t))
16103
> ($(time (fetch t 20000 0)))
; (time (fetch t 20000 0))
;     1098 ms real time
;     1084 ms cpu time (1080 user, 4 system)
;     7 collections accounting for 99 ms real time (96 user, 0 system)
;     143408032 bytes allocated
;     no minor faults
;     no major faults
; (time (fetch t 20000 0))
;     75 ms real time
;     72 ms cpu time (72 user, 0 system)
;     no collections
;     4160000 bytes allocated
;     1 minor fault
;     no major faults
; (time (fetch t 20000 0))
;     104 ms real time
;     104 ms cpu time (100 user, 4 system)
;     1 collection accounting for 4 ms real time (4 user, 0 system)
;     4160000 bytes allocated
;     112 minor faults
;     no major faults
; (time (fetch t 20000 0))
;     71 ms real time
;     68 ms cpu time (68 user, 0 system)
;     no collections
;     4160000 bytes allocated
;     no minor faults
;     no major faults
;dito
; (time (fetch t 20000 0))
;     66 ms real time
;     68 ms cpu time (68 user, 0 system)
;     1 collection accounting for 4 ms real time (4 user, 0 system)
;     4160000 bytes allocated
;     96 minor faults
;     no major faults
20000
> ($(time (fetch t 20000 1)))
; (time (fetch t 20000 1))
;     1202 ms real time
;     1200 ms cpu time (1200 user, 0 system)
;     8 collections accounting for 111 ms real time (112 user, 0 system)
;     156450096 bytes allocated
;     no minor faults
;     no major faults
; (time (fetch t 20000 1))
;     87 ms real time
;     80 ms cpu time (72 user, 8 system)
;     1 collection accounting for 4 ms real time (4 user, 0 system)
;     3064384 bytes allocated
;     93 minor faults
;     no major faults
; (time (fetch t 20000 1))
;     109 ms real time
;     108 ms cpu time (108 user, 0 system)
;     1 collection accounting for 4 ms real time (4 user, 0 system)
;     3064384 bytes allocated
;     no minor faults
;     no major faults
; (time (fetch t 20000 1))
;     76 ms real time
;     76 ms cpu time (72 user, 4 system)
;     1 collection accounting for 4 ms real time (4 user, 0 system)
;     3064384 bytes allocated
;     no minor faults
;     no major faults
; (time (fetch t 20000 1))
;     61 ms real time
;     60 ms cpu time (60 user, 0 system)
;     no collections
;     3064384 bytes allocated
;     no minor faults
;     no major faults
;mit macro version of next:
; (time (fetch t 20000 1))
;     62 ms real time
;     60 ms cpu time (60 user, 0 system)
;     no collections
;     1464384 bytes allocated
;     182 minor faults
;     no major faults
2881


> (set! top 10000000)
;; in orig mode still:
; > (define t (time (create 20000)))
; (time (create 20000))
;     194 ms real time
;     192 ms cpu time (184 user, 8 system)
;     5 collections accounting for 27 ms real time (16 user, 8 system)
;     32513696 bytes allocated
;     3664 minor faults
;     no major faults
; > (wbtree:size t)
; 19964
; > (define t (time (create 1000000)))
; (time (create 1000000))
;     13351 ms real time
;     13257 ms cpu time (13185 user, 72 system)
;     60 collections accounting for 2000 ms real time (1964 user, 4 system)
;     2055686944 bytes allocated
;     38030 minor faults
;     no major faults
; > (wbtree:size t)
; 886793
; > (time (fetch t 1000000 0))
; (time (fetch t 1000000 0))
;     4804 ms real time
;     4784 ms cpu time (4780 user, 4 system)
;     2 collections accounting for 151 ms real time (152 user, 0 system)
;     128000000 bytes allocated
;     2267 minor faults
;     no major faults
; 1000000
; > (time (fetch t 1000000 1))
; (time (fetch t 1000000 1))
;     4770 ms real time
;     4748 ms cpu time (4748 user, 0 system)
;     no collections
;     68865856 bytes allocated
;     no minor faults
;     no major faults
; 76029

;;my variant:
; > (define t (time (create 1000000)))
; (time (create 1000000))
;     13085 ms real time
;     13061 ms cpu time (13025 user, 36 system)
;     15 collections accounting for 1080 ms real time (1072 user, 8 system)
;     1412866432 bytes allocated
;     45442 minor faults
;     no major faults
; > (define t (time (create 20000)))
; (time (create 20000))
;     226 ms real time
;     224 ms cpu time (224 user, 0 system)
;     no collections
;     26784320 bytes allocated
;     no minor faults
;     no major faults
; > (define t (time (create 1000000)))
; (time (create 1000000))
;     12926 ms real time
;     12897 ms cpu time (12793 user, 104 system)
;     179 collections accounting for 1034 ms real time (1036 user, 8 system)
;     1412866432 bytes allocated
;     7 minor faults
;     no major faults
; ok nun etwas schneller guuut. und je weniger allocated.
; > (time (fetch t 1000000 0))
; (time (fetch t 1000000 0))
;     3910 ms real time
;     3904 ms cpu time (3904 user, 0 system)
;     18 collections accounting for 80 ms real time (80 user, 0 system)
;     128000000 bytes allocated
;     no minor faults
;     no major faults
; 1000000
; > (time (fetch t 1000000 1))
; (time (fetch t 1000000 1))
;     3959 ms real time
;     3932 ms cpu time (3928 user, 4 system)
;     13 collections accounting for 57 ms real time (60 user, 0 system)
;     89600000 bytes allocated
;     no minor faults
;     no major faults
; 400000
; jay nun bringts was jay froh.
; nun:
;   PID USER      PR  NI  VIRT  RES  SHR S %CPU %MEM    TIME+  COMMAND
;  4547 chrisjaz  20   0  232m 186m 6164 S    0  4.8   0:58.86 gsc
; > (wbtree:size t)
; 41696
; uh
; ehr. ä 1mio und nur e grr.
; oh
; > top
; 100000
; ach MANNNNN
; MANNNN.

; again:
; nun my variant zuerst:

;ps mach nie quit now. war mem oben irgendkrank retained oder  jus not freed?
; > (define t (time (create 20000)))
; (time (create 20000))
;     261 ms real time
;     260 ms cpu time (256 user, 4 system)
;     3 collections accounting for 18 ms real time (12 user, 4 system)
;     29255648 bytes allocated
;     4426 minor faults
;     no major faults
; > (define t (time (create 1000000)))
; (time (create 1000000))
;     18253 ms real time
;     18197 ms cpu time (18029 user, 168 system)
;     82 collections accounting for 1788 ms real time (1760 user, 8 system)
;     1914129472 bytes allocated
;     22444 minor faults
;     no major faults
; > (wbtree:size t)
; 886793
; > (time (fetch t 1000000 0))
; (time (fetch t 1000000 0))
;     5577 ms real time
;     5568 ms cpu time (5564 user, 4 system)
;     2 collections accounting for 89 ms real time (88 user, 0 system)
;     128000000 bytes allocated
;     623 minor faults
;     no major faults
; 1000000
; > (time (fetch t 1000000 1))
; (time (fetch t 1000000 1))
;     5548 ms real time
;     5524 ms cpu time (5524 user, 0 system)
;     1 collection accounting for 42 ms real time (40 user, 0 system)
;     68865856 bytes allocated
;     no minor faults
;     no major faults
; 76029

; nun mit orig variant:
; > (define t (time (create 20000)))
; (time (create 20000))
;     219 ms real time
;     216 ms cpu time (204 user, 12 system)
;     1 collection accounting for 56 ms real time (44 user, 8 system)
;     32513696 bytes allocated
;     18547 minor faults
;     no major faults
; > (define t (time (create 1000000)))
; (time (create 1000000))
;     12961 ms real time
;     12929 ms cpu time (12897 user, 32 system)
;     54 collections accounting for 1945 ms real time (1952 user, 4 system)
;     2055686944 bytes allocated
;     29089 minor faults
;     no major faults
; > (time (fetch t 1000000 0))
; (time (fetch t 1000000 0))
;     4870 ms real time
;     4848 ms cpu time (4848 user, 0 system)
;     2 collections accounting for 157 ms real time (160 user, 0 system)
;     128000000 bytes allocated
;     97 minor faults
;     no major faults
; 1000000
; > (time (fetch t 1000000 1))
; (time (fetch t 1000000 1))
;     4866 ms real time
;     4856 ms cpu time (4856 user, 0 system)
;     1 collection accounting for 67 ms real time (68 user, 0 system)
;     68865856 bytes allocated
;     4 minor faults
;     no major faults
; 76029

; gopf einfach gopf. was hell ist los .
; either gambit so schlecht eh dassnidsichtbar oder monnier hat n  dep.

;; mit ##fixnum? als test:
; > (define t (time (create 1000000)))
; (time (create 1000000))
;     18171 ms real time
;     18125 ms cpu time (18097 user, 28 system)
;     17 collections accounting for 1734 ms real time (1736 user, 4 system)
;     1914129472 bytes allocated
;     35227 minor faults
;     no major faults
; > (time (fetch t 1000000 1))
; (time (fetch t 1000000 1))
;     5321 ms real time
;     5320 ms cpu time (5320 user, 0 system)
;     no collections
;     68865856 bytes allocated
;     no minor faults
;     no major faults
; 76029
; gopf liegts auch nid. thus no cross mod reason.

;; with the let-wbtree accessors. safe compilation, optim  mode:
; > (define t (time (create 1000000)))
; (time (create 1000000))
;     10359 ms real time
;     10333 ms cpu time (10285 user, 48 system)
;     65 collections accounting for 1814 ms real time (1816 user, 4 system)
;     1914129472 bytes allocated
;     28942 minor faults
;     no major faults
; > (time (fetch t 1000000 1))
; (time (fetch t 1000000 1))
;     5077 ms real time
;     5072 ms cpu time (5072 user, 0 system)
;     1 collection accounting for 50 ms real time (48 user, 0 system)
;     68865856 bytes allocated
;     656 minor faults
;     no major faults
; 76029

;; same, with orig mode:
; > (define t (time (create 1000000)))
; (time (create 1000000))
;     10621 ms real time
;     10605 ms cpu time (10541 user, 64 system)
;     21 collections accounting for 1987 ms real time (1972 user, 8 system)
;     2055686944 bytes allocated
;     66599 minor faults
;     no major faults
; > (time (fetch t 1000000 1))
; (time (fetch t 1000000 1))
;     4842 ms real time
;     4836 ms cpu time (4828 user, 8 system)
;     1 collection accounting for 97 ms real time (92 user, 4 system)
;     68865856 bytes allocated
;     2342 minor faults
;     no major faults
; 76029

;; again optim mode:
; > (define t (time (create 1000000)))
; (time (create 1000000))
;     10668 ms real time
;     10657 ms cpu time (10541 user, 116 system)
;     16 collections accounting for 2018 ms real time (1968 user, 36 system)
;     2055686944 bytes allocated
;     83694 minor faults
;     no major faults
; > (time (fetch t 1000000 1))
; (time (fetch t 1000000 1))
;     4789 ms real time
;     4776 ms cpu time (4776 user, 0 system)
;     no collections
;     68865856 bytes allocated
;     no minor faults
;     no major faults
; 76029

;; ok at least no slowdown.

;; optim, now benchmark in separate file, and now with ##vector-ref in let-* macros:
; > (define t (time (create 1000000)))
; (time (create 1000000))
;     10603 ms real time
;     10593 ms cpu time (10529 user, 64 system)
;     87 collections accounting for 1908 ms real time (1920 user, 8 system)
;     1914129472 bytes allocated
;     26602 minor faults
;     no major faults
; > (time (fetch t 1000000 1))
; (time (fetch t 1000000 1))
;     5427 ms real time
;     5420 ms cpu time (5420 user, 0 system)
;     1 collection accounting for 48 ms real time (48 user, 0 system)
;     68865856 bytes allocated
;     548 minor faults
;     no major faults
; 76029

;; orig:
; > (define t (time (create 1000000)))
; (time (create 1000000))
;     15885 ms real time
;     15821 ms cpu time (15705 user, 116 system)
;     21 collections accounting for 2214 ms real time (2192 user, 16 system)
;     2055686944 bytes allocated
;     65934 minor faults
;     no major faults
; > (time (fetch t 1000000 1))
; (time (fetch t 1000000 1))
;     6644 ms real time
;     6608 ms cpu time (6596 user, 12 system)
;     1 collection accounting for 102 ms real time (96 user, 4 system)
;     68865856 bytes allocated
;     2324 minor faults
;     no major faults
; 76029
;; heh. now it really got that much slower through the sep module?

;; but then optim again is also as slow. no idea. unclean vm?
; > (define t (time (create 1000000)))
; (time (create 1000000))
;     14774 ms real time
;     14749 ms cpu time (14693 user, 56 system)
;     16 collections accounting for 1889 ms real time (1864 user, 8 system)
;     1914129472 bytes allocated
;     67682 minor faults
;     no major faults
; > (time (fetch t 1000000 1))
; (time (fetch t 1000000 1))
;     6785 ms real time
;     6768 ms cpu time (6708 user, 60 system)
;     1 collection accounting for 77 ms real time (68 user, 8 system)
;     68865856 bytes allocated
;     1472 minor faults
;     no major faults
; 76029

;; with vector-ref (revert of ##vector-ref):
; > (define t (time (create 1000000)))
; (time (create 1000000))
;     14905 ms real time
;     14865 ms cpu time (14801 user, 64 system)
;     23 collections accounting for 1923 ms real time (1904 user, 16 system)
;     1914129472 bytes allocated
;     52488 minor faults
;     no major faults
; > (time (fetch t 1000000 1))
; (time (fetch t 1000000 1))
;     6366 ms real time
;     6344 ms cpu time (6344 user, 0 system)
;     no collections
;     68865856 bytes allocated
;     no minor faults
;     no major faults
; 76029
;; so "it must be the sep module".ehr. anyway ##vector-ref seems not to really help

;; compiled with (not safe):
; > (define t (time (create 1000000)))
; (time (create 1000000))
;     10008 ms real time
;     9997 ms cpu time (9949 user, 48 system)
;     23 collections accounting for 1889 ms real time (1864 user, 4 system)
;     1914129472 bytes allocated
;     52092 minor faults
;     no major faults
; > (time (fetch t 1000000 1))
; (time (fetch t 1000000 1))
;     4994 ms real time
;     4984 ms cpu time (4980 user, 4 system)
;     1 collection accounting for 66 ms real time (60 user, 4 system)
;     68865856 bytes allocated
;     1041 minor faults
;     no major faults
; 76029

;; compiled safe, but finally with declares in *this* file:
; > (define t (time (create 1000000)))
; (time (create 1000000))
;     14021 ms real time
;     14005 ms cpu time (13969 user, 36 system)
;     87 collections accounting for 1908 ms real time (1884 user, 8 system)
;     1914129472 bytes allocated
;     26495 minor faults
;     no major faults
; > (time (fetch t 1000000 1))
; (time (fetch t 1000000 1))
;     6903 ms real time
;     6892 ms cpu time (6888 user, 4 system)
;     1 collection accounting for 48 ms real time (48 user, 0 system)
;     68865856 bytes allocated
;     655 minor faults
;     no major faults
; 76029
;so that didn't matter either.


;; with hash table instead: (no test: argument)
; > (define t (time (create 1000000)))
; (time (create 1000000))
;     1622 ms real time
;     1620 ms cpu time (1564 user, 56 system)
;     2 collections accounting for 122 ms real time (112 user, 16 system)
;     119907184 bytes allocated
;     35134 minor faults
;     no major faults
; > (time (fetch t 1000000 1))
; (time (fetch t 1000000 1))
;     1243 ms real time
;     1232 ms cpu time (1212 user, 20 system)
;     1 collection accounting for 59 ms real time (60 user, 0 system)
;     68865856 bytes allocated
;     5022 minor faults
;     no major faults
; 76029

;; hm, did wbwbtree get faster thanks to more usage of let*-wbtree ? !:
; > (define t (time (create 1000000)))
; (time (create 1000000))
;     10602 ms real time
;     10589 ms cpu time (10281 user, 308 system)
;     22 collections accounting for 1877 ms real time (1756 user, 104 system)
;     1914129472 bytes allocated
;     56284 minor faults
;     no major faults
; > (time (fetch t 1000000 1))
; (time (fetch t 1000000 1))
;     4848 ms real time
;     4840 ms cpu time (4836 user, 4 system)
;     1 collection accounting for 69 ms real time (64 user, 4 system)
;     68865856 bytes allocated
;     1151 minor faults
;     no major faults
; 76029
;; and the same with wbwbtree compiled (not safe):
; > (define t (time (create 1000000)))
; (time (create 1000000))
;     9838 ms real time
;     9821 ms cpu time (9761 user, 60 system)
;     22 collections accounting for 1812 ms real time (1792 user, 24 system)
;     1914129472 bytes allocated
;     51812 minor faults
;     no major faults
; > (time (fetch t 1000000 1))
; (time (fetch t 1000000 1))
;     4979 ms real time
;     4972 ms cpu time (4960 user, 12 system)
;     2 collections accounting for 121 ms real time (116 user, 8 system)
;     68865856 bytes allocated
;     1059 minor faults
;     no major faults
; 76029
;(that part is no help.)

;; after parametrization (threading of $param, taking of lt and element? from there):
; > (define t (time (create 1000000)))
; (time (create 1000000))
;     18344 ms real time
;     18321 ms cpu time (18249 user, 72 system)
;     84 collections accounting for 1823 ms real time (1812 user, 8 system)
;     1914129472 bytes allocated
;     23890 minor faults
;     no major faults
; > (time (fetch t 1000000 1))
; (time (fetch t 1000000 1))
;     10881 ms real time
;     10873 ms cpu time (10873 user, 0 system)
;     1 collection accounting for 56 ms real time (56 user, 0 system)
;     68865856 bytes allocated
;     660 minor faults
;     no major faults
; 76029
;ugh. How comes?

;; with C variant (for fetch only):
; > (define t (time (create 1000000)))
; (time (create 1000000))
;     10768 ms real time
;     10685 ms cpu time (10593 user, 92 system)
;     70 collections accounting for 1740 ms real time (1708 user, 12 system)
;     1914129472 bytes allocated
;     27685 minor faults
;     no major faults
;**EHR** how comes. now again as fast as previously
; > (time (fetch t 1000000 1))
; (time (fetch t 1000000 1))
;     943 ms real time
;     932 ms cpu time (932 user, 0 system)
;     2 collections accounting for 100 ms real time (96 user, 0 system)
;     68865856 bytes allocated
;     1221 minor faults
;     no major faults
; 76029
;oh wow
; > (time (fetch t 1000000 1))
; (time (fetch t 1000000 1))
;     894 ms real time
;     880 ms cpu time (880 user, 0 system)
;     1 collection accounting for 44 ms real time (44 user, 0 system)
;     68865856 bytes allocated
;     323 minor faults
;     no major faults
; 76029
; > (time (fetch t 1000000 1))
; (time (fetch t 1000000 1))
;     881 ms real time
;     876 ms cpu time (872 user, 4 system)
;     1 collection accounting for 44 ms real time (44 user, 0 system)
;     68865856 bytes allocated
;     no minor faults
;     no major faults
; 76029
;heh. faster than hash.

;back to scheme version:
; > (time (fetch t 1000000 1))
; (time (fetch t 1000000 1))
;     5179 ms real time
;     5152 ms cpu time (5116 user, 36 system)
;     1 collection accounting for 73 ms real time (48 user, 24 system)
;     68865856 bytes allocated
;     26983 minor faults
;     no major faults
; 76029
;strange still faster than it was. like the create above.
;introduce named let into wbtree:member?:
; > (compile-file "wbwbtree" options: '(debug))
; "/mnt/rootextend/chrisjazz/GIT-Repo-CDS/Utilities/wbwbtree.o20"
; > (load "wbwbtree")
; "/mnt/rootextend/chrisjazz/GIT-Repo-CDS/Utilities/wbwbtree.o20"
; > (time (fetch t 1000000 1))
; (time (fetch t 1000000 1))
;     5456 ms real time
;     5432 ms cpu time (5420 user, 12 system)
;     1 collection accounting for 71 ms real time (64 user, 8 system)
;     68865856 bytes allocated
;     26956 minor faults
;     no major faults
; 76029
;so that didn't appear to help.
;now also change the lt and element? access to fetch-on-entry instead of on each call:
; > (time (fetch t 1000000 1))
; (time (fetch t 1000000 1))
;     4219 ms real time
;     4192 ms cpu time (4160 user, 32 system)
;     1 collection accounting for 74 ms real time (64 user, 8 system)
;     68865856 bytes allocated
;     27464 minor faults
;     no major faults
; 76029
;yay.
; > (define t (time (create 1000000)))
; (time (create 1000000))
;     11511 ms real time
;     11453 ms cpu time (11377 user, 76 system)
;     20 collections accounting for 1797 ms real time (1760 user, 24 system)
;     1914129472 bytes allocated
;     32141 minor faults
;     1 major fault
; > (time (fetch t 1000000 1))
; (time (fetch t 1000000 1))
;     4225 ms real time
;     4208 ms cpu time (4120 user, 88 system)
;     no collections
;     68865856 bytes allocated
;     506 minor faults
;     no major faults
; 76029
; > (define t (time (create 1000000)))
; (time (create 1000000))
;     11480 ms real time
;     11453 ms cpu time (11437 user, 16 system)
;     20 collections accounting for 1795 ms real time (1784 user, 4 system)
;     1914129472 bytes allocated
;     10772 minor faults
;     no major faults
;so the fetch-on-entry possibly penalizes create some.
)

