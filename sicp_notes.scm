;;Ex1.1
10
(+ 5 3 4)
(- 9 1)
(/ 6 2)
(+ (* 2 4) (- 4 6))
(define a 3)
(define b (+ a 1))
(+ a b (* a b))
(= a b)
(if (and (> b a) (< b (* a b)))
    b
    a)
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
(+ 2 (if (> b a) b a))
(* (cond ((> a b) a)
	 ((< a b) b)
	 (else -1))
   (+ a 1))

;;Ex1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 3.0)))))
   (* 3 (- 6 2) (- 2 7)))

;;Ex1.3
(define (sum-squares-2of3 a b c)
  (cond ((and (< a b) (< a c)) (+ (* b b) (* c c)))
	((and (< b a) (< b c)) (+ (* a a) (* c c)))
	((and (< c b) (< c a)) (+ (* b b) (* a a)))))

(sum-squares-2of3 1 2 3)

;;Ex1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(a-plus-abs-b 3 -4)

;;Ex1.5
(define (p) (p))
(define  (test x y)
  (if (= x 0)
      0
      y))

(test 0 (p))

;;Ex1.6
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))


(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
	(else else-clause)))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
	  guess
	  (sqrt-iter (improve guess x)
		     x)))
(sqrt-iter 1.0 3)  ;Aborting!: maximum recursion depth exceeded


(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
	guess
	(sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(define (average x y)
  (/ (+ x y) 2))


(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

(define (cube-root x)
  (define (good-enough? guess)
    (< (abs (- (cube guess) x)) 0.001))
  (define (improve guess)
    (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))
  (define (cube-root-iter guess)
    (if (good-enough? guess)
	guess
	(cube-root-iter (improve guess))))
  (cube-root-iter 1.0))

(cube 3)
(cube (cube-root 222))
(- 213213213321 (cube (cube-root 213213213321)))

(restart 1)

;;Ex1.9
(define (dec x)
  (- x 1))
(define (inc x)
  (+ x 1))
(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))

(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))

(+ 2 1)


;;Ex1.10
(define (A x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1) (A x (- y 1))))))
(A 1 10)
(A 2 4)
(A 3 3)
(A 3 2)
(define (f n) (A 0 n))  ;; *2
(define (g n) (A 1 n))  ;; 2^n
(define (h n) (A 2 n))
(define (k n)
  (/ (* 5 (f n) (f n)) 4))

(k 5)

(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

(fib 100000)

;;counting change
(define (count-change amount)
  (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
	((or (< amount 0) (= kinds-of-coins 0)) 0)
	(else (+ (cc amount
		     (- kinds-of-coins 1))
		 (cc (- amount
			(first-denomination kinds-of-coins))
		     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
	((= kinds-of-coins 2) 5)
	((= kinds-of-coins 3) 10)
	((= kinds-of-coins 4) 25)
	((= kinds-of-coins 5) 50)))

(count-change 400)
(count-change 11)

;;Ex1.11
(define (fn-recur n)
  (cond ((< n 3) n)
	(else (+ (fn-recur (- n 1))
		 (* 2 (fn-recur (- n 2)))
		 (* 3 (fn-recur (- n 3)))))))

(define (fn-iter n)
  (cond ((< n 3) n)
	 (else (fn-iter-inter 0 1 2  n))))
(define (fn-iter-inter a b c  count)
  (cond ((= count 3) (+ (* 3 a) (* 2 b) c))
	(else (fn-iter-inter b c  (+ (* 3 a) (* 2 b) c) (- count 1)))))

(fn-recur 35)
(fn-iter 35)


;;Ex1.12 Pascal's triangle
(define (pascalTriangle n)
  (cond ((<= n 1) 1)
	((= n 2) (list 1 1))
	(else (map + (cons 0 (pascalTriangle (- n 1)))
		   (append (pascalTriangle (- n 1)) (list 0))))))
			
(map pascalTriangle (list 1 2 3 4 5 6 7 8 9 10))


;;Ex1.15
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

(sine 12.15)
(sine 1.57)
(define (countpsine x)
  (define (count n x)
    (if (> x 0.1)
	(count (+ 1 n) (/ x 3))
	n))
  (count 0 x))

(countpsine 12.15)

;;1.2.4 Exponentiation
(define (expt b n)
  (expt-iter b n 1))
(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
		 (- counter 1)
		 (* b product))))

(expt 33.3 500)

(define (fast-expt b n)
  (cond ((= n 0) 1)
	((even? n) (square (fast-expt b (/ n 2))))
	(else (* b (fast-expt b (- n 1))))))

(fast-expt 1.003333 50000)
(expt 1.003333 50000)

(define (countFuncall x base)
  (define (count n x)
    (if (> x base)
	(count (+ 1 n) (/ x base))
	n))
  (count 0 x))

(countFuncall 1000 2)

;;Ex1.16
(define (fast-square b n)
  (cond ((= n 0) 1)
	((even? n) (fast-square (* b b) (/ n 2)))
	(else (* b (fast-square (* b b) (/ (- n 1) 2))))))



(define (fast-square-iter b n)
  (define (fast-square-iter-inter b n a)
    (cond ((= n 0) a)
	  ((even? n) (fast-square-iter-inter (* b b) (/ n 2) a))
	  (else (fast-square-iter-inter (* b b) (/ (- n 1) 2) (* a b)))))
  (fast-square-iter-inter b n 1))

;;(timeit (fast-square 3.3 300))
;;(timeit (fast-square-iter 3.3 300))

(define (time-test f a b)
  (newline)
  (display a b)
  (start-time-test f a b (runtime)))
(define (start-time-test f a b start-time)
  (f a b)
  (report-time (- (runtime) start-time)))
(define (report-time elapsed-time)
  (display " *** *** ***")
  (display elapsed-time))

(time-test fast-suqare 3.3 300)
	   
;;Ex1.17
(define (ex* a b)
  (if (= b 0)
      0
      (+ a (ex* a (- b 1)))))

(ex* 3 15050)

(define (ex*-recur a b)
  (define (double a)
    (* 2 a))
  (define (halve a)
    (/ a 2))
  (if (even? b)
      (double (ex*-recur a (halve b)))
      (double (+ a (ex*-recur a (halve ( - b 1)))))))


;;Ex1.18
(define (ex*-iter a b)
  (define (double a)
    (* 2 a))
  (define (halve a)
    (/ a 2))
  (define (ex*iter-i a b c)
    (if (= b 0)
	c
	(ex*iter-i a (- b 1) (+ a c))))
  (if (even? b)
      (double (ex*iter-i a (halve b) 0))
      (double (+ a (ex*iter-i a (halve ( - b 1)) 0)))))

(ex*-iter 3 10)


;;Ex1.19
(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
	((even? count)
	 (fib-iter a
		   b
                   (+ (* p p) (* q q))     ; p'
		   (+ (* q q) (* 2 p q))   ;q'
		   (/ count 2)))
	(else (fib-iter (+ (* b q) (* a q) (* a p))
			(+ (* b p) (* a q))
			p
			q
			(- count 1)))))

(fib 100)

;;Ex1.20
(countFuncall 40 1.6)

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (- a (* b (floor (/ a b)))))))

(gcd 96 30)

;;1.2.6 Primality
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))

(prime? 1977)
(smallest-divisor 1977)

;;Ex1.21
(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)
(smallest-divisor 2857)

;;Ex1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
 (define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes n )
  (cond ((prime? n) n)
	(else (search-for-primes (+ 1 n)))))

(search-for-primes 1010)
(search-for-primes 100000)
(search-for-primes 100000000)
	      
(timed-prime-test 1999)
(runtime)

;;Ex1.23
(define (next-primer x)
  (cond ((= x 2) 3)
	((odd? x) (+ 2 x))
	(else (+ 1 x))))

    
;;Ex1.27 Carmichael numbers
(define (fermatTest n)
  (define (congruent a n)
    (if (>= a n)
	#t
	(and (= a (remainder (fast-expt a n) n))
	     (congruent (+ 1 a) n))))
  (congruent 2 n))

(fermatTest 561)
(fermatTest 1105)
(fermatTest 1729)
(fermatTest 2465)
(fermatTest 2821)
(fermatTest 6601)

;;1.3.1
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term  (next a) next b))))
(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))

(sum-cubes 1 10)

(define (identity x) x)
(define (sum-integers a b)
  (sum identity a inc b))

(sum-integers 1 100)

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(* 8 (pi-sum 1 100000000))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(integral cube 0 1 0.0001)

;;Ex1.29
(define (simpson-integral fun a b n)
  (define (step a)
    (+ a (/ (- b a) n)))
  (if (odd? n)
      (simpson-integral f a b (+ 1 n))
      (sum fun a step b)))

(simpson-integral cube 0 1 10)

;;Ex1.30
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ (term a) result))))
  (iter a 0))

;;Ex1.31
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
	 (product term (next a) next b))))

(define (factorial n)
  (product identity 1 inc n))

(factorial 100)

(define (product-iter term a next b)
  (define (pii term a next b p)
    (if (> a b)
      p
      (pii term (next a) next b (* (term a) p))))
  (pii term a next b 1))

(define (factorial-iter n)
  (product-iter identity 1 inc n))

(factorial-iter 100)

;;Ex1.32
(define (accumulate combiner null-value term a next b)
  (define (acc-iter combiner null-value term a next b)
    (if (> a b)
	null-value
	(acc-iter combiner (combiner (term a) null-value)
		  term (next a) next b)))
  (acc-iter combiner null-value term a next b))




;;Ex1.33
(define (filtered-accumulate combiner null-value term a next b filter)
  (define (acc-iter combiner null-value term a next b filter)
    (if (> a b)
	null-value
	(if (filter (term a))
	    (acc-iter combiner (combiner (term a) null-value)
		      term (next a) next b filter)
	    (acc-iter combiner null-value term (next a) next b filter))))
  (acc-iter combiner null-value term a  next b filter))

;;1.33a
(filtered-accumulate + 0 next-primer 2 next-primer 10 prime?)

;;1.33b
(filtered-accumulate * 1 (lambda (x) (+ 1 x)) 1 (lambda (x) (+ 1 x)) 100
		     (lambda (x)
		       (< 2 (gcd 100 x))))
;;Ex1.34
(define (f g)
  (g 2))
(f f)

(f square)
(f (lambda (z) (* z (+ z 1))))

;;1.3.3
(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
	midpoint
	(let ((test-value (f midpoint)))
	  (cond ((positive? test-value)
		 (search f neg-point midpoint))
		((negative? test-value)
		 (search f midpoint pos-point))
		(else midpoint))))))
(define (close-enough? x y)
  (< (abs (- x y)) 0.001))
(define (half-interval-method f a b)
  (let ((a-value (f a))
	(b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
	   (search f a b))
	  ((and (negative? b-value) (positive? a-value))
	   (search f b a))
	  (else
	   (error "Values are not of opposite sign" a b)))))

(half-interval-method sin 2.0 4.0)

(half-interval-method (lambda (x) (- (* x x) (* 2 x) 3))
		      1.0
		      4.0)

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

(fixed-point cos 1.0)
(fixed-point (lambda (x) (+ (sin x) (cos x)))
	     1.0)
(fixed-point (lambda (x) (- x (* x x)))
	     0.1)

(fixed-point (lambda (y) (/ 5 y))
	     1.0)

(fixed-point (lambda (y) (average y (/ 3  y)))
	     1.0)

;;Ex1.35
(fixed-point (lambda (x) (+ 1 (/ 1.0 x)))
	     1)

;;Ex1.36
(define (fixed-point-print f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (cond ((close-enough? guess next) (newline)
	     (display next)
	     next)
	    (else (newline)
		  (display next)
		  (try next)))))
  (try first-guess))

(fixed-point-print (lambda (x) (/ (log 1000.0) (log x)))
		   2.0)

;;Ex1.37
(define (cont-frac n d k)
  (define (iter i)
    (if (> i k)
	0
	(/ (n i) (+ (d i) (iter (+ i 1))))))
  (iter 1))

(cont-frac (lambda (i) 1.0)
	   (lambda (i) 1.0)
	   240)

(define (count-step x)
  (if (> (abs (- (cont-frac (lambda (i) 1.0)
			(lambda (i) 1.0)
			x)
		 (/ (- (sqrt 5) 1) 2)))
	 0.00001)
      (count-step (+ 1 x))
      x))

(count-step 5)

(define (cont-frac n d k)
  (define (iter i result)
    (if (> i k)
	result
	(iter (+ i 1) (/ (n k) (+ (d k) result)))))
  (iter 1 0))

;;Ex1.38
(define (eulerE x)
  (define (dd i)
    (if (= (remainder i 3) 2)
	(* 2 (/ (+ 1 i) 3))
	1))
  (+ 2.0 (cont-frac (lambda (i) 1.0)
	     dd
	     x)))

(eulerE 10)

;;Ex1.39
(define (tan-cf x k)
  (define (iter i result)
    (if (> i k)
	result
	(iter (+ 1 i) (/ (- (+ (* 2.0 i) 1)
			    result)))))
  (iter 1 x))

(tan-cf 2 20)
				   
;;1.3.4
(define (average-damp f)
  (lambda (x) (average x (f x))))

((average-damp square) 10)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

((deriv cube) 5)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt-newton x)
  (newtons-method (lambda (y) (- (square y) x))
		  1.0))
(sqrt-newton 3)

;;abstractions and first-class procedure
(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

;;Ex1.40
(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))
(newtons-method (cubic 1 2 3) 1)
(newtons-method (cubic 1 1 0) 1)

;;Ex1.41
(define (double f)
  (lambda (x) (f (f x))))
(double inc)
((double inc) 5)
(((double double) inc) 5)
(((double (double double)) inc) 5)
(((double (double (double double))) inc) 5)

;;Ex1.42
(define (compose f g)
  (lambda (x) (f (g x))))

((compose square inc) 6)

;;Ex1.43
(define (repeated f n)
  (if (<= n 1)
      (lambda (x) (f x))
      (compose f (repeated f (- n 1)) )))

((repeated square 2) 33)

;;Ex1.44
(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx))
		    (f x)
		    (f (+ x dx)))
		 3)))
(define (smooth-n-fold f n)
  (repeated (smooth f) n)) 

((smooth square) 1)
((smooth-n-fold square 6) 1)


;;Ex1.45  ???
(define (nth-root x n)
  (fixed-point ((repeated average-damp 
			  (lambda (y) (/ x (* y y y)))
		n))
	       1.0))

(nth-root 2 5)


;;Ex1.46
(define (iterative-improve good-enough? improve)
  (lambda (guess) (if (good-enough? guess)
		      (* 1.0 guess)
		      ((iterative-improve good-enough? improve)
		       (improve guess)))))

(define (sqrt-ii x)
  (define (good-enough? guess)
    (< (abs (- (* guess guess) x)) 0.00001))
  (define (improve guess)
    (average guess (/ x guess))) 
  ((iterative-improve good-enough? improve) x))

(sqrt-ii 3)

(define (fixed-point-ii f guess)
  (define (good-enough? guess)
    (< (abs (- guess (f guess))) tolerance))
  (define (improve guess)
    (f guess))
  ((iterative-improve good-enough? improve) guess))

(fixed-point-ii (lambda (x) (+ (cos x) (sin x)))
		1.0)







;;Chapter 2 Building abstractions with data
(define (linear-combination a b x y)
  (+ (* a x) (* b y)))

(define (add-rat x y)
  (make-rat (+ (* (numer  x) (denom y))
	       (* (numer  y) (denom x)))
	    (* (denom x)
	       (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer  x) (denom y))
	       (* (numer  y) (denom x)))
	    (* (denom x)
	       (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
	    (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom  x))))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))
(define one-half (make-rat 1 2))

(print-rat one-half)
(print-rat (make-rat 33 66))

;;Ex2.1
(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (and  (> n 0) (< d 0))
	(cons (/ (* -1 n) g) (/ (* -1 d) g)))
    (cons (/ n g) (/ d g))))

(print-rat (make-rat 99 -182))

(equal-rat? (make-rat 40 80) one-half)


;;Ex2.2
(define (make-point x y)
  (cons x y))
(define (make-segment p1 p2)
  (cons p1 p2))
(define (start-segment seg)
  (car seg))
(define (end-segment seg)
  (cdr seg))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))
(define (midpoint-segment seg)
  (make-point (average (x-point (start-segment seg))
		       (x-point (end-segment seg)))
	      (average (y-point (start-segment seg))
		       (y-point (end-segment seg)))))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(print-point (midpoint-segment
	      (make-segment (make-point 0 0) (make-point 2 2))))


;;Ex2.3
(define (line-length p1 p2)
  (sqrt (+ (square (- (x-point p1)
		      (x-point p2)))
	   (square (- (y-point p1)
		      (y-point p2))))))

(define (make-rect p1 p2)
  (cons p1 p2))

(define (perimeter rect)
  (let ((p1 (car rect)) (p2 (cdr rect)))
    (* 2 (+ (abs (- (x-point p1) (x-point p2)))
	    (abs (- (y-point p1) (y-point p2)))))))
(define (area rect)
    (let ((p1 (car rect)) (p2 (cdr rect)))
      (*  (abs (- (x-point p1) (x-point p2)))
	  (abs (- (y-point p1) (y-point p2))))))

(perimeter (make-rect  (make-point 0 0)
		       (make-point 2 8)))
(area (make-rect  (make-point 1 1)
		       (make-point 2 8)))
;;Ex2.4
(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))
(define (cdr z)
  (z (lambda (p q) q)))
(car (cons 1 2))
(cdr (cons 1 2))

;;Ex2.5
(define (cons x y)
  (* (expt 2  x) (expt 3 y)))
(define (car x)
  (if (= 1 (remainder x 2))
      0
      (+ 1 (car (/ x 2)))))
(define (cdr x)
  (if (= 1 (remainder x 3))
      0
      (+ 1 (car (/ x 3)))))

(car (cons 5 6))
(cdr (cons 5 6))


	  
;;Ex2.6  ???
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (add-1 zero))
(define two (add-1 one))

((zero 0) 2)

((add-1 zero) 0)

;;2.1.4 Interval arithmetic
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
		(make-interval (/ 1.0 (upper-bound y))
			       (/ 1.0 (lower-bound y)))))
(define (make-interval a b) (cons a b))

;;Ex2.7
(define (upper-bound a)
  (let ((x (car a)) (y (cdr a)))
    (if (> x y)
	x
	y)))
(define (lower-bound a)
  (let ((x (car a)) (y (cdr a)))
    (if (< x y)
	x
	y)))

(upper-bound (make-interval 1 3))
(lower-bound (make-interval 1 3))

;;Ex2.8
(define (sub-interval x y)
  (let ((p1 (- (lower-bound x) (lower-bound y)))
	(p2 (- (lower-bound x) (upper-bound y)))
	(p3 (- (upper-bound x) (lower-bound y)))
	(p4 (- (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

;;Ex2.9
(define (width x)
  (/ (- (upper-bound x) (lower-bound x)) 2.0))

  (width (make-interval 1 3))
  (width (make-interval 5 9))
(width (add-interval (make-interval 1 3)
		 (make-interval 5 9)))
(width (sub-interval (make-interval 1 3)
		     (make-interval 5 9)))
(width (mul-interval (make-interval 1 3)
		     (make-interval 5 9)))
(width (div-interval (make-interval 1 3)
		     (make-interval 5 9)))

;;Ex2.10
(define (div-interval x y)
  (define (cross-zero x)
    (< (* (upper-bound x) (lower-bound x)) 0))
  (if (or (cross-zero x) (cross-zero y))
      (error "interval cannot spans zero." x y)
      (mul-interval x
		    (make-interval (/ 1.0 (upper-bound y))
				   (/ 1.0 (lower-bound y))))))

;;Ex2.12
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (make-interval (- c (* 1.0 p c))
		 (+ c (* 1.0 p c))))
(define (percent cp)
  (/ (- (upper-bound cp) (center cp)) (center cp)))
(define (center cp)
  (+ (/ (- (upper-bound cp) (lower-bound cp)) 2.0)
     (lower-bound cp)))

(center (make-interval 1 3))
(percent  (make-interval 1 3))

;;Ex2.14 ???
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
		(add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
		  (add-interval (div-interval one r1)
				(div-interval one r2)))))


;;2.2.1 representing sequences
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))
(define squares (list 1 4 9 16 25))

(list-ref squares 3)

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(length squares)

;;Ex2.17
(define (last-pair l)
  (let ((remain (cdr l)))
    (if (null? (cdr remain))
	remain
	(last-pair remain))))

(last-pair squares)

;;Ex2.18
(define (reverse l)
  (define (r l1 l2)
    (if (null? l1)
	l2
	(r (cdr l1) (cons (car l1) l2))))
  (r l ()))

(reverse squares)

;;Ex2.19 ???
(define us-coins (list  25 10 5 1 50))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
	((or (< amount 0) (no-more? coin-values)) 0)
	(else
	 (+ (cc amount
		(except-first-denomination coin-values))
	    (cc (- amount
		   (first-denomination coin-values))
		coin-values)))))

(define (no-more? c)
  (null? c))

(define (first-denomination c)
  (car c))

(define (except-first-denomination c)
  (cdr c))

(first-denomination us-coins)
(except-first-denomination us-coins)
(cc 200 us-coins)

;;Ex2.20
(define (same-parity x . z)
  (define (app fn l1 l2)
    (cond ((null? l1) (reverse l2))
	  ((fn (car l1)) (app fn (cdr l1) (cons (car l1) l2)))
	  (else (app fn (cdr l1) l2))))
  (if (even? x) (cons x (app even? z ()))
      (cons x  (app odd? z ()))))

(same-parity 1 2 3 4 5 6 7)
(same-parity  2 3 4 5 6 7)

(define nil ())
;;(define nil (quote ()))
(equal? () (quote ()))


;;Ex2.21
(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

(square-list (list 1 2 3 4))

(define (square-list items)
  (map square items))

;;Ex2.22
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
	answer
	(iter (cdr things)
	      (append (list answer)  (list (square (car things)) )))))
  (iter items nil))




;;Ex2.23
(define (for-each fn items)
  (cond ((null? items)  #t)
	(else (fn (car items))
	      (for-each fn (cdr items)))))

(for-each (lambda (x) (newline) (display x))
	  (list 57 321 88))


;;2.2.2 Hierarchical structures
(cons (list 1 2) (list 3 4))

(define (count-leaves x)
  (cond ((null? x) 0)
	((not (pair? x)) 1)
	(else (+ (count-leaves (car x))
		 (count-leaves (cdr x))))))

;;Ex2.24
(list 1 (list 2 (list 3 4)))

;;Ex2.25
(car (cdr (car  (cdr (cdr '(1 3 (5 7) 9))))))
(car (car '((7))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr '(1 (2 (3 (4 (5 (6 7))))))))))))))))))

;;Ex2.26
(define x (list 1 2 3))
(define y (list 4 5 6))
(append x y)
(cons x y)
(list x y)

;;Ex2.27 
(define x (list (list 1 2) (list 3 4)))

(define (deep-reverse x)
  (cond ((null? x) nil)
	(else (append  (deep-reverse (cdr x))
		       (list
			(if (not (pair? (car x)))
			    (car x)
			    (deep-reverse  (car x))))))))

(pair? (car (cdr x)))
(pair? '(1 (2 3)))
(append () (list 1 2) (list 3 4))

(deep-reverse x)

;;Ex2.28 
(define (fringe x)
  (cond ((null? x) nil)
	((not (pair? x)) (list x))
	(else (append (fringe (car x))
		    (fringe (cdr x))))))
(fringe x)
(fringe (list x x))

;;Ex2.29  ???
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;;Ex2.29a
(define (left-branch bm)
  (car bm))
(define (right-branch bm)
  (car (cdr bm)))
(define (branch-length b)
  (car b))
(define (branch-structure b)
  (car (cdr b)))

(car (cdr (list 1 2)))

;;Ex2.29b
(define (total-weight b)
  (if (null? b)
      0
      (else (+ (if (pair? b)
		   (+ (total-weight (car b))
		      (total-weight (cdr b)))
		   (car (cdr b)))
	       (total-weight (car b))))))

;;Ex2.29c
;;Ex2.29d


;;mapping over trees
(define (scale-tree tree factor)
  (cond ((null? tree) nil)
	((not (pair? tree)) (* tree factor))
	(else (cons (scale-tree (car tree) factor)
		    (scale-tree (cdr tree) factor)))))

(scale-tree x 10)

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (scale-tree sub-tree factor)
	     (* sub-tree factor)))
       tree))

;;Ex2.30
(define (square-tree tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (square-tree sub-tree)
	     (square sub-tree)))
       tree))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

;;Ex2.31
(define (tree-map fn tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (tree-map fn sub-tree)
	     (fn sub-tree)))
       tree))

(define (square-tree tree)
  (tree-map square tree))

;;Ex2.32 ???
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
	(append rest (map (lambda (x)
			    (cons (car s) x))
			  rest)))))

(subsets (list 2 3 1))


;;2.2.3 sequences as conventional interfaces
(define (even-fibs n)
  (define (next k)
    (if (> k n)
	nil
	(let ((f (fib k)))
	  (if (even? f)
	      (cons f (next (+ k 1)))
	      (next (+ k 1))))))
  (next 0))

(even-fibs 100)

;;sequence operation
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
	((predicate (car sequence))
	 (cons (car sequence)
	       (filter predicate (cdr sequence))))
	(else (filter predicate (cdr sequence)))))

(filter odd? (list 1 2 3 4 5 6))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(accumulate op initial (cdr sequ(accumulate + 0 (list 1 2 3 4 5))
(accumulate * 1 (list 1 2 3 4 5))
(accumulate cons nil (list 1 2 3 4 5))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(enumerate-interval 2 7)

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
	((not (pair? tree)) (list tree))
	(else (append (enumerate-tree (car tree))
		      (enumerate-tree (cdr tree))))))

(enumerate-tree (list 1 (list 2 (list 3 4)) 5))

(define (sum-odd-squares tree)
  (accumulate +
	      0
	      (map square
		   (filter odd?
			   (enumerate-tree tree)))))
(define (even-fibs n)
  (accumulate cons
	      nil
	      (filter even?
		      (map fib
			   (enumerate-interval 0 n)))))

(define (list-fib-squares n)
  (accumulate cons
	      nil
	      (map square
		   (map fib
			(enumerate-interval 0 n)))))

(list-fib-squares 10)

(define (product-of-squares-of-odd-elements sequence)
  (accumulate *
	      1
	      (map square
		   (filter odd? sequence))))
(product-of-squares-of-odd-elements (list 1 2 3 4 5))


;;Ex2.33
(define (map p sequence)
  (accumulate (lambda (x y)
		(cons (p x) y))
	      nil
	      sequence))

(map square (list 1 2 3 4 5))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
	      
(append (list 1 2 3 ) (list  4 5 6))

(define (length sequence)
  (accumulate (lambda (x y)
		(+ y 1))
	      0
	      sequence))

(length (list 1 2 3 4 5))

;;Ex2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
		(+ (* higher-terms x) this-coeff))
	      0
	      coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))


;;Ex2.35  
(define (count-leaves tree)
  (accumulate (lambda (x y)
		(+ 1 y))
	      0
	      (map
	      (lambda (x) (not (pair? x)))
	      tree)))

(count-leaves (list 1 (list 2 3) 4))
(count-leaves (list 1 2 3 4 5))
(count-leaves (list 1 2))


;;Ex2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map (lambda (l) (car l)) seqs))
	    (accumulate-n op init (map (lambda (l) (cdr l)) seqs)))))

(accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
(list (list 1 2 3) (list 4 5 6) (list 7 8 9))
(map (lambda (l) (car l)) (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
		    


;;Ex2.37
(define (ppp m)
  (cond ((null? m)   nil)
	(else (newline)
	      (display (car m))
	      (ppp (cdr m)))))

(define v (list 1 1 1 1))
(define m (list (list 1 2 3 4)
		(list 4 5 6 6)
		(list 6 7 8 9)))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda  (w) (dot-product v w)) m))

(dot-product v v)
(dot-product v (car m))
(matrix-*-vector m v)

(define (transpose mat)
  (accumulate-n cons nil mat))

(transpose m)


(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v) (map (lambda (x) (dot-product x v)) cols)) m)))

(matrix-*-matrix m m)


;;Ex2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op result (car rest))
	      (cdr rest))))
  (iter initial sequence))

(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(fold-right list nil (list 1 2 3))
(fold-left list nil (list 1 2 3))
;; op commucativity

;;Ex2.39
(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))
(define (reverse sequence)
  (fold-left (lambda (x y) (append (list y) x)) nil sequence))

(reverse (list 1 2 3 4))

;;Nested mappings
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
	       (flatmap
		(lambda (i)
		  (map (lambda (j) (list i j))
		       (enumerate-interval 1 (- i 1))))
		(enumerate-interval 1 n)))))

(prime-sum-pairs 10)

(define (permutations s)
  (if (null? s)
      (list nil)
      (flatmap (lambda (x)
		 (map (lambda (p) (cons x p))
		      (permutations (remove x s))))
	       s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
	sequence))

(permutations (list 1 2 3 4))

;;Ex2.40
(define (unique-pairs n)
    (flatmap (lambda (i)
	 (map (lambda (j) (cons j (list i)))
	      (enumerate-interval 1 (- i 1))))
       (enumerate-interval 1 n)))

(unique-pairs 10)

(define (prime-sum-pairs n)
  (filter prime-sum? (cdr (unique-pairs 10))))
;;Ex2.41

(define (all-triples n s)
  (define (sum-to l)
    (= s (+ (car l) (cadr l) (caddr l))))
  (filter sum-to
	  (flatmap (lambda (i)
		 (flatmap (lambda (j)
			(map (lambda (k) (cons k (list j i)))
			     (enumerate-interval 1 (- j 1))))
		      (enumerate-interval 1 (- i 1))))
	       (enumerate-interval 1 n))))

(all-triples 100 150)



;;Ex2.42
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
	(list empty-board)
	(filter
	 (lambda (positions)
	   (safe? k positions))
	 (flatmap
	  (lambda (rest-of-queens)
	    (map (lambda (new-row)
		   (adjoin-position new-row k rest-of-queens))
		 (enumerate-interval 1 board-size)))
	  (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (adjoin-position new-row k rest-of-queens)
  )

(define empty-board ())

(define (safe? row positions)
  )

;;A picture language
(define wave2 (beside wave (flip-vert wave)))
(define wave4 (below wave2 wave2))

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))


(define wave4 (flipped-pairs wave))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
	(beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
	    (right (right-split painter (- n 1))))
	(let ((top-left (beside up up))
	      (bottom-right (below right right))
	      (corner (corner-split painter (- n 1))))
	  (beside (below painter top-left)
		  (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))


(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

;;Ex2.44




(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
	  (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
				  identity flip-vert)))
    (combine4 painter)))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
				  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))


;;Ex2.45





(define (frame-coord-map frame)
(lambda (v)
(add-vect
(origin-frame frame)
(add-vect (scale-vect (xcor-vect v)
(edge1-frame frame))
(scale-vect (ycor-vect v)
(edge2-frame frame))))))

((frame-coord-map a-frame) (make-vect 0 0))


;;Ex2.46
(define (make-vect x y)
  (cons x y))
(define (xcor-vect v)
  (car v))
(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1)
		(xcor-vect v2))
	     (+ (ycor-vect v1)
		(ycor-vect v2))))
(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1)
		(xcor-vect v2))
	     (- (ycor-vect v1)
		(ycor-vect v2))))

(define (scale-vect v scale)
  (make-vect (* (xcor-vect v) scale)
	     (* (ycor-vect v) scale)))

;;Ex2.47
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

;;selector
