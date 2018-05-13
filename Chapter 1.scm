(define (sq-add a b) 
		(+ 
			(* a a)
			(* b b)
		)
	)

; Newton-Raphson square root method
(define (sqrt-iter guess x)
	(if (good-enough? guess x)
		guess
		(sqrt-iter (improve guess x) 
					x)))

(define (good-enough? guess x) 
	(< (abs (- (square guess) x)) 0.001))

(define (square x) (* x x))

(define (improve guess x)
	(average guess (/ x guess)))

(define (average a b)
	( /
		(+ a b)
		2
	)
)

; 1.7 Newton-Raphson sqrt method using convergence
(define (sqrt-iter-converge current-guess previous-guess x)
	(if (good-enough-conv? current-guess previous-guess)
		current-guess
		(sqrt-iter-converge (improve current-guess x)
					current-guess
					x)))

(define (good-enough-conv? current-guess previous-guess)
	(< (abs (- current-guess previous-guess)) (/ current-guess 100.0)))


; 1.8 Newton-Raphson cube root approximation

(define (cube-iter guess x)
	(if (good-enough? guess x)
		guess
		(cube-iter (cube-approx guess x) 
					x)))

(define (good-enough? guess x) 
	(< (abs (- (cube guess) x)) 0.001))

(define (cube x) (* x x x))

(define (cube-approx guess x)
	(/ 
		(+ 
			(/ x 
			(* guess guess)) 
		(* 2 guess))
	3))

; Factorial

(define (factorial n)
	(if (< n 2)
		1
		(* n (factorial (- n 1)))))

; Iterative Factorial
(define (factorial n)
	(fact-iter 1 1 n))

(define (fact-iter product counter max-count) 
	(if (> counter max-count)
		product
		(fact-iter (* counter product)
				   (+ counter 1)
				   max-count)))

; 1.11 “A function f is defined by the rule that f(n) = n if n<3 and f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n> 3. 
;		Write a procedure that computes f by means of a recursive process. Write a procedure that computes f by means of an iterative process.”

(define (func num)
	(cond ((< num 3) num)
				(else (+ (func (- num 1))
						 (* 2 (func (- num 2)))
						 (* 3 (func (- num 3))) 
						))))

; 1.12 “Write a procedure that computes elements of Pascal's triangle by means of a recursive process.”

(define (pascal row col)
	(cond   ((= row 1) 1)
			((= col 1) 1)
			((= col row) 1)
			(else (+ (pascal (- row 1) (- col 1))
					 (pascal (- row 1) col)))
		))


; 1.16  iterative faster exponentiation algorithm

(define (fast-exp base pow)
	(fast-exp-iter 1 base pow))

(define (even? n) (= (remainder n 2) 0))

(define (square n) (* n n))

(define (fast-exp-iter acc base pow)
	(cond ((< n 1) acc)
		  ((even? n) (fast-exp-iter acc (square base) (/ pow 2)))
		  (else (fast-exp-iter (* acc base) base (- pow 1)))
		))

; 1.29 Simpson's Rule
(define (sum term a next b)
	(if (> a b)
		0
		(+ (term a)
			(sum term (next a) next b))))

(define (cube a) (* a a a))

(define (inc a) (+ a 1))

(define (simpson-integral f a b n)
	(let ((h (/ (- b a) n)))
	(define (term k)
		(let ((y (f (+ a (* k h)))))
		(cond ((or (= k 0) (= k n)) y)
			  ((even? k) (* 2 y))
			  (else (* 4 y)))))
    (* (/ h 3)
       (sum term 0 inc n))))


; 1.30 Iterative sum
(define (sum term a next b)
	(define (iter a result)
	(if (> a b)
	result
	(iter (next a) (+ (term a) result))))
(iter a 0))

; 1.31 Product of a series
(define (product term a next b)
	(if (> a b)
		1
		(* (term a)
			(sum term (next a) next b))))

; Factorial
(define (factorial a)
	(define (identity a) a)
	(if (not (> 0))
		1
		(product identity 1 inc b)))

(define (product-iter term a next b)
	(define (iter a result)
	(if (> a b)
	result
	(iter (next a) (* (term a) result)))))


; 1.32 Accumulator
; recursive
(define (accumulate combiner null-value term a next b)
	(if (> a b)
	null-value
	(combiner (term a) (accumulate combiner null-value (next a) next b))))

; sum with accumulate
(define (sum term a next b)
	(accumulate + 0 term a next b))

;product with accumulate
(define (product term a next b)
	(accumulate * 1 term a next b))
; iterative
(define (iter-accumulate combiner null-value term a next b)
	(define (iter a result)
		(if (> a b)
		null-value
		(iter (next a) (combiner (term a) result))))

; 1.33 Filtered accumulate
(define (filter-accumulate filter combiner null-value term a next b)
	(if (> a b)
	null-value
	(if (filter a)
	(combiner (term a) (filter-accumulate filter combiner null-value term (next a) next b))
	(filter-accumulate filter combiner null-value null-value term (next a) next b))))

; Alternate
(define (filter-accumulate filter combiner null-value term a next b)
	(if (> a b)
	null-value
	(combiner 
		(if (filter a) (term a) null-value)
		(filter-accumulate filter combiner null-value term (next a) b))))

;a Sum of square of prime numbers from a to b
(define (sum-square-prime a b)
	(filter-accumulate prime? + 0 square a inc b))

(define (relative-prime-product n)
	(define (relatively-prime? i)
    	(define (gcd a b)
      		(if (= b 0)
          	a
          	(gcd b (remainder a b))))
    	(= (gcd i n) 1))
	(filtered-accumulate relatively-prime? * 1 identity 0 inc n))

; 1.34 
; (f f) => (f (f 2)) => (f (2 2))
; (2 2) is not callable



; 1.40 
(define (cubic a b c) 
	(lambda (x) (+ (* a x x) (* b x) c)))

; 1.41
(define (double f)
	(lambda (x) (f (f x))))


; 1.42 Compose function
(define (compose f g)
	(lambda (x) (f (g x))))

; 1.43 Apply same function n-times
(define (n-times f n)
	(define (iter a result)
		(if (> a n)
		result
		(iter a (f a))))
	(lambda (x) (iter 1 x)))


; 1.46 iterative-improve

(define (iterative-improve good-enough? improve)
	(lambda (guess)
	(if (good-enough? guess)
		guess
		((iterative-improve good-enough? improve) (improve guess)))))