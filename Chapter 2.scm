; 2.1 Method to make rational number with regard to sign
; cons: construct, car: first, cdr: last

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
    (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (make-rat n d)
	(let ((sign (if (< d 0) - +)))
		(cons (sign n) (sign d))))

; 2.2 Represent line segment and find midpoint

 (define (make-point x y) (cons x y)) 
 (define (x-point p) (car p)) 
 (define (y-point p) (cdr p)) 
 (define (print-point p) 
   (newline) 
   (display "(") 
   (display (x-point p)) 
   (display ",") 
   (display (y-point p)) 
   (display ")")) 

(define (make-segment start-point end-point) (cons start-point end-point)) 
(define (start-segment segment) (car segment)) 
(define (end-segment segment) (cdr segment)) 
(define (print-segment segment)
	(print-point (start-segment segment))
	(display " -> ")
	(print-point (end-segment segment)))

(define (midpoint segment)
	(define (average-segment getter) (/ (+ (getter (start-segment segment)) (getter (end-segment segment))) 2))
	(make-point (average-segment x-point) (average-segment y-point)))

; 2.3 Represent a rectangle
(define (make-rect bottom-left top-right) (cons bottom-left top-right))
(define (bottom-left rectangle) (car rectangle))
(define (top-right rectangle) (cdr rectangle))
(define (top-left rectangle) 
	(cons (x-point (top-right rectangle))
		  (y-point (bottom-left rectangle))))
(define (top-right rectangle)
	(cons (x-point (bottom-left rectangle))
		  (y-point (top-right rectangle))))
(define (width-rect rectangle)
	(- (x-point (bottom-left rectangle))
	   (x-point (top-right rectangle))))
(define (length-rect rectangle)
	(- (y-point (bottom-left rectangle))
	   (y-point (top-right rectangle))))

(define (area-rect rectangle)
	(* (width-rect rectangle) (length-rect rectangle)))
(define (perimeter-rect rectangle)
	(* 2 (+ 
			(width-rect rectangle)
			(length-rect rectangle))))

; Alternate implementation
(define (make-rect1 bottom-left width height)
  (cons bottom-left (cons width height)))
(define (width-rect1 rectangle)
  (car (cdr rectangle)))
(define (height-rect1 rectangle)
  (cdr (cdr rectangle)))
(define (perimeter-rect1 rectangle)
  (* 2 
     (+ (width-rect1 rectangle)
        (height-rect1 rectangle))))
(define (area-rect1 rectangle)
  (* 
    (width-rect1 rectangle)
    (height-rect1 rectangle)))

; 2.4 Alternative implementation of cons, car and cdr
(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))
(define (cdr z)
  (z (lambda (p q) q)))

; 2.5 
(define (cons x y)
  (* 
    (power 2 x) 
    (power 3 y)))

(define (div-till-none-left x n)
  (define (div-iter acc) 
    (if (= 0 (remainder acc n))
        (div-iter (/ acc n))
        acc))
  (div-iter x))

(define (car z)
  (div-till-none-left z 3))

(define (cdr z)
  (div-till-none-left z 2))

; 2.7 Intervals
(define (make-interval x y) (cons x y))
(define (upper-bound z) (max (car z) (cdr z)))
(define (lower-bound z) (min (car z) (cdr z)))

; 2.8
(define (sub-interval z1 z2)
  (make-interval 
    (- (lower-bound z1) (upper-bound z2))
    (- (upper-bound z1) (lower-bound z2))))

; 2.9 subtract and add using width
(define (get-width z) 
  (/ 2 
     (- 
       (upper-bound z)
       (lower-bound z))))

; 2.12
(define (make-center-percent center tolerance-percent)
  (let ((tolerance (/ (* tolerance-percent center) 100)))
    (make-interval (- center tolerance) (+ center tolerance))))

; /*-*-----------------------------*-*/;
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

; 2.17 last of the element
(define (last-pair list)
  (if (null? (cdr list))
      (car list)
      (last-pair (cdr list))))

; 2.18 reverse
(define nil '()) 

(define (reverse list-in)
  (define (iter-reverse acc rest-list)
    (if (null? rest-list)
        acc
        (iter-reverse (cons (car rest-list) acc) (cdr rest-list))))
  (iter-reverse nil list-in))
  
; 2.19 
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))
(define (except-first-denomination coin-values) (cdr coin-values))
(define (first-denomination coin-values) (car coin-values))
(define (no-more? coin-values) (null? coin-values))

; 2.20 Same parity
(define (same-parity first . rest)
  (define (parity x) (= (remainder x 2) (remainder first 2)))
  (define (iter-parity acc rest-list)
    (if (null? rest-list)
        acc
        (iter-parity
          (if (parity (car rest-list))
              (append acc (list(car rest-list)))
              acc
              )
          (cdr rest-list))))
  (cons
     first
     (iter-parity nil rest)))

; 2.21 
(define (square x) (* x x))
(define (square-list items)
  (if (null? items)
      nil
      (cons 
        (square (car items)) 
        (square-list (cdr items)))))
(define (square-list items)
  (map square items))


; 2.22 

; 2.23

; 2.24 
(1 (2 (3 4)))

; 2.25 
(cdr (car (cdr (cdr list))))
(car (car list))
(cdr)*6

; 2.26
(1 2 3 4 5 6)
((1 2 3) 4 5 6)
((1 2 3) (4 5 6))

; 2.27
