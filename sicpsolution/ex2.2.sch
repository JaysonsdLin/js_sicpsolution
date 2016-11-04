(define (print-point p)
    (newline)
	(display "(")
	(display (x-point p))
	(display ",")
	(display (y-point p))
	(display ")"))
(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))
(define (make-segment p1 p2)
  (cons (- (car p1)
           (car p2))
		(- (cdr p1)
		   (cdr p2))))
(define (midpoint-segment se)
  (define (halve a) (/ a 2.0))
  (let ((a (car se))
        (b (cdr se)))
	(make-point (halve a)
	            (halve b))))
		   
    
  