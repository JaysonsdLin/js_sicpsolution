(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
		((< x (car set)) false)
		((else (element-of-set? x (cdr set))))))
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
	  (cons x set)))
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2)) 
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
		(cond ((= x1 x2)
		       (cons x1
			         (intersection-set (cdr set1) (cdr set2))))
			  ((< x1 x2)
			   (intersection-set (cdr set1) set2))
			  ((> x1 x2)
			   (intersection-set set1 (cdr set2)))))))
(define (union-set set1 set2)
  (define (iter s1 result)
    (if (null? s1)
	    result
		(iter (cdr s1) (adjoin-set (car s1) result))))
  (iter set1 set2))
  
  
  