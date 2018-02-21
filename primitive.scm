(define number? (lambda (x) (or (integer? x) (rational? x))))

(define list
	(lambda x x))

(define +
    (lambda x
        (let ((len (length x)))
            (cond 
                ((= len 0) 0)
                ((= len 1) (car x))
                (else (ass-plus-bin (car x) (+ (cdr x))))))))

	
(define map 
    (lambda (func lst)
       (if (null? lst)
           '()
           (cons (func (car lst)) (map func (cdr lst))))))
		   
