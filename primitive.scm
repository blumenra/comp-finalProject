(define number? (lambda (x) (or (integer? x) (rational? x))))

(define map 
    (lambda (func lst)
       (if (null? lst)
           '()
           (cons (func (car lst)) (map func (cdr lst))))))
		   
(define list
	(lambda x x))