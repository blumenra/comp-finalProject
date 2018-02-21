;(((lambda (x) 
;(lambda ( y ) (set! x 1) x)) 4) 5)

(define l '(1 2 3))
;(set-cdr! l '(3 4 5 6 7))
l

;(string-ref "abc" 0)

;(make-string 5 #\a)

;(define l "ziv")
;(string-set! l 1 '#\3)
;l

;((lambda n (car n)) 1 2 3)

;((lambda (x . n) n) 1)

;(define list
;	(lambda x x))
;(list 1 2 3)

;((lambda (f x) (f (car x) (cdr x))) + '(1 . 2))

;((lambda (x) x) 2)

;(define v '#(1 2 3))
;(vector-set! v 1 5)
;v

;(vector-ref '#(1 (1 2) 3) 1)

;(remainder -4 12)
;(numerator -4/8)
;(numerator 0)
;(numerator -5)

;(denominator -4/8)
;(denominator 0)
;(denominator -5)

;; (define plusss
;;     (lambda x
;;         (let ((len (length x)))
;;             (cond 
;;                 ((= len 0) 0)
;;                 ((= len 1) (car x))
;;                (else (+ (car x) (plusss (cdr x))))))))
   
   
;; (define plusss
;;     (lambda (x)
;;           (+ (car x) (car (cdr x)))))
;;      
;; (plusss '(1 2))
;; (define plusss
;;     (lambda (x)
;;         (if (zero? x)
;;             22
;;             (plusss (- x 1)))))
;(car (cdr '(1 2)))
