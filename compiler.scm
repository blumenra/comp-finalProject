(load "ass1/sexpr-parser.scm")
(load "ass2/tag-parser.scm")
(load "ass3/semantic-analyzer.scm")
;(load "schemetest")

(define T_UNDEFINED 0)
(define T_VOID 1)
(define T_NIL 2)
(define T_INTEGER 3)
(define T_FRACTION 4)
(define T_BOOL 5)
(define T_CHAR 6)
(define T_STRING 7)
(define T_SYMBOL 8)
(define T_CLOSURE 9)
(define T_PAIR 10)
(define T_VECTOR 11)

(define pipeline
    (lambda (s)
        ((star <sexpr>)
        s 
        (lambda (m r)
            (map (lambda (e)
                (annotate-tc
                    (pe->lex-pe
                        (box-set
                            (remove-applic-lambda-nil (parse e))))))
            m))
        (lambda (f) 'fail))))
   
(define file->list
    (lambda (in-file)
        (let ((in-port (open-input-file in-file)))
            (letrec ((run (lambda ()
				(let ((ch (read-char in-port)))
					(if (eof-object? ch)
						(begin
							(close-input-port in-port)
							'())
							(cons ch (run)))))))
			(run)))))
        
(define basic-table `((1 ,(void) (,T_void)) 
						(2 () (,T_nil)) 
						(3 #f (,T_bool 0))
						(5 #t (,T_bool 1))))
				
(define address-count 5)

;; (define code-gen 
;;     (lambda ()
;;         ))

(define const-token?
    (lambda (exp)
        (and
            (list? exp)
            (not (null? exp))
            (equal? (car exp) 'const))))
        
(define help
    (lambda (exp)
        (cond 
        ((null? exp) '())
        ((not (pair? exp)) `(,exp))
        (else (append (help (car exp)) (help (cdr exp)) `(,exp))))))
        
;; input: list of sexprs
;; output: list of all constant values
;; example: ((define (fvar x) (const 1)) (applic (fvar +) ((fvar x) (const 1)))))
;;          ==> (1 1)
(define extract-consts
    (lambda (lst-sexp)
        (cond 
            ((or (null? lst-sexp) (not (pair? lst-sexp))) '())
            ((const-token? lst-sexp) (cdr lst-sexp)) 
            (else (append (extract-consts (car lst-sexp)) 
                        (extract-consts (cdr lst-sexp)))))))
        
(define remove-dups
    (lambda (consts-list)
        (letrec ((inner-func (lambda (rvs)
            (cond ((null? rvs) '()) 
				((member (car rvs) (cdr rvs)) (inner-func (cdr rvs)))
                (else (cons (car rvs) (inner-func (cdr rvs))))))))
        (reverse (inner-func (reverse consts-list))))))
				
(define is-member 
    (lambda (arg table)
        (ormap (lambda (x) (equal? arg (cadr x))) table)))

(define find-address
    (lambda (arg table)
        (let ((expr (filter (lambda (x) (equal? (cadr x) arg)) table)))
            (car (car expr)))))
			
(define make-integer-const
	(lambda (const rest table)
		(set! address-count (+ address-count 2))
		(add-to-consts-table rest (append table (list `(,address-count ,const (,T_INTEGER ,const)))))))

(define make-rational-const
	(lambda (const rest table)
		(set! address-count (+ address-count 3))
		(add-to-consts-table rest (append table (list `(,address-count ,const (,T_FRACTION ,(numerator const) ,(denominator const))))))))
		
(define make-pair-const
	(lambda (const rest table)
		(set! address-count (+ address-count 3))
		(add-to-consts-table 
		rest 
		(append table (list `(,address-count ,const (,T_PAIR ,(find-address (car const) table) ,(find-address (cdr const) table))))))))
		
(define make-vector-const
	(lambda (const rest table)
		(set! address-count (+ address-count (+ 2 (vector-length const))))
		(add-to-consts-table 
		rest 
		(append table (list `(,address-count ,const (,T_VECTOR ,(vector-length const) 
                                                            ,@(map (lambda (elm) (find-address elm table)) (vector->list const)))))))))
													
(define make-string-const
	(lambda (const rest table)
		(set! address-count (+ address-count (+ 2 (string-length const))))
		(add-to-consts-table 
		rest 
		(append table (list `(,address-count ,const (,T_STRING ,(string-length const) 
							,@(map (lambda (c) (char->integer c)) (string->list const)))))))))	

;(define make-closure-const
;	(lambda (const rest table)
;	))

(define make-char-const
	(lambda (const rest table)
		(set! address-count (+ address-count 2))
		(add-to-consts-table rest (append table (list `(,address-count ,const (,T_CHAR ,(char->integer const))))))))
		
(define make-symbol-const
	(lambda (const rest table)
		(set! address-count (+ address-count 2))
		(add-to-consts-table rest (append table (list `(,address-count ,const (,T_SYMBOL ,const)))))))
	
(define add-to-consts-table
   (lambda (consts-list table)
        (if (null? consts-list) table
            (let ((const (car consts-list))
                (rest (cdr consts-list)))
                    (cond 
                        ((or (equal? (void) const)
                            (null? const)
                            (equal? const #f)
                            (equal? const #t))
                            (add-to-consts-table rest))
                        ((is-member const table) (add-to-consts-table rest))
                        ((integer? const) (make-integer-const const rest table)) 
                        ((rational? const) (make-rational-const const rest table))
                        ((pair? const) (make-pair-const const rest table))
                        ((vector? const) (make-vector-const const rest table))
                        ((string? const) (make-string-const const rest table))									
                        ;;;;((closure)
                        ((char? const) (make-char-const const rest table)) 
                        ((symbol? const) (make-symbol-const const rest table))							
                        (else 'error))))))

(define compile-scheme-file 
    (lambda (src-file trg-file)
        (set! consts-table '())
        
        (let* ((lst-sexp (pipeline (file->list src-file))) 
                (consts (remove-dups (fold-left append '() (map help (extract-consts lst-sexp))))))
                ;(const-table (add-to-consts-table consts basic-table)))
                (display lst-sexp)
				(newline)
				(display consts)
				(newline)
				;(display const-table))
                ;(find-consts lst-sexp))
        )))
