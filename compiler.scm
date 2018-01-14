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



(define consts-table '())
(define global-var-table '())


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
        
(define basic-table `((1 ,(void) (,T_VOID)) 
                    (2 () (,T_NIL)) 
                    (3 #t (,T_BOOL 0))
                    (5 #f (,T_BOOL 1))))
				
(define address-count 7)

(define make-token-pred
    (lambda (tag)
        (lambda (exp)
            (and
                (list? exp)
                (not (null? exp))
                (equal? (car exp) tag)))))
                
(define const-token? (make-token-pred 'const))

;; input: const
;; output: list of all the components of const
;; examples: 5  ==>  '(5)
;;           '(1 2 3)  ==>  '(1 2 3 (3) (2 3) (1 2 3))
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
            ((const-token? lst-sexp)
                (cond 
                    ((vector? (cadr lst-sexp)) 
                        (append (vector->list (cadr lst-sexp)) (cdr lst-sexp)))
                    ((symbol? (cadr lst-sexp))
                        (cons (symbol->string (cadr lst-sexp)) (cdr lst-sexp)))
                    (else (cdr lst-sexp))))
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
            (let ((addr address-count))
		(set! address-count (+ address-count 2))
		(add-to-consts-table rest (append table (list `(,addr ,const (,T_INTEGER ,const))))))))

(define make-rational-const
	(lambda (const rest table)
             (let ((addr address-count))
		(set! address-count (+ address-count 3))
		(add-to-consts-table rest (append table (list `(,addr ,const (,T_FRACTION ,(numerator const) ,(denominator const)))))))))
		
(define make-pair-const
	(lambda (const rest table)
             (let ((addr address-count))
		(set! address-count (+ address-count 3))
		(add-to-consts-table 
		rest 
		(append table (list `(,addr ,const (,T_PAIR ,(find-address (car const) table) ,(find-address (cdr const) table)))))))))
		
(define make-vector-const
	(lambda (const rest table)
            (let ((addr address-count))
		(set! address-count (+ address-count (+ 2 (vector-length const))))
		(add-to-consts-table 
		rest 
		(append table (list `(,addr ,const (,T_VECTOR ,(vector-length const) 
                                                            ,@(map (lambda (elm) (find-address elm table)) (vector->list const))))))))))
													
(define make-string-const
	(lambda (const rest table)
             (let ((addr address-count))
		(set! address-count (+ address-count (+ 2 (string-length const))))
		(add-to-consts-table 
		rest 
		(append table (list `(,addr ,const (,T_STRING ,(string-length const) 
							,@(map (lambda (c) (char->integer c)) (string->list const))))))))))	

;(define make-closure-const
;	(lambda (const rest table)
;	))

(define make-char-const
	(lambda (const rest table)
             (let ((addr address-count))
		(set! address-count (+ address-count 2))
		(add-to-consts-table rest (append table (list `(,addr ,const (,T_CHAR ,(char->integer const)))))))))
		
(define make-symbol-const
	(lambda (const rest table)
             (let ((addr address-count))
		(set! address-count (+ address-count 2))
		(add-to-consts-table rest (append table (list `(,addr ,const (,T_SYMBOL ,(find-address (symbol->string const) table)))))))))
	
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
                            (add-to-consts-table rest table))
                        ((is-member const table) (add-to-consts-table rest table))
                        ((integer? const) (make-integer-const const rest table)) 
                        ((rational? const) (make-rational-const const rest table))
                        ((pair? const) (make-pair-const const rest table))
                        ((vector? const) (make-vector-const const rest table))
                        ((string? const) (make-string-const const rest table))									
                        ;;;;((closure)
                        ((char? const) (make-char-const const rest table)) 
                        ((symbol? const) (make-symbol-const const rest table))							
                        (else 'error))))))

                        
(define fvar-token? (make-token-pred 'fvar))
            
;; (define make-append-to-table
;;     (lambda (table)
;;         (lambda (x1)
;;             (display `(x1: ,x1))
;;             (set! table (cons table x1)))))
;; 
;; 
;; (define append-to-global (make-append-to-table global-var-table))
            
            
;; (define add-to-global-var-table
;;     (lambda (lst-sexp)
;;         (cond 
;;             ((or (null? lst-sexp) (not (pair? lst-sexp))) '())
;;             ((fvar-token? lst-sexp) (append-to-global (cdr lst-sexp)))
;;             (else (append-to-global (append (add-to-global-var-table (car lst-sexp)) 
;;                                             (add-to-global-var-table (cdr lst-sexp))))))))
    
(define extract-fvars
    (lambda (lst-sexp)
        (cond 
            ((or (null? lst-sexp) (not (pair? lst-sexp))) '())
            ((fvar-token? lst-sexp) (cdr lst-sexp))
            (else (append (extract-fvars (car lst-sexp)) 
                                            (extract-fvars (cdr lst-sexp)))))))
                                            
(define add-to-global-var-table
    (lambda (fvars-list table)
        (if (null? fvars-list) table
            (let ((fvar (car fvars-list))
                    (rest (cdr fvars-list))
                    (addr address-count))
                    (cond 
                        ((is-member fvar table) (add-to-global-var-table rest table))
                        (else (set! address-count (+ 1 address-count))
                            (add-to-global-var-table rest (append table (list `(,addr ,fvar (-1)))))))))
    ))
    
(define compile-scheme-file 
    (lambda (src-file trg-file)
        (let* ((lst-sexp (pipeline (file->list src-file))) 
                (consts (remove-dups (fold-left append '() (map help (extract-consts lst-sexp)))))
                (const-table (add-to-consts-table consts basic-table)))
                (set! consts-table const-table)
                (set!  global-var-table (add-to-global-var-table (remove-dups (extract-fvars lst-sexp)) '()))
                
                ;(display `(lst-sexp: ,lst-sexp))
                ;(map code-gen lst-sexp)
;;                 (newline)
;;                 (display `(const-table: ,const-table))
;;                 (newline)
;;                 (display `(global-table: ,global-var-table))
        )))

        
        
        
        
        
        
        
        
        


(define make-lable-count
    (lambda (prefix)
      (lambda ()
        (let ((n 0))
            (lambda ()
                (set! n (+ n 1))
                (string-append prefix (number->string n)))))))
                    
(define make-gen-if3-else-lable (make-lable-count "L_if3_else"))
(define gen-if3-else-lable (make-gen-if3-else-lable))
(define make-gen-if3-done-lable (make-lable-count "L_if3_done"))
(define gen-if3-done-lable (make-gen-if3-done-lable))

(define make-gen-or-done-lable (make-lable-count "L_or_done"))
(define gen-or-done-lable (make-gen-or-done-lable))       
        
        
(define code-gen-if-exp 
    (lambda (exp)
        (let
            ((test (cadr exp))
            (dit (caddr exp))
            (dif (cadddr exp))
            (false-address (find-address #f consts-table))
            (L_else (gen-if3-else-lable))
            (L_done (gen-if3-done-lable)))
            
            (string-append 
                (code-gen test) 
                "cmp rax, " (number->string (+ 1 false-address)) ";\n"
                "je " L_else ";\n"
                (code-gen dit)
                "jmp " L_done ";\n"
                L_else ":\n"
                (code-gen dif)
                L_done ":\n"))))
                
                
        
        
        
        
(define code-gen 
    (lambda (exp)
        (cond 
            ((or (null? exp) (not (pair? exp))) "")
            (else
                (let ((tag (car exp)))
                    (cond 
                        ((equal? tag 'if3) (code-gen-if-exp exp))))
        
       ))))
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       