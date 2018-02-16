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
(define num_of_last_param 0)

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
        
(define basic-table `((0 312177220 (,T_UNDEFINED 0))
                    (1 ,(void) (,T_VOID 0)) 
                    (2 () (,T_NIL 0)) 
                    (3 #t (,T_BOOL 1))
                    (5 #f (,T_BOOL 0))))
				
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
        ((not (pair? exp))
            (cond 
                ((symbol? exp) (cons (symbol->string exp) `(,exp)))
                ((rational? exp) (append `(,(numerator exp)) `(,(denominator exp)) `(,exp)))
                (else `(,exp))))
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
                    ((string? (cadr lst-sexp))
                        (cons (string->symbol (cadr lst-sexp)) (cdr lst-sexp)))
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
		(add-to-consts-table rest (append table (list `(,addr ,const (,T_FRACTION ,(find-address (numerator const) table) ,(find-address (denominator const) table)))))))))
		
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
                    (add-to-global-var-table rest (append table (list `(,addr ,fvar (-1)))))))))))
    
(define primitive-funcs 
    (lambda (lst)
        (append 
            lst 
            '(append apply < = > + / * - boolean? car cdr char->integer 
            char? cons denominator integer? integer->char list make-string 
            make-vector map eq? not null? number? numerator pair? procedure? rational? 
            remainder set-car! set-cdr! string-length string-ref string-set! 
            string->symbol string? symbol? symbol->string vector vector-length 
            ;vector-ref vector-set! vector? zero? our_gcd))))
            vector-ref vector-set! vector? zero? our_gcd smaller-then-bin))))
            

(define initialize-tables-to-asm
    (lambda ()
        (string-append
            (initialize-consts-table-to-asm)
            (initialize-fvars-table-to-asm)
            ;(initialize-symbols-table-to-asm)
            )))

(define const-length
    (lambda (exp)
        (length (caddr exp))))

(define const-label-generator
    (lambda (n)
        (string-append "L_const" (number->string n)))) ; 3 ==> "L_const3")
        
(define split-lst
    (lambda (lst delimiter func)
        (let* ((rest (cdr lst))
                (rvs-rest              (reverse rest))  ;'(1 2 5) ==> '(5 2 1)
                (last-elm              (car rvs-rest))  ; 1
                (all-except-last-elm   (reverse (cdr rvs-rest))) ; '(2 5)
;;                 (const-label-generator (lambda (n) (string-append "L_const" (number->string n)))) ; 3 ==> "L_const3"
                (const-labels          (map func all-except-last-elm)) ; '(2 5) ==> '("L_const2" "L_const5" ...) except for last elm
                (last-const-label      (func last-elm))
                (complete-labels       (string-append
                                            (append-str-list-with const-labels delimiter)
                                            last-const-label)))
                 complete-labels)))
                    
                    
(define gen-const-label
    (lambda (const)
        (let*
            ((addr (car const))
            (literal (caddr const))
            (type (car literal))
            (value (cdr literal)))
            (string-append 
                "L_const" (number->string addr) ":\n"
                (cond 
                    ((or 
                        (equal? type T_UNDEFINED)
                        (equal? type T_VOID)
                        (equal? type T_NIL)
                        (equal? type T_INTEGER)
                        (equal? type T_BOOL)
                        (equal? type T_CHAR)
                        (equal? type T_SYMBOL))
                        (string-append 
                            "\t" "dq MAKE_LITERAL(" (number->string type) ", " (number->string (car value)) ")\n"))
                            
                    ((equal? type T_FRACTION)
                        (let 
                            ((label-car (string-append "L_const" (number->string (car value))))
                            (label-cdr (string-append "L_const" (number->string (cadr value)))))
                            (string-append 
                                "\t" "dq MAKE_LITERAL_FRACTION(" label-car ", " label-cdr ")\n")))
                            
                    ((equal? type T_STRING) 
                        (string-append 
                            "\t" "MAKE_LITERAL_STRING " (split-lst value ", " number->string) "\n"))
                    ((equal? type T_PAIR)
                        (let 
                            ((label-car (string-append "L_const" (number->string (car value))))
                            (label-cdr (string-append "L_const" (number->string (cadr value)))))
							
                        (string-append 
                                "\t" "dq MAKE_LITERAL_PAIR(" label-car ", " label-cdr ")\n")))
                    ((equal? type T_VECTOR)
                        
;;                         (let* ((rest (cdr value))
;;                             (rvs-rest              (reverse rest))  ;'(1 2 5) ==> '(5 2 1)
;;                             (last-elm              (car rvs-rest))  ; 1
;;                             (all-except-last-elm   (reverse (cdr rvs-rest))) ; '(2 5)
;;                             (const-label-generator (lambda (n) (string-append "L_const" (number->string n)))) ; 3 ==> "L_const3"
;;                             (const-labels          (map const-label-generator all-except-last-elm)) ; '(2 5) ==> '("L_const2" "L_const5" ...) except for last elm
;;                             (last-const-label      (const-label-generator last-elm))
;;                             (complete-labels       (string-append
;;                                                         (append-str-list-with const-labels ", ")
;;                                                         last-const-label)))
                            (string-append 
                                "\t" "MAKE_LITERAL_VECTOR " (split-lst value ", " const-label-generator) "\n")))))))                            
                                
;MAKE_LITERAL_VECTOR sob8, sob7, sobInt1, sobInt2, sobInt3, sob4 


(define gen-fvar-label
    (lambda (fvar)
        (let*
            ((addr (car fvar))
            (value (cadr fvar)))
            (string-append 
                "L_glob" (number->string addr) ":\n"
				"\t dq SOB_UNDEFINED \n"))))
       
    
(define initialize-consts-table-to-asm
    (lambda ()
;;         (let*
;;             ((table-size (apply + (map const-length consts-table))))
            (string-append 
                "consts_table:\n"
                (append-str-list (map gen-const-label consts-table))
                "\n")))
				
(define initialize-fvars-table-to-asm
    (lambda ()
        (string-append 
            "global_table:\n"
            (append-str-list (map gen-fvar-label global-var-table))
            "\n")))

			
(define print-macro-str
    (string-append
        "%include \"scheme.s\"\n\n"
            
        "%macro print 2 \n\n"
        "push rdi \n"
        "push rsi \n"
        "push rax \n"
        
        "mov rdi,%1 \n"
        "mov rsi,%2 \n"
        "mov rax,0 \n"
        "call printf \n"
        
        "pop rax \n"
        "pop rsi \n"
        "pop rdi \n\n"

        "%endmacro \n\n"))

(define load-primitive-funcs
    (lambda ()
        (string-append 
            ;(ass-pred? 'null? 'null "T_NIL")
            ;(ass-pred? 'boolean? 'boolean "T_BOOL")
            ;(ass-pred? 'char? 'char "T_CHAR")
            ;(ass-pred? 'integer? 'integer "T_INTEGER")
            ;(ass-pred? 'pair? 'pair "T_PAIR")
            ;(ass-pred? 'procedure? 'procedure "T_CLOSURE") 
            ;(ass-pred? 'rational? 'rational "T_FRACTION")
            ;(ass-pred? 'string? 'string "T_STRING")
            ;(ass-pred? 'symbol? 'symbol "T_SYMBOL")
            ;(ass-pred? 'vector? 'vector "T_VECTOR")
            (ass-car)
            (ass-cdr)
            ;(ass-integer-to-char)
            ;(ass-char-to-integer)
            (ass-cons)
            ;(ass-zero?)
            ;(ass-not)
            ;(ass-eq?)
            ;(ass-string-len)
            (ass-string-ref)
            (ass-make-string)
            ;(ass-string-set!)
            ;(ass-vector-len)
            (ass-vector-ref)
            (ass-vector-set!)
            (ass-set-car!)
            (ass-set-cdr!)
            (ass-remainder)
            (ass-smaller-then-bin)
			
                 
        )))
            
;; input: ??
;; output: string of the head code in assembly
(define make-prologue
    (lambda ()
        (string-append 
            print-macro-str
            (initialize-tables-to-asm)
            
            "global main\n"
            "section .text\n"
            "main:\n\n"
            "mov rax, malloc_pointer \n"
            "mov qword [rax], start_of_malloc \n\n"
            
            (load-primitive-funcs)
            )))
            
(define make-epilogue
    (lambda ()
        (string-append
            "cmp qword [rsp], L_const2 \n"
            "jne END_of_program \n"
            "add rsp, 8 \n"
            "END_of_program: \n"
            "\n"
            "ret\n"
            "\n\n"
            "section .rodata \n"
            "\t format_str: DB \"%s\", 10,0 \n"
            "\t format_num: DD \"%d\", 10,0 \n"
            "\t newline: DB 10, 0 \n"
            "\t error_msg: DB \"ERROR!!!\", 10, 0 \n"
            "\t error_num_args_msg: DB \"incorrect number of arguments\", 10, 0 \n"
            "\t error_type_msg: DB \"incorrect type\", 10, 0 \n\n"
            "L_error: \n"
            "	print format_str, error_msg \n"
            "   jmp L_END \n\n"
            "L_incorrect_num_of_args: \n"
            "	print format_str, error_num_args_msg \n"
            "   jmp L_END \n\n"
            "L_incorrect_type: \n"
            "	print format_str, error_type_msg \n"
            "   jmp L_END \n\n"
            "L_END: \n"
            
            )))
        
(define write-sob-string
    (string-append 
        "push qword [rax]\n"
        "call write_sob_if_not_void\n"
        "add rsp, 1*8\n"))

(define compile-scheme-file 
    (lambda (src-file trg-file)
        (let* ((lst-sexp (pipeline (file->list src-file))) 
                (consts (remove-dups (fold-left append '() (map help (extract-consts lst-sexp)))))
                (const-table (add-to-consts-table consts basic-table)))
                (set! consts-table const-table)
                (set! global-var-table (add-to-global-var-table (primitive-funcs (remove-dups (extract-fvars lst-sexp))) '()))

                (display `(lst-sexp: ,lst-sexp))
				;(code-gen (car lst-sexp))
                (newline)
                (display `(constlist: ,consts)) 
                (newline)
                ;(display `(const-table: ,consts-table))
                ;(initialize-consts-table-to-asm)
                ;(newline)
                ;(display `(global-table: ,global-var-table))
				
                (string->file 
                     trg-file 
                     (string-append 
                        (make-prologue)
                         (append-str-list (map (lambda (exp) (code-gen exp 0)) lst-sexp)) ; not sure how to handle the env in this line yet
						 write-sob-string
                         (make-epilogue)))
                ;(string->file trg-file (append-str-list (map code-gen lst-sexp)))
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

(define make-gen-lambda-lable (make-lable-count "L_lambda_code"))
(define gen-lambda-lable (make-gen-lambda-lable)) 
        
(define code-gen-const
    (lambda (exp env)
        (let* 
            ((value (cadr exp))
            (address (find-address value consts-table)))
            (string-append 
                "mov rax, " (string-append "L_const" (number->string address)) " \n"))))

(define code-gen-if 
    (lambda (exp env)
        (let
            ((test (cadr exp))
            (dit (caddr exp))
            (dif (cadddr exp))
            (false-address (find-address #f consts-table))
            (L_else (gen-if3-else-lable))
            (L_done (gen-if3-done-lable)))
            (string-append 
                (code-gen test env) 
                "mov rax, [rax] \n"
                "cmp rax, [L_const" (number->string false-address) "] \n"
                "je " L_else "\n"
                (code-gen dit env)
                "jmp " L_done "\n"
                L_else ":\n"
                (code-gen dif env)
                L_done ":\n"))))

                
;; input: list of srtings
;; output: appended string
;; example: '("a" "bc" "d") ==> "abcd"
(define append-str-list-with
    (lambda (str-lst extra-str)
        (fold-left
                (lambda (acc-str str) (string-append acc-str str extra-str))
                ""
                str-lst)))
                
(define append-str-list
    (lambda (str-lst)
        (append-str-list-with str-lst "")))
                
(define code-gen-seq 
    (lambda (exp env)
        (let ((rest (cadr exp)))
            (append-str-list (map (lambda (exp) (code-gen exp env)) rest)))))
            
(define code-gen-or 
    (lambda (exp env)
        (let* 
            ((rest (cadr exp))
            (rvs-rest (reverse rest))
            (last-elm (car rvs-rest))
            (all-except-last-elm (reverse (cdr rvs-rest)))
            (false-address (find-address #f consts-table))
            (L_done (gen-or-done-lable)))
            (string-append
                (append-str-list-with 
                    (map (lambda (exp) (code-gen exp env)) all-except-last-elm) 
                    (string-append
                        "mov r13, [rax] \n"
                        "cmp r13, [L_const" (number->string false-address) "]\n"
                        "jne " L_done "\n"))
                (code-gen last-elm env)
                L_done ":\n"))))

(define params-code-gen
    (lambda (params-lst env)
        (if (null? params-lst)
            ""
            (string-append
                (params-code-gen (cdr params-lst) env)
                (code-gen (car params-lst) env)
                "push rax \n"))))
	
(define code-gen-applic
    (lambda (exp env)
        (let* ((func (cadr exp))
            (params (caddr exp))
            (num-of-params (length params))
            (proc (cadr exp)))
            (string-append
                "push L_const2 \n" ;push '() to stack
                (params-code-gen params env) ;push evaluated params reversely to stack
                "push " (number->string num-of-params) "\n"
                (code-gen proc env)
                "mov rbx, [rax] \n"
                "TYPE rbx \n"
                "cmp rbx, T_CLOSURE \n"
                "jne L_error \n"
                "mov rbx, [rax] \n"
                "CLOSURE_ENV rbx \n"
                "push rbx \n"
                "mov rax, [rax] \n"
                "CLOSURE_CODE rax \n"
                "call rax \n"
                "mov rbx, qword [rsp + 8] \n"
                "add rbx, 2 \n"
                "shl rbx, 3 \n"
                "add rsp, rbx \n"))))
;;                 (if (eq? (car func) 'lambda-opt)
;;                     (if (eq? (length (cadr func)) num-of-params)
;;                         (string-append "add rsp, 8 * (3 + " (number->string (length (cadr func))) ") \n")
;;                         (string-append "add rsp, 8 * (3 + " (number->string (+ 1 (length (cadr func)))) ") \n"))
;;                    (string-append "add rsp, 8 * (3 + " (number->string num-of-params) ") \n"))))))

                
(define override-frame
    (lambda (index)
            (if (zero? index)
                ""
                (string-append
                    "sub r8 , 8 \n"
                    "sub r9 , 8 \n"
                    "mov rcx, [r8] \n"
                    "mov [r9], rcx \n"
                    (override-frame (- index 1))))))
                    
                    
(define code-gen-tc-applic
    (lambda (exp env)
        (let* ((params (caddr exp))
            (num-of-params (length params))
            (proc (cadr exp)))
            (string-append
                "push L_const2 \n" ;push '() to stack
                (params-code-gen params env) ;push evaluated params reversely to stack
                "push " (number->string num-of-params) "\n"
                (code-gen proc env)
                "mov rbx, [rax] \n"
                "TYPE rbx \n"
                "cmp rbx, T_CLOSURE \n"
                "jne L_error \n"
                "mov rbx, [rax] \n"
                "CLOSURE_ENV rbx \n"
                "push rbx \n"
                ; until here same as the regular applic

                "mov rdi, [rbp+8] \n"
                "push rdi \n"
                
                "mov r8, rbp \n"
                
                "mov r9, r8 \n"
                ;"add r9, 8*" (number->string (+ 5 num-of-params)) " \n"
                "add r9, 8*" (number->string (+ 4 num-of-params)) " \n"

                (override-frame (+ 4 num-of-params)) ; copy the following from new frame to old frame: null, num_of_args, env (4), n-args (num-of-params)
                "mov rsp, r9 \n"
                "mov rax, [rax] \n"
                "CLOSURE_CODE rax \n"
                "jmp rax \n"))))
                
(define code-gen-set
    (lambda (exp env)
        (let ((tag (caadr exp))
            (lst_var (cadr exp))
            (e (caddr exp)))
            (cond 
                ((equal? tag 'pvar)
                    (let ((minor (caddr lst_var)))
                        (string-append
                            (code-gen e env)
                            "mov qword [rbp + (4 + " (number->string minor) ") * 8], rax \n"
                            "mov rax, L_const1 \n")))
                ((equal? tag 'bvar)
                    (let ((major (caddr lst_var))
                        (minor (cadddr lst_var)))
                        (string-append
                            (code-gen e env)
                            "mov rbx, qword [rbp +  2 * 8] \n"
                            "mov rbx, qword [rbx + " (number->string major) " * 8] \n"
                            "mov qword [rbx + " (number->string minor) " * 8], rax \n"
                            "mov rax, L_const1 \n")))
                ((equal? tag 'fvar)
                    (let* ((value (cadr lst_var))
                        (address (find-address value global-var-table)))
                        (string-append
                            (code-gen e env)
                            "mov rbx, [rax] \n"
                            "mov rax, L_glob" (number->string address) "\n"
                            "mov [rax], rbx \n"
                            "mov rax, L_const1 \n")))))))

(define code-gen-box
    (lambda (exp env)
        (let ((minor (car (cddadr exp)))) 
            (string-append
                "mov rax, [malloc_pointer] \n"
                "my_malloc 8 \n"
                "mov rbx, qword [rbp + (4 + " (number->string minor) ") * 8] \n"
                "mov [rax], rbx \n" ))))
		
(define code-gen-box-get
    (lambda (exp env)
        (string-append
            (code-gen (cadr exp) env)
            "mov rax, [rax] \n")))
			
(define code-gen-box-set
    (lambda (exp env)
        (let 
            ((lst_var (cadr exp))
            (e (caddr exp)))
                (string-append 
                    (code-gen e env)
                    "mov rbx, rax \n"
                    (code-gen lst_var env)
                    "mov [rax], rbx \n"
                    "mov rax, L_const1 \n"))))
							
(define code-gen-pvar
    (lambda (exp env)
        (let ((minor (caddr exp)))
            (string-append 
                "mov rax, qword [rbp + (4 + " (number->string minor) ") * 8] \n"))))
	
(define code-gen-bvar
    (lambda (exp env)
        (let ((major (caddr exp))
            (minor (cadddr exp)))
                (string-append 
                    "mov rax, qword [rbp +  2 * 8] \n"
                    "mov rax, qword [rax + " (number->string major) " * 8] \n"
                    "mov rax, qword [rax + " (number->string minor) " * 8] \n"))))	   
	
(define code-gen-fvar	
    (lambda (exp env)
	(let* 
            ((value (cadr exp))
            (address (find-address value global-var-table)))
            (string-append 
                "mov rax, L_glob" (number->string address) " \n"))))

(define code-gen-def	
    (lambda (exp env)
        (let* ((value (cadadr exp))
            (address (find-address value global-var-table))
            (e (caddr exp)))
                (string-append
                    (code-gen e env)
                    "mov rax, [rax] \n"
                    "mov [L_glob" (number->string address) "], rax \n"
                    "mov rax, L_const1 \n"))))

(define copy-to-memory-params
	(lambda (num-of-params reg count)
            (if (zero? num-of-params)
                ""
                (string-append
                    "add " reg ", 8*" (number->string count) " \n"
                    "mov rdx,[rbp + (4 + " (number->string count) ")*8] \n"
                    "mov qword [" reg "], rdx \n" 
                    (copy-to-memory-params (- num-of-params 1) reg (+ 1 count))))))
                            
(define copy-to-memory-env
	(lambda (num-of-params reg count)
            (if (zero? num-of-params)
                ""
                (string-append
                    "add " reg ", 8*" (number->string count) " \n"
                    "mov rdx, [rdx + 8*" (number->string count) "] \n"
                    "mov qword [" reg "], rdx \n" 
                    (copy-to-memory-env (- num-of-params 1) reg (+ 1 count))))))
				
(define code-gen-lambda-simple
    (lambda (exp env)
        (let* 
            ((params (cadr exp))
            (num-of-params (length params))
            (last-params num_of_last_param)
            (body (cddr exp))
            (code-lable (gen-lambda-lable))
            (end-lambda-lable (string-append "END_" code-lable)))
                (set! num_of_last_param num-of-params)
                (string-append
                    "mov rax, [malloc_pointer] \n"
                    "my_malloc 16 \n"
                    "mov rbx, [malloc_pointer] \n"
                    "my_malloc (8*" (number->string (+ 1 env)) ") \n"
                    "mov rcx, [malloc_pointer] \n"
                    "my_malloc (8*" (number->string last-params) ") \n"
                    "mov rsi, rbx \n"
                    "mov qword [rsi], rcx \n"
                    (copy-to-memory-params last-params "rcx" 0)
                    "add rsi, 8 \n"
                    "mov rdx,[rbp + 2*8] \n"
                    (copy-to-memory-env env "rsi" 0)
                    "MAKE_LITERAL_CLOSURE rax, rbx, " code-lable "\n"
                    ;"mov rax, [rax] \n"
                    "jmp " end-lambda-lable "\n"
                    code-lable ": \n"
                    "push rbp \n"
                    "mov rbp, rsp \n"
                    (append-str-list 
                        (map (lambda (exp) (code-gen exp (+ 1 env))) body))
                    "leave \n"
                    "ret \n"
                    end-lambda-lable ": \n"))))

(define create_list_from_lambda_opt_params
    (lambda (i end)
        (string-append
            "mov rax, L_const2 ;Initialize rax with nil \n\n"
            ";If at this point " end " is 0, it means that there are no args in the list so we do nothing! \n"
            "cmp " end ", 0 \n"
            "je L_END_for_lambda_opt \n"
                        
           
            "push qword L_const2   ; push nil to as first argument \n"
            "push qword [" i "]      ; push the first element of the iteration \n"
            "push 2            ; push num of args \n"
            "push L_const2     ; push empty env (only to keep the form) \n"
            "call L_cons       ; the return value will be stored in rax \n"
            "add rsp, 8*4 \n"
            
        
            "L_start_for_lambda_opt: \n"
            "   sub " i ", 8 \n"
            "   sub " end ", 1 \n"
            "   cmp " end ", 0 \n"
            "   je L_END_for_lambda_opt \n"
            "   \n"
            "   push rax          ; push the last created pair to be the cdr \n"
            "   push qword [" i "]      ; push next arg to be the car of the pair \n"
            "   push 2            ; push num of args (car and cdr) \n"
            "   push L_const2     ; push empty env (only to keep the form) \n"
            "   call L_cons       ; the return value will be stored in rax \n"
            "   add rsp, 8*4 \n"
            "   jmp L_start_for_lambda_opt \n"
            
            "L_END_for_lambda_opt: \n"
            ";At this point, the whole list is stored in rax \n"
            
        )))

;start = pointer to the start of the new frame
(define rearrange_stack_lambda_opt
    (lambda (start end lst)
        (string-append
					
            "mov qword [" start "], " lst " ; copy the args list to the bottom of the new frame \n"
			
            "L_start_rearrange_lambda_opt: \n"
            "   sub " start ", 8 \n"
            "   mov rsi, [rbp + 8*" end "] \n"
            "   mov qword [" start "], rsi \n"
            
            "   \n"
            "   sub " end ", 1 \n"
            "   cmp " end ", 0 \n"
            "   jge L_start_rearrange_lambda_opt \n"
            "mov rsp, " start " ;update rsp to point to the top of the rearranged frame \n"
            "mov rbp, rsp \n"
        )))
        
        
(define code-gen-lambda-opt
    (lambda (exp env)
        (let* 
            ((params (cadr exp))
            (num-of-params (length params))
            (last-params num_of_last_param)
            (body (cddr exp))
            (code-lable (gen-lambda-lable))
            (end-lambda-lable (string-append "END_" code-lable)))
                (set! num_of_last_param num-of-params)
                (string-append
                    "mov rax, [malloc_pointer] \n"
                    "my_malloc 16 \n"
                    "mov rbx, [malloc_pointer] \n"
                    "my_malloc (8*" (number->string (+ 1 env)) ") \n"
                    "mov rcx, [malloc_pointer] \n"
                    "my_malloc (8*" (number->string last-params) ") \n"
                    "mov rsi, rbx \n"
                    "mov qword [rsi], rcx \n"
                    (copy-to-memory-params last-params "rcx" 0)
                    "add rsi, 8 \n"
                    "mov rdx,[rbp + 2*8] \n"
                    (copy-to-memory-env env "rsi" 0)
                    "MAKE_LITERAL_CLOSURE rax, rbx, " code-lable "\n"
                    ;"mov rax, [rax] \n"
                    "jmp " end-lambda-lable "\n"
                    code-lable ": \n"
                    "push rbp \n"
                    "mov rbp, rsp \n"
                    
                    "mov r12, rbp \n"
                    "add r12, 8*3 \n"
                    "mov r13, [r12]   ;store total num of params in r13 \n"
                    "mov r10, r13 \n"
                    "shl r10, 3  ; r10 = r13*8\n"
                    "add r12, r10 \n"
                    "mov r11, r12 ; In order to keep the initial point \n"
                    "add r11, 8  ; points to nil which is the first argument in any frame \n"
					
                    "sub r13, " (number->string num-of-params) " \n"
                    ;"cmp r13, 0 \n"
                    ;"jne continue \n"
                    ;"add r11, 8  ; points to nil which is the first argument in any frame \n"
                    ;"continue: \n"

                    "\n"
                    ";r12 = poinetr to last param (first index of iteration) \n"
                    ";r13 = num of params (only those from the list) \n"
                    ";r11 = nil which is the first argument in any frame \n"

                    (create_list_from_lambda_opt_params "r12" "r13")
					
                    ;"mov r13, [rbp+ 8*3] \n"
                    ;"cmp r13, " (number->string num-of-params) " \n"
                    ;"je L_same_num_of_args \n"
                    "mov qword [rbp + 8*3], " (number->string (+ num-of-params 1)) " \n"
                    ;"L_same_num_of_args: \n"
                    "mov r12, " (number->string (+ num-of-params 3)) " \n"
					
                    (rearrange_stack_lambda_opt "r11" "r12" "rax")

;;                     "cmp rax, L_const2 \n"
;;                     "jne continue \n"
;;                     "add r11, 8  ; points to nil which is the first argument in any frame \n"
;;                     "continue: \n"
                    (append-str-list 
                        (map (lambda (exp) (code-gen exp (+ 1 env))) body))
                    "leave \n"
                    "ret \n"
                    end-lambda-lable ": \n"))))
        

(define string->file
    (lambda (file-name str)
        (let 
            ((file (open-output-file file-name 'replace)))
            (display str file)
            (close-output-port file))))
        
(define code-gen 
    (lambda (exp env)
        (cond 
            ((or (null? exp) (not (pair? exp))) "")
            (else
                (let ((tag (car exp)))
                    (cond 
                        ((equal? tag 'const) (code-gen-const exp env))
                        ((equal? tag 'if3) (code-gen-if exp env))
                        ((equal? tag 'seq) (code-gen-seq exp env))
                        ((equal? tag 'or) (code-gen-or exp env))
                        ((equal? tag 'applic) (code-gen-applic exp env))
                        ((equal? tag 'tc-applic) (code-gen-tc-applic exp env))
                        ((equal? tag 'set) (code-gen-set exp env))
                        ((equal? tag 'box) (code-gen-box exp env))
                        ((equal? tag 'box-get) (code-gen-box-get exp env))
                        ((equal? tag 'box-set) (code-gen-box-set exp env))
                        ((equal? tag 'pvar) (code-gen-pvar exp env))
                        ((equal? tag 'bvar) (code-gen-bvar exp env))
                        ((equal? tag 'fvar) (code-gen-fvar exp env))
                        ((equal? tag 'define) (code-gen-def exp env))
                        ((equal? tag 'lambda-simple) (code-gen-lambda-simple exp env))
                        ((equal? tag 'lambda-opt) (code-gen-lambda-opt exp env))
       ))))))

(define ass-pred?
    (lambda (pred? pred str-type)      
        (let* 
            ((address (find-address pred? global-var-table))
            (str-pred (symbol->string pred)))
            (string-append 
                "jmp L_make_" str-pred "\n"
                "L_" str-pred ": \n"
                "push rbp \n"
                "mov rbp, rsp \n"
                "mov rbx, [rbp + 8*3] \n"
                "cmp rbx , 1 \n"
                "jne L_error \n"
                "mov rbx, [rbp + 8*4] \n"
                "mov rbx, [rbx] \n"
                "TYPE rbx \n"
                "cmp rbx, " str-type "\n"
                "jne L_" str-pred "_else \n "
                "mov rax, L_const3 \n"
                "jmp END_" str-pred "\n"
                "L_" str-pred "_else: \n"
                "mov rax, L_const5 \n" 
                "END_" str-pred ": \n"
                "leave \n"
                "ret \n"
                
                "L_make_" str-pred ": \n"
                "mov rax, [malloc_pointer] \n"
                "my_malloc 16 \n"
                "MAKE_LITERAL_CLOSURE rax, L_const2, L_" str-pred " \n"
                "mov rax, [rax] \n"
                "mov [L_glob" (number->string address) "], rax \n\n" ))))       
       
(define ass-car
    (lambda ()      
        (let* 
            ((address (find-address 'car global-var-table)))
            (string-append 
                "jmp L_make_car \n" 
                "L_car: \n"
                "push rbp \n"
                "mov rbp, rsp \n"
                "mov rbx, [rbp + 8*3] \n"
                "cmp rbx , 1 \n"
                "jne L_incorrect_num_of_args \n"
                "mov rbx, [rbp + 8*4] \n"
				"mov rbx, [rbx] \n"
				"mov rax, rbx \n"
                "TYPE rbx \n"
                "cmp rbx, T_PAIR \n"
                "jne L_incorrect_type \n "
                "MY_CAR rax \n"
				
                "leave \n"
                "ret \n"
                
                "L_make_car: \n"
                "mov rax, [malloc_pointer] \n"
                "my_malloc 16 \n"
                "MAKE_LITERAL_CLOSURE rax, L_const2, L_car \n"
                "mov rax, [rax] \n"
                "mov [L_glob" (number->string address) "], rax \n\n" )))) 

(define ass-cdr
    (lambda ()      
        (let* 
            ((address (find-address 'cdr global-var-table)))
            (string-append 
                "jmp L_make_cdr \n" 
                "L_cdr: \n"
                "push rbp \n"
                "mov rbp, rsp \n"
                "mov rbx, [rbp + 8*3] \n"
                "cmp rbx , 1 \n"
                "jne L_incorrect_num_of_args \n"
                "mov rbx, [rbp + 8*4] \n"
				"mov rbx, [rbx] \n"
				"mov rax, rbx \n"
                "TYPE rbx \n"
                "cmp rbx, T_PAIR \n"
                "jne L_incorrect_type \n "
                "MY_CDR rax \n"
                "leave \n"
                "ret \n"
                
                "L_make_cdr: \n"
                "mov rax, [malloc_pointer] \n"
                "my_malloc 16 \n"
                "MAKE_LITERAL_CLOSURE rax, L_const2, L_cdr \n"
                "mov rax, [rax] \n"
                "mov [L_glob" (number->string address) "], rax \n\n" ))))

(define ass-integer-to-char
    (lambda ()      
        (let* 
            ((address (find-address 'integer->char global-var-table)))
            (string-append 
                "jmp L_make_integer_to_char \n" 
                "L_integer_to_char: \n"
                "push rbp \n"
                "mov rbp, rsp \n"
                "mov rbx, [rbp + 8*3] \n"
                "cmp rbx , 1 \n"
                "jne L_incorrect_num_of_args \n"
                "mov rax, [rbp + 8*4] \n"
                "mov rax, [rax] \n"
                "mov rbx, rax \n"
                "TYPE rax \n"
                "cmp rax, T_INTEGER \n"
                "jne L_incorrect_type \n "
                "DATA rbx \n"
                "mov rax, [malloc_pointer] \n"
                "my_malloc 8 \n"
                "mov qword [rax],  rbx \n"
                "shl qword [rax], 4 \n"
                "or qword [rax], T_CHAR \n"
                "leave \n"
                "ret \n"
                
                "L_make_integer_to_char: \n"
                "mov rax, [malloc_pointer] \n"
                "my_malloc 16 \n"
                "MAKE_LITERAL_CLOSURE rax, L_const2, L_integer_to_char \n"
                "mov rax, [rax] \n"
                "mov [L_glob" (number->string address) "], rax \n\n" ))))
				
(define ass-char-to-integer
    (lambda ()      
        (let* 
            ((address (find-address 'char->integer global-var-table)))
            (string-append 
                "jmp L_make_char_to_integer \n" 
                "L_char_to_integer: \n"
                "push rbp \n"
                "mov rbp, rsp \n"
                "mov rbx, [rbp + 8*3] \n"
                "cmp rbx , 1 \n"
                "jne L_incorrect_num_of_args \n"
                "mov rax, [rbp + 8*4] \n"
                "mov rax, [rax] \n"
                "mov rbx, rax \n"
                "TYPE rax \n"
                "cmp rax, T_CHAR \n"
                "jne L_incorrect_type \n "
                "DATA rbx \n"
                "mov rax, [malloc_pointer] \n"
                "my_malloc 8 \n"
                "mov qword [rax],  rbx \n"
                "shl qword [rax], 4 \n"
                "or qword [rax], T_INTEGER \n"
                "leave \n"
                "ret \n"
                
                "L_make_char_to_integer: \n"
                "mov rax, [malloc_pointer] \n"
                "my_malloc 16 \n"
                "MAKE_LITERAL_CLOSURE rax, L_const2, L_char_to_integer \n"
                "mov rax, [rax] \n"
                "mov [L_glob" (number->string address) "], rax \n\n" ))))
                
(define ass-cons
    (lambda ()      
        (let* 
            ((address (find-address 'cons global-var-table)))
            (string-append 
                "jmp L_make_cons \n"
                "L_cons: \n"
                "push rbp \n"
                "mov rbp, rsp \n"
                
                             
                "mov rbx, [rbp + 8*3] \n"
                "cmp rbx , 2 \n"
                "jne L_incorrect_num_of_args \n"
                "mov r8, [malloc_pointer] \n"
                "my_malloc 8 \n"
                "mov rbx,[rbp + 8*4] \n"
                "mov rbx, [rbx] \n"
                "mov [r8], rbx  ; here stored car \n"
                
                "mov r9, [malloc_pointer] \n"
                "my_malloc 8 \n"
                "mov rbx,[rbp + 8*5] \n"
                "mov rbx, [rbx] \n"
                "mov [r9], rbx ; here stored cdr \n"
                
                "\n;allocate memory for pair in heap in rax \n"
                "mov r10, [malloc_pointer] \n"
                "my_malloc 8 \n"
                "MAKE_MALLOC_LITERAL_PAIR r10, r8, r9\n"
                "mov rax, r10 \n"
           
           
                "leave \n"
                "ret \n\n"
                
                "L_make_cons: \n"
                "mov rax, [malloc_pointer] \n"
                "my_malloc 16 \n"
                "MAKE_LITERAL_CLOSURE rax, L_const2, L_cons \n"
                "mov rax, [rax] \n"
                "mov [L_glob" (number->string address) "], rax \n\n" ))))
                
(define ass-zero?
    (lambda ()      
        (let* 
            ((address (find-address 'zero? global-var-table)))
            (string-append 
                "jmp L_make_zero \n" 
                "L_zero: \n"
                "push rbp \n"
                "mov rbp, rsp \n"
                "mov rbx, [rbp + 8*3] \n"
                "cmp rbx , 1 \n"
                "jne L_incorrect_num_of_args \n"
                "mov rbx, [rbp + 8*4] \n"
				"mov rbx, [rbx] \n"
                "DATA rbx \n"
                "cmp rbx, 0 \n"
                "jne not_zero \n"
                "mov rax, L_const3 \n"
                "jmp END_zero \n"
                "not_zero: \n"
                "mov rax, L_const5 \n"
                "END_zero: \n"
                "leave \n"
                "ret \n"
                
                "L_make_zero: \n"
                "mov rax, [malloc_pointer] \n"
                "my_malloc 16 \n"
                "MAKE_LITERAL_CLOSURE rax, L_const2, L_zero \n"
                "mov rax, [rax] \n"
                "mov [L_glob" (number->string address) "], rax \n\n" ))))
                
(define ass-not
    (lambda ()      
        (let* 
            ((address (find-address 'not global-var-table)))
            (string-append 
                "jmp L_make_not \n" 
                "L_not: \n"
                "push rbp \n"
                "mov rbp, rsp \n"
                "mov rbx, [rbp + 8*3] \n"
                "cmp rbx , 1 \n"
                "jne L_incorrect_num_of_args \n"
                "mov rbx, [rbp + 8*4] \n"
				"mov rbx, [rbx] \n"
                "DATA rbx \n"
                "mov rax, L_const5 \n"
                "DATA rax \n"
                "cmp rbx,rax \n"
                "jne not_not \n"
                "mov rax, L_const3 \n"
                "jmp END_not \n"
                "not_not: \n"
                "mov rax, L_const5 \n"
                "END_not: \n"
                "leave \n"
                "ret \n"
                
                "L_make_not: \n"
                "mov rax, [malloc_pointer] \n"
                "my_malloc 16 \n"
                "MAKE_LITERAL_CLOSURE rax, L_const2, L_not \n"
                "mov rax, [rax] \n"
                "mov [L_glob" (number->string address) "], rax \n\n" ))))
                
(define ass-eq?
    (lambda ()      
        (let* 
            ((address (find-address 'eq? global-var-table)))
            (string-append 
                "jmp L_make_eq \n" 
                "L_eq: \n"
                "push rbp \n"
                "mov rbp, rsp \n"
                "mov rbx, [rbp + 8*3] \n"
                "cmp rbx , 2 \n"
                "jne L_incorrect_num_of_args \n"
                "mov rbx, [rbp + 8*4] \n"
                "mov rcx, [rbp + 8*5] \n"
                "cmp rbx, rcx \n"
                "jne not_eq \n" 
                "mov rax, L_const3 \n"
                "jmp END_eq \n"
                "not_eq: \n"
                "mov rax, L_const5 \n"
                "END_eq: \n"
                "leave \n"
                "ret \n"
                
                "L_make_eq: \n"
                "mov rax, [malloc_pointer] \n"
                "my_malloc 16 \n"
                "MAKE_LITERAL_CLOSURE rax, L_const2, L_eq \n"
                "mov rax, [rax] \n"
                "mov [L_glob" (number->string address) "], rax \n\n" ))))
				
(define ass-string-len
    (lambda ()      
        (let* 
            ((address (find-address 'string-length global-var-table)))
            (string-append 
                "jmp L_make_string_len \n" 
                "L_string_len: \n"
                "push rbp \n"
                "mov rbp, rsp \n"
                "mov rbx, [rbp + 8*3] \n"
                "cmp rbx , 1 \n"
                "jne L_incorrect_num_of_args \n"
                "mov rax, [rbp + 8*4] \n"
                "mov rax, [rax] \n"
                "mov rbx, rax \n"
                "TYPE rax \n"
                "cmp rax, T_STRING \n"
                "jne L_incorrect_type \n "
                "STRING_LENGTH rbx \n"
                "mov rax, [malloc_pointer] \n"
                "my_malloc 8 \n"
                "mov qword [rax],  rbx \n"
                "shl qword [rax], 4 \n"
                "or qword [rax], T_INTEGER \n"
                "leave \n"
                "ret \n"
                
                "L_make_string_len: \n"
                "mov rax, [malloc_pointer] \n"
                "my_malloc 16 \n"
                "MAKE_LITERAL_CLOSURE rax, L_const2, L_string_len \n"
                "mov rax, [rax] \n"
                "mov [L_glob" (number->string address) "], rax \n\n" ))))

(define ass-string-ref
    (lambda ()      
        (let* 
            ((address (find-address 'string-ref global-var-table)))
            (string-append 
                "jmp L_make_string_ref \n" 
                "L_string_ref: \n"
                "push rbp \n"
                "mov rbp, rsp \n"
                "mov rbx, [rbp + 8*3] \n"
                "cmp rbx , 2 \n"
                "jne L_incorrect_num_of_args \n"
                "mov rax, [rbp + 8*4] \n"
                "mov rax, [rax] \n"
                "mov rbx, rax \n"
                "TYPE rbx \n"
                "cmp rbx, T_STRING \n"
                "jne L_incorrect_type \n "
                "mov rcx, [rbp + 8*5] \n"
                "mov rcx, [rcx] \n"
                "mov rbx, rcx \n"
                "TYPE rbx \n"
                "cmp rbx, T_INTEGER \n"
                "jne L_incorrect_type \n "
                "DATA rcx \n"
                "mov rdx, 0 \n"
                "STRING_REF dl, rax, rcx \n"
                "mov rax, [malloc_pointer] \n"
                "my_malloc 8 \n"
                "mov qword [rax],  rdx \n"
                "shl qword [rax], 4 \n"
                "or qword [rax], T_CHAR \n"
                "leave \n"
                "ret \n"
                
                "L_make_string_ref: \n"
                "mov rax, [malloc_pointer] \n"
                "my_malloc 16 \n"
                "MAKE_LITERAL_CLOSURE rax, L_const2, L_string_ref \n"
                "mov rax, [rax] \n"
                "mov [L_glob" (number->string address) "], rax \n\n" ))))

(define ass-string-set!
    (lambda ()      
        (let* 
            ((address (find-address 'string-set! global-var-table)))
            (string-append 
                "jmp L_make_string_set \n" 
                "L_string_set: \n"
                "push rbp \n"
                "mov rbp, rsp \n"
                "mov rbx, [rbp + 8*3] \n"
                "cmp rbx , 3 \n"
                "jne L_incorrect_num_of_args \n"
                "mov rax, [rbp + 8*4] \n"
                "mov rax, [rax] \n"
                "mov rbx, rax \n"
                "TYPE rbx \n"
                "cmp rbx, T_STRING \n"
                "jne L_incorrect_type \n "
                "mov rcx, [rbp + 8*5] \n"
                "mov rcx, [rcx] \n"
                "mov rbx, rcx \n"
                "TYPE rbx \n"
                "cmp rbx, T_INTEGER \n"
                "jne L_incorrect_type \n "
                "DATA rcx \n"
                "mov rdx, [rbp + 8*6] \n"
                "mov rdx, [rdx] \n"
                "mov rbx, rdx \n"
                "TYPE rbx \n"
                "cmp rbx, T_CHAR \n"
                "jne L_incorrect_type \n"
                "DATA rdx \n"
                "STRING_ELEMENTS rax \n"
                "add rax, rcx \n"
                "mov byte [rax], dl \n"
                "mov rax, L_const1 \n"
                "leave \n"
                "ret \n"
                
                "L_make_string_set: \n"
                "mov rax, [malloc_pointer] \n"
                "my_malloc 16 \n"
                "MAKE_LITERAL_CLOSURE rax, L_const2, L_string_set \n"
                "mov rax, [rax] \n"
                "mov [L_glob" (number->string address) "], rax \n\n" ))))				

(define ass-make-string
    (lambda ()      
        (let* 
            ((address (find-address 'make-string global-var-table)))
            (string-append 
                "jmp L_make_make_string \n" 
                "L_make_string: \n"
                "push rbp \n"
                "mov rbp, rsp \n"
                "mov rbx, [rbp + 8*3] \n"
                "cmp rbx , 2 \n"
                "jne L_incorrect_num_of_args \n"
                "mov rax, [rbp + 8*4] \n"
                "mov rax, [rax] \n"
                "mov rbx, rax \n"
                "TYPE rbx \n"
                "cmp rbx, T_INTEGER \n"
                "jne L_incorrect_type \n "
                "mov rbx, rax \n"
                "DATA rbx \n"
                "mov rcx, [rbp + 8*5] \n"
                "mov rcx, [rcx] \n"
                "mov rdx, rcx \n"
                "TYPE rdx \n"
                "cmp rdx, T_CHAR \n"
                "jne L_incorrect_type \n "
                
                
                "mov r10, rbx ;save the length \n"
                
                "mov r9, rbx                        ;use r9 as size to malloc \n"
                "mov r8 , 64                         ;space for length \n"
                "add r9, r8                         ;make space for the length\n"
                "add r9, 4                          ;make space for the type \n"
                "push r10 \n"
                "push r8 \n"
                "push r9 \n"
                
                "DATA rcx \n"
                
                "pop r9                             ;restore r9 \n"
                "mov rax, [malloc_pointer] \n"
                "mov rdx, rax \n"
                "my_malloc r9 \n"
                "L_make_string_loop: \n"
                "cmp rbx, 0 \n"
                "je L_end_loop \n"
                "mov [rdx],rcx \n"
                "add rdx, 1 \n"
                "sub rbx, 1 \n"
                "jmp L_make_string_loop \n"
                "L_end_loop: \n"
                
                "pop r8 \n"
                "pop r10 \n"
		"shl qword [rax], 4 \n"
                "or qword [rax], T_STRING \n"
                "shl qword [rax], 64 \n"
                "or qword [rax], r10 \n"
                
                "leave \n"
                "ret \n"
                
                "L_make_make_string: \n"
                "mov rax, [malloc_pointer] \n"
                "my_malloc 16 \n"
                "MAKE_LITERAL_CLOSURE rax, L_const2, L_make_string \n"
                "mov rax, [rax] \n"
                "mov [L_glob" (number->string address) "], rax \n\n" ))))				
				
(define ass-vector-len
    (lambda ()      
        (let* 
            ((address (find-address 'vector-length global-var-table)))
            (string-append 
                "jmp L_make_vector_len \n" 
                "L_vector_len: \n"
                "push rbp \n"
                "mov rbp, rsp \n"
                "mov rbx, [rbp + 8*3] \n"
                "cmp rbx , 1 \n"
                "jne L_incorrect_num_of_args \n"
                "mov rax, [rbp + 8*4] \n"
                "mov rax, [rax] \n"
                "mov rbx, rax \n"
                "TYPE rax \n"
                "cmp rax, T_VECTOR \n"
                "jne L_incorrect_type \n "
                "VECTOR_LENGTH rbx \n"
                "mov rax, [malloc_pointer] \n"
                "my_malloc 8 \n"
                "mov qword [rax],  rbx \n"
                "shl qword [rax], 4 \n"
                "or qword [rax], T_INTEGER \n"
                "leave \n"
                "ret \n"
                
                "L_make_vector_len: \n"
                "mov rax, [malloc_pointer] \n"
                "my_malloc 16 \n"
                "MAKE_LITERAL_CLOSURE rax, L_const2, L_vector_len \n"
                "mov rax, [rax] \n"
                "mov [L_glob" (number->string address) "], rax \n\n" ))))
                
(define ass-vector-set!
    (lambda ()      
        (let* 
            ((address (find-address 'vector-set! global-var-table)))
            (string-append 
                "jmp L_make_vector_set \n" 
                "L_vector_set: \n"
                "push rbp \n"
                "mov rbp, rsp \n"
                "mov rbx, [rbp + 8*3] \n"
                "cmp rbx , 3 \n"
                "jne L_incorrect_num_of_args \n"
                "mov rax, [rbp + 8*4] \n"
                "mov rax, [rax] \n"
                "mov rbx, rax \n"
                "TYPE rbx \n"
                "cmp rbx, T_VECTOR \n"
                "jne L_incorrect_type \n "
                "mov rcx, [rbp + 8*5] \n"
                "mov rcx, [rcx] \n"
                "mov rbx, rcx \n"
                "TYPE rbx \n"
                "cmp rbx, T_INTEGER \n"
                "jne L_incorrect_type \n "
                "DATA rcx \n"
                "mov rdx, [rbp + 8*6] \n"
                
                
                "VECTOR_ELEMENTS rax \n"
                "shl rcx, 3 \n"
                "add rax, rcx \n"
                "mov [rax], rdx \n"
                "mov rax, L_const1 \n"
                "leave \n"
                "ret \n"
                
                "L_make_vector_set: \n"
                "mov rax, [malloc_pointer] \n"
                "my_malloc 16 \n"
                "MAKE_LITERAL_CLOSURE rax, L_const2, L_vector_set \n"
                "mov rax, [rax] \n"
                "mov [L_glob" (number->string address) "], rax \n\n" ))))
                
(define ass-vector-ref
    (lambda ()      
        (let* 
            ((address (find-address 'vector-ref global-var-table)))
            (string-append 
                "jmp L_make_vector_ref \n" 
                "L_vector_ref: \n"
                "push rbp \n"
                "mov rbp, rsp \n"
                "mov rbx, [rbp + 8*3] \n"
                "cmp rbx , 2 \n"
                "jne L_incorrect_num_of_args \n"
                "mov rax, [rbp + 8*4] \n"
                "mov rax, [rax] \n"
                "mov rbx, rax \n"
                "TYPE rbx \n"
                "cmp rbx, T_VECTOR \n"
                "jne L_incorrect_type \n "
                "mov rcx, [rbp + 8*5] \n"
                "mov rcx, [rcx] \n"
                "mov rbx, rcx \n"
                "TYPE rbx \n"
                "cmp rbx, T_INTEGER \n"
                "jne L_incorrect_type \n "
                
                "DATA rcx \n"
                "VECTOR_ELEMENTS rax \n"
                "mov rax, [rax + rcx*8] \n"
                ;"VECTOR_REF rdx, rax, rcx \n"
                
                "leave \n"
                "ret \n"
                
                "L_make_vector_ref: \n"
                "mov rax, [malloc_pointer] \n"
                "my_malloc 16 \n"
                "MAKE_LITERAL_CLOSURE rax, L_const2, L_vector_ref \n"
                "mov rax, [rax] \n"
                "mov [L_glob" (number->string address) "], rax \n\n" ))))
				
(define ass-set-car!
    (lambda ()      
        (let* 
            ((address (find-address 'set-car! global-var-table)))
            (string-append 
                "jmp L_make_set_car \n" 
                "L_set_car: \n"
                "push rbp \n"
                "mov rbp, rsp \n"
                "mov rbx, [rbp + 8*3] \n"
                "cmp rbx , 2 \n"
                "jne L_incorrect_num_of_args \n"
                "mov rax, [rbp + 8*4] \n"
				"mov rcx, [rbp + 8*5] \n"
				"mov rcx, [rcx] \n"
				"mov rax, [rax] \n"
				"mov rbx, rax \n"
				"TYPE rbx \n"
                "cmp rbx, T_PAIR \n"
                "jne L_incorrect_type \n "
				"MY_CAR rax \n"
				"mov qword [rax], rcx \n"
				"mov rax, L_const1 \n"
                "leave \n"
                "ret \n"
                
                "L_make_set_car: \n"
                "mov rax, [malloc_pointer] \n"
                "my_malloc 16 \n"
                "MAKE_LITERAL_CLOSURE rax, L_const2, L_set_car \n"
                "mov rax, [rax] \n"
                "mov [L_glob" (number->string address) "], rax \n\n" ))))
				
(define ass-set-cdr!
    (lambda ()      
        (let* 
            ((address (find-address 'set-cdr! global-var-table)))
            (string-append 
                "jmp L_make_set_cdr \n" 
                "L_set_cdr: \n"
                "push rbp \n"
                "mov rbp, rsp \n"
                "mov rbx, [rbp + 8*3] \n"
                "cmp rbx , 2 \n"
                "jne L_incorrect_num_of_args \n"
                "mov rax, [rbp + 8*4] \n"
				"mov rcx, [rbp + 8*5] \n"
				"mov rcx, [rcx] \n"
				"mov rax, [rax] \n"
				"mov rbx, rax \n"
				"TYPE rbx \n"
                "cmp rbx, T_PAIR \n"
                "jne L_incorrect_type \n "
				"MY_CDR rax \n"
				"mov qword [rax], rcx \n"
				"mov rax, L_const1 \n"
                "leave \n"
                "ret \n"
                
                "L_make_set_cdr: \n"
                "mov rax, [malloc_pointer] \n"
                "my_malloc 16 \n"
                "MAKE_LITERAL_CLOSURE rax, L_const2, L_set_cdr \n"
                "mov rax, [rax] \n"
                "mov [L_glob" (number->string address) "], rax \n\n" ))))
                
(define ass-remainder
    (lambda ()      
        (let* 
            ((address (find-address 'remainder global-var-table)))
            (string-append 
                "jmp L_make_remainder \n" 
                "L_remainder: \n"
                "push rbp \n"
                "mov rbp, rsp \n"
                "mov rbx, [rbp + 8*3] \n"
                "cmp rbx , 2 \n"
                "jne L_incorrect_num_of_args \n"
                "mov rax, [rbp + 8*4] \n"
                "mov rax, [rax] \n"
                "mov rbx, rax \n"
                "TYPE rbx \n"
                "cmp rbx, T_INTEGER \n"
                "jne L_incorrect_type \n "
                "mov rcx, [rbp + 8*5] \n"
                "mov rcx, [rcx] \n"
                "mov rbx, rcx \n"
                "TYPE rbx \n"
                "cmp rbx, T_INTEGER \n"
                "jne L_incorrect_type \n "
                
                "mov rdx, 0 \n"
                "DATA rax \n"
                "DATA rcx \n"
                "cmp rax, 0 \n"
                "jge L_CONT \n"
                "mov r8, rax \n"
                "sar rax, 31      ; -1 or 0 (sign of rax) \n"
                "xor r8, rax \n"
                "sub r8, rax \n"
                "mov rax, r8 \n"
                
                "idiv rcx \n"
                "mov r8, rdx \n"
                "mov rdx, 0 \n"
                "sub rdx, r8 \n"
                "jmp L_CONT2 \n"
                "L_CONT: \n"
                "idiv rcx \n"
                "L_CONT2: \n"
                "mov rax, [malloc_pointer] \n"
                "my_malloc 8 \n"
                "mov qword [rax],  rdx \n"
                "shl qword [rax], 4 \n"
                "or qword [rax], T_INTEGER \n"
                "leave \n"
                "ret \n"
                
                "L_make_remainder: \n"
                "mov rax, [malloc_pointer] \n"
                "my_malloc 16 \n"
                "MAKE_LITERAL_CLOSURE rax, L_const2, L_remainder \n"
                "mov rax, [rax] \n"
                "mov [L_glob" (number->string address) "], rax \n\n" ))))
                
(define ass-smaller-then-bin
    (lambda ()      
        (let* 
            ((address (find-address 'smaller-then-bin global-var-table)))
            (string-append 
                "jmp L_make_smaller_then_bin \n" 
                "L_smaller_then_bin: \n"
                "push rbp \n"
                "mov rbp, rsp \n"
                "mov rbx, [rbp + 8*3] \n"
                "cmp rbx , 2 \n"
                "jne L_incorrect_num_of_args \n"
                "mov rax, [rbp + 8*4] \n"
                "mov rax, [rax] \n"
                "mov rbx, rax \n"
                "TYPE rbx \n"
                "cmp rbx, T_INTEGER \n"
                "je L_make_frac1 \n "
                
                "cmp rbx, T_FRACTION \n"
                "jne L_incorrect_type \n "
                "mov rbx, rax \n"
                "CAR rax \n"
                "mov r8, rax \n"
                "CDR rbx \n"
                "mov r9, rbx \n"
                "jmp L_next_arg1 \n "
                
                "L_make_frac1: \n"
                "DATA rax \n"
                "int_to_frac rax, r8, r9 \n"
                
                "L_next_arg1: \n"
                
                ";At this point the first argument is stored as fraction in r8, r9 \n"
                "mov rcx, [rbp + 8*5] \n"
                "mov rcx, [rcx] \n"
                "mov rbx, rcx \n"
                "TYPE rbx \n"
                "cmp rbx, T_INTEGER \n"
                "je L_make_frac12 \n "
                
                "cmp rbx, T_FRACTION \n"
                "jne L_incorrect_type \n "
                "mov rbx, rcx \n"
                "CAR rcx \n"
                "mov r10, rcx \n"
                "CDR rbx \n"
                "mov r11, rbx \n"
                "jmp L_start_smaller_then_bin \n "
                
                "L_make_frac12: \n"
                "DATA rcx \n"
                "int_to_frac rcx, r10, r11 \n"
               
                "L_start_smaller_then_bin: \n"
                ";At this point the first argument is stored as fraction in r8, r9 \n"
                ";At this point the second argument is stored as fraction in r10, r11 \n"
                
                "mov rax, r8 \n"
                "imul r11 \n"
                "mov r13, rdx \n"
                "mov r14, rax \n"
                
                "mov rax, r9 \n"
                "imul r10 \n"
                "mov rsi, rdx \n"
                "mov rdi, rax \n"
                
                "cmp r13, rsi \n"
                "jg L_smaller_then_bin_false \n"
                "cmp r13, rsi \n"
                "jl L_smaller_then_bin_true \n"
                
                "cmp r14, rdi \n"
                "jge L_smaller_then_bin_false \n"
                "cmp r14, rdi \n"
                "jl L_smaller_then_bin_true \n"
      
                "L_smaller_then_bin_false: \n"
                "mov rax, L_const5 \n"
                "jmp L_end_smaller_then_bin \n"
                
                "L_smaller_then_bin_true \n"
                "mov rax, L_const3 \n"
                "jmp L_end_smaller_then_bin \n"
             
        
                "L_end_smaller_then_bin: \n"
                "leave \n"
                "ret \n"
                
                "L_make_smaller_then_bin \n"
                "mov rax, [malloc_pointer] \n"
                "my_malloc 16 \n"
                "MAKE_LITERAL_CLOSURE rax, L_const2, L_smaller_then_bin \n"
                "mov rax, [rax] \n"
                "mov [L_glob" (number->string address) "], rax \n\n" ))))