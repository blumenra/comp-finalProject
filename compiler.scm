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
        
(define basic-table `((0 0 (,T_UNDEFINED 0))
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
            (if (symbol? exp) 
                (cons (symbol->string exp) `(,exp)) 
                `(,exp)))
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
					(add-to-global-var-table rest (append table (list `(,addr ,fvar (-1)))))))))
    ))

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
                            
                    ;(IF FRACTION)...
                    
                    ((equal? type T_STRING) 
                        (string-append 
                            "\t" "MAKE_LITERAL_STRING " (split-lst value ", " number->string) "\n"))
                    ((equal? type T_PAIR)
                        (let 
                            ((label-car (string-append "L_const" (number->string (car value))))
                            (label-cdr (string-append "L_const" (number->string (cadr value)))))
                        (string-append 
                                "\t" "dq MAKE_LITERAL_PAIR(" lable-car ", " label-cdr ")\n")))
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
				"\t" "\n"))))
       
    
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
    
;; input: ??
;; output: string of the head code in assembly
(define make-prologue
    (lambda ()
        (string-append 
            "%include \"scheme.s\"\n\n"
			"L_error: \n\n"
            (initialize-tables-to-asm)
            "global main\n"
            "section .text\n"
            "main:\n\n"
            )))
            
(define make-epilogue
    (lambda ()
        (string-append 
            "ret\n")))
        
(define write-sob-string
    (string-append 
        "push qword rax\n"
        "call write_sob_if_not_void\n"
        "add rsp, 1*8\n"))

(define compile-scheme-file 
    (lambda (src-file trg-file)
        (let* ((lst-sexp (pipeline (file->list src-file))) 
                (consts (remove-dups (fold-left append '() (map help (extract-consts lst-sexp)))))
                (const-table (add-to-consts-table consts basic-table)))
                (set! consts-table const-table)
                (set! global-var-table (add-to-global-var-table (remove-dups (extract-fvars lst-sexp)) '()))
                
                
                (display `(lst-sexp: ,lst-sexp))
;;                 (code-gen (car lst-sexp))
                (newline)
                (display `(constlist: ,consts)) 
                (newline)
                (display `(const-table: ,consts-table))
                ;(initialize-consts-table-to-asm)
				(newline)
				(display `(global-table: ,global-var-table))
				
                (string->file 
                     trg-file 
                     (string-append 
                        (make-prologue)
                         (append-str-list (map (lambda (exp) (code-gen exp '())) lst-sexp)) ; not sure how to handle the env in this line yet
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
        
(define code-gen-const
    (lambda (exp env)
        (let* 
            ((value (cadr exp))
            (address (find-address value consts-table)))
            (string-append 
                "mov rax, " (string-append "[L_const" (number->string address)) "] \n"))))

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
                        "cmp rax, [L_const" (number->string false-address) "]\n"
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
        (let* ((params (caddr exp))
            (num-of-params (length params))
            (proc (cadr exp)))
            (string-append
                "push L_const2 \n" ;push '() to stack
                (params-code-gen params env) ;push evaluated params reversely to stack
                "push " (number->string num-of-params) "\n"
                (code-gen proc env)
                "mov rbx, rax \n"
                "TYPE rbx \n"
                "cmp rbx, T_CLOSURE \n"
                "jne L_error \n"
                "mov rbx, rax \n"
                "CLOSURE_ENV rbx \n"
                "push rbx \n"
                "CLOSURE_CODE rax \n"
                "call rax \n"))))
				
(define code-gen-set
    (lambda (exp env)
        (let ((tag (caadr exp))
            (lst_var (cadr exp))
            (e (caddr exp)))
            (cond 
    ;				((equal? tag 'pvar)
    ;					(let ((minor (caddr lst_var)))
    ;						(string-append
    ;							(code-gen e)
    ;							"mov qword [rbp + (4 + " (number->string minor) ") * 8], rax \n"
    ;							"mov rax, L_const1 \n")))
    ;				((equal? tag 'bvar)
    ;					(let ((major (caddr lst_var))
    ;						(minor (cadddr lst_var)))
    ;						(string-append
    ;							(code-gen e)
    ;							"mov rbx, qword [rbp +  2 * 8] \n"
    ;							"mov rbx, qword [rbx + " (number->string major) " * 8] \n"
    ;							"mov qword [rbx + " (number->string minor) " * 8], rax \n"
    ;							"mov rax, L_const1 \n")))
                ((equal? tag 'fvar)
                    (let* ((value (cadr lst_var))
                        (address (find-address value global-var-table)))
                        (string-append
                            (code-gen e env)
                            "mov rbx, rax \n"
                            "mov rax, L_glob" (number->string address) "\n"
                            "mov [rax], rbx \n"
                            "mov rax, L_const1 \n")))))))


;(define code-gen-box-set
;	(lambda (exp)							
 ;       (let ((tag (caadr exp))
	;		(lst_var (cadr exp))
	;		(e (caddr exp)))
	;		(cond 
	;			((equal? tag 'pvar) 
	;				(string-append
	;					(code-gen e)
	;					"mov qword [rbp + (4 + " (number->string minor) ") * 8], rax \n"
	;					"mov rax, L_const1 \n"))
     ;                      
		;		((equal? tag 'bvar)
		;			(let ((major (caddr lst_var))
		;				(minor (cadddr lst_var)))
		;				(string-append
		;					(code-gen e)
		;					"mov rbx, qword [rbp +  2 * 8] \n"
		;					"mov rbx, qword [rbx + " (number->string major) " * 8] \n"
		;					"mov qword [rbx + " (number->string minor) " * 8], rax \n"
		;					"mov rax, L_const1 \n")))))))
		
(define code-gen-box-get
    (lambda (exp env)
        (string-append
            (code-gen (cadr exp) env)
            "mov [rax], rax \n")))
			
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
                "mov rax, [L_glob" (number->string address) "] \n"))))

(define code-gen-def	
    (lambda (exp env)
        (let* ((value (cadadr exp))
            (address (find-address value global-var-table))
            (e (caddr exp)))
                (string-append
                    (code-gen e env)
                    "mov [L_glob" (number->string address) "], rax \n"
                    "mov rax, L_const1 \n"))))
                    
(define code-gen-lambda-simple
    (lambda (exp env)
				
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
                        ;((equal? tag 'tc-applic) (code-gen-or exp env))))
                        ((equal? tag 'set) (code-gen-set exp env))
                        ;((equal? tag 'box (code-gen-or exp env))))
                        ((equal? tag 'box-get) (code-gen-box-get exp env))
                        ((equal? tag 'box-set) (code-gen-box-set exp env))
                        ((equal? tag 'pvar) (code-gen-pvar exp env))
                        ((equal? tag 'bvar) (code-gen-bvar exp env))
                        ((equal? tag 'fvar) (code-gen-fvar exp env))
                        ((equal? tag 'define) (code-gen-def exp env))))
                        ;((equal? tag 'lambda-simple) (code-gen-or exp env))))
                        ;((equal? tag 'lambda-opt) (code-gen-or exp env))))
						
       ))))
       
       
       
       
