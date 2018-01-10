(load "ass1/sexpr-parser.scm")
(load "ass2/tag-parser.scm")
(load "ass3/semantic-analyzer.scm")

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
                        (extract-consts (cdr lst-sexp)))))
        ))
        
(define remove-dups
    (lambda (consts-list)
        (letrec ((inner-func (lambda (consts-list)
                                (let ((rvs (reverse consts-list)))
                                    (cond ((null? rvs) '()) 
                                            ((member (car rvs) (cdr rvs)) (inner-func (cdr rvs)))
                                            (else (cons (car rvs) (inner-func (cdr rvs)))))))))
                (reverse (inner-func consts-list)))))

;; (define add-to-consts-table
;;     (lambda (consts-list)
;;     
;;     ))

(define is-member 
    (lambda (arg acc)
        (ormap (lambda (x) (equal? arg (cadr x))) acc)))

(define compile-scheme-file 
    (lambda (src-file trg-file)
        (set! consts-table '())
        
        (let* ((lst-sexp (pipeline (file->list src-file))) 
                        ;(consts (fold-left append '() (map help (extract-consts lst-sexp)))))

                (consts (remove-dups (fold-left append '() (map help (extract-consts lst-sexp))))))
                ;(add-to-consts-table consts)
                consts)
                ;(find-consts lst-sexp))
        ))

(define find-consts
    (lambda (lst)
    
        (cond ((or (null? lst) (not (pair? lst))) '())
        
              ((equal? (car lst) 'const) 
                (cond ((list? (cadr lst)) (find-consts-rec (cadr lst)))
                
                      ((pair? (cadr lst)) (find-consts-rec (cadr lst)))
                
                      ((vector? (cadr lst)) (find-consts-rec (cadr lst)))
                
                      ((symbol? (cadr lst)) (list (symbol->string (cadr lst)) (cadr lst)))
                      
                      ;((char? (cadr lst) )
                
                      (else (list (cadr lst)))))
                    
            (else `(,@(find-consts (car lst)) ,@(find-consts (cdr lst)))))))
                
                
                
(define find-consts-rec
    (lambda (lst)
    
        (cond ((null? lst) lst)
        
              ((list? lst) `(,@(find-consts-rec (car lst)) ,@(find-consts-rec (cdr lst)) ,lst))
                
              ((pair? lst) `(,@(find-consts-rec (car lst)) ,@(find-consts-rec (cdr lst)) ,lst))
                
              ((vector? lst) `(,@(apply append (map (lambda (x) (find-consts-rec x)) (vector->list lst))) ,lst))
                
              ((symbol? lst) (list (symbol->string lst) lst))
                
              (else (list lst)))))