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
        
        
;; input: list of sexprs
;; output: list of all constant values
;; example: ((define (fvar x) (const 1)) (applic (fvar +) ((fvar x) (const 1)))))
;;          ==> (1 1)
(define add-to-constants-table
    (lambda (lst-sexp)
        (cond 
            ((or (null? lst-sexp) (not (pair? lst-sexp))) '())
            ((const-token? lst-sexp) (cdr lst-sexp))
            (else (append (add-to-constants-table (car lst-sexp)) 
                        (add-to-constants-table (cdr lst-sexp)))))
        ))
    
(define compile-scheme-file 
    (lambda (src-file trg-file)
        (let* ((lst-sexp (pipeline (file->list src-file))) 
                (consts (add-to-constants-table lst-sexp)))
                 consts)
        ))
