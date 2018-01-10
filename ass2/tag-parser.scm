(load "qq.scm")


(define parse
    (lambda (sexp)
        (or
            (parseConst sexp)
            (parseVar sexp)
            (parseIf sexp)
            (parseOr sexp)
            (parseLambda sexp)
            (parseDefine sexp)
            (parseAssignment sexp)
            (parseApplic sexp)
            (parseBegin sexp)
            (parseLet sexp)
            (parseLet* sexp)
            (parseLetrec sexp)
            (parseQuasiquote sexp)
            (parseAnd sexp)
            (parseCond sexp)
            )))


            
(define parseConst
    (lambda (sexp)
        (parser evalConst sexp 'const)))


(define parseVar
    (lambda (sexp)
       (parser evalVar sexp 'var)))
    

(define parseIf
    (lambda (sexp)
      (parser evalIf sexp 'if3)))
        

(define parseOr
    (lambda (sexp)
        (if (and (list? sexp) (< (length sexp) 3))
            (cadr (evalOr sexp))
            (parser evalOr sexp 'or))))
            

(define parseLambda
    (lambda (sexp)
        (or
            (parser eval-variadic-lambda sexp 'lambda-opt)
            (parser eval-simple-lambda sexp 'lambda-simple)
            (parser eval-optional-args sexp 'lambda-opt))))
            

(define parseDefine
    (lambda (sexp)
        (parser evalDefine sexp 'define)))
		; (parser evalDefine sexp 'def)))
        

(define parseAssignment
    (lambda (sexp)
        (parser evalAssignment sexp 'set)))
        

(define parseApplic
    (lambda (sexp)
        (parser evalApplic sexp 'applic)))
            

(define parseBegin
    (lambda (sexp)
        (if (and (list? sexp) (> 3 (length sexp)))
            (cadr (evalBegin sexp))
            (parser evalBegin sexp 'seq))))


(define parseLet
    (lambda (sexp)
            (cadr (evalLet sexp))))


(define parseLet*
    (lambda (sexp)
            (cadr (evalLet* sexp))))


(define parseLetrec
    (lambda (sexp)
            (cadr (evalLetrec sexp))))
            

(define parseQuasiquote
    (lambda (sexp)
            (cadr (evalQuasiquote sexp))))


(define parseAnd
    (lambda (sexp)
        (if (list? sexp) (cadr (evalAnd sexp)) #f)))
        

(define parseCond
    (lambda (sexp)
        (if (list? sexp) (cadr (evalCond sexp)) #f)))
        

(define flatten 
    (lambda (x)
        (cond ((null? x) '())
                ((pair? x) (append (flatten (car x)) (flatten (cdr x))))
                (else (list x)))))


(define make-check-double
    (lambda (lst)
        (lambda (sym)
            (let* ((match (member sym lst))
                    (rest (if match
                                (cdr match)
                                #f)))
                  (if (or (not rest) (not (member sym rest)))
                    #f
                    #t)))))


(define check-double-in-list
    (lambda (lst)
        (member #t (map (make-check-double lst) lst))))

                
(define parser
   (lambda (evaluator sexp tag) 
     (let* ((lst (evaluator sexp))
            (bool (car lst))
            (val (cdr lst)))
            (if bool `(,tag ,@val) #f)))) 
            

(define *reserved-words*
    '(and begin cond define do else if lambda
    let let* letrec or quasiquote unquote
    unquote-splicing quote set!))
    

(define reserved-word?
    (lambda (word)
        (ormap (lambda (res-word) (equal? word res-word))
                *reserved-words*)))


(define evalConst
    (lambda (c)
        (cond 
            ((quote? c) `(#t ,(cadr c)))
            ((const? c) `(#t ,c))
            ((null? c) `(#t ,c))
            ((vector? c) `(#t ,c))
            ((eq? (void) c) `(#t ,c))
            (else '(#f #f)))))


(define evalVar
    (lambda (v)
        (if 
            (and (not (reserved-word? v)) (symbol? v)) 
            `(#t ,v) 
            '(#f #f))))
        

(define evalIf
    (lambda (s)
        (if (not (and (list? s) (equal? (car s) 'if)))
            '(#f #f)
            (let* 
                ((test (cadr s))
                (dit (caddr s))
                (dif (cdddr s))
                (ret-dif (if (null? dif) (void) (car dif))))
                 `(#t ,(parse test) ,(parse dit) ,(parse ret-dif))))))
                

(define evalOr
    (lambda (s)
        (if (not (and (list? s) (equal? (car s) 'or)))
            '(#f #f)
            (let* 
                ((args (cdr s)))
                (cond 
                    ((= 0 (length args)) `(#t ,(parse #f)))
                    ((= 1 (length args)) `(#t ,(parse (car args))))
                    (else `(#t ,(map parse (cdr s)))))))))
                

(define eval-variadic-lambda
    (lambda (s)
       (if (not (and (list? s) (equal? (car s) 'lambda)))
            '(#f #f)
            (let
                ((args (cadr s))
                (exps (cddr s)))
                (if 
                    (not (symbol? args))
                    '(#f #f)
                    `(#t () ,args ,(parse `(begin ,@exps))))))))


(define eval-simple-lambda
    (lambda (s)
       (if (not (and (list? s) (equal? (car s) 'lambda)))
            '(#f #f)
            (if (not (list? (cadr s)))
                '(#f #f)
                (let*
                    ((args (cadr s))
                    (exps (cddr s))
                    (multi-paramed (if (null? args)
                                        #f
                                        (check-double-in-list args))))
                    (if multi-paramed
                        (error 'parser "Invalid params: " args)
                        `(#t ,args ,(parse `(begin ,@exps)))))))))
                        

(define eval-optional-args
     (lambda (s)
       (if (not (and (list? s) (equal? (car s) 'lambda)))
            '(#f #f)
            (if (not (pair? (cadr s))) 
                '(#f #f)
                (let*
                    ((args (cadr s))
                    (exps (cddr s))
                    (reversed-list (reverse (flatten args)))
                    (rest (car reversed-list))
                    (proper-args (reverse (cdr reversed-list))))
                    (if (null? proper-args) 
                        '(#f #f)
                        `(#t ,proper-args ,rest ,(parse `(begin ,@exps)))))))))
                        

(define evalDefine
    (lambda (s)
        (if (not (and (list? s) (equal? (car s) 'define)))
        '(#f #f)
        (let*
            ((name (cadr s))
            (exps (cddr s)))
            (cond 
                ((symbol? name) `(#t (var ,name) ,(parse `(begin ,@exps))))
                ((and (pair? name) (not (null? name))) `(#t (var ,(car name)) ,(parse `(lambda ,(cdr name) ,@exps))))
                (else '(#f #f))
                )))))
                

(define evalAssignment
    (lambda (s)
        (if (not (and (list? s) (equal? (car s) 'set!)))
            '(#f #f)
            (let*
                ((name (cadr s))
                (exps (cddr s)))
                (if (symbol? name) 
                    `(#t (var ,name) ,(parse (car exps)))
                    '(#f #f))))))                
                

(define evalApplic
    (lambda (s)
        (if (or (not (list? s)) (reserved-word? (car s)))
            '(#f #f)
            (let*
                ((first (car s))
                (rest (cdr s))
                (parsed-rest (if (null? rest) 
                                '() 
                                (map parse rest))))
                `(#t ,(parse first) ,parsed-rest)))))


(define make-non-empty-sexp-pred
    (lambda (tag)
        (lambda (exp)
            (and
                (list? exp)                 ;is list
                (not (null? exp))           ;not empty list
                (equal? (car exp) tag)))))  ;its tag is value(tag)


(define non-empty-seq? (make-non-empty-sexp-pred 'seq))


(define helpFunc1
	(lambda (currList exp)
        (let* ((parsed-exp (parse exp))
                (ret (if (non-empty-seq? parsed-exp);if not (seq '())
                            (cadr parsed-exp)       ;((var x) (var y))
                            (list parsed-exp))))    ;((var z))
                    (append currList ret))))
			
			
(define evalBegin
    (lambda (s)
        (if (not (and (list? s) (equal? (car s) 'begin)))
            '(#f #f)
            (if (null? (cdr s))
                `(#t ,(parse (void)))
                (let*
                    ((first (cadr s))
                    (rest (cddr s))
                    (parsed-rest (if (null? rest) 
                                    (parse first) 
									(fold-left helpFunc1 '() (cdr s)))))
                    `(#t ,parsed-rest))))))


(define evalLet
    (lambda (s)
       (if (not (and (list? s) (equal? (car s) 'let)))
            '(#f #f)
            (let*
                ((bindings (cadr s))
                (body (cddr s))
                (params (map car bindings))
                (values (map cadr bindings)))
                `(#t ,(parse `((lambda ,params ,@body) ,@values)))))))
                
                
(define evalLet*
    (lambda (s)
       (if (not (and (list? s) (equal? (car s) 'let*)))
            '(#f #f)
            (let*
                ((bindings (cadr s))
                (body (cddr s)))
                (cond
                    ((null? bindings)
                        `(#t ,(parse `(let () (begin ,@body)))))
                    ((= 1 (length bindings))
                        `(#t ,(parse `(let (,(car bindings)) (begin ,@body)))))
                    (else 
                        `(#t ,(parse `(let (,(car bindings)) (let* ,(cdr bindings) ,@body))))))))))

            
(define evalQuasiquote
    (lambda (s)
       (if (not (and (list? s) (equal? (car s) 'quasiquote)))
            '(#f #f)
            `(#t ,(parse (expand-qq (cadr s)))))))


(define evalAnd
    (lambda (s)
        (if (not (and (list? s) (equal? (car s) 'and)))
            '(#f #f)
            (let* 
                ((args (cdr s)))
                (cond 
                    ((= 0 (length args)) `(#t ,(parse #t)))
                    ((= 1 (length args)) `(#t ,(parse (car args))))
                    (else 
                    `(#t ,(parse `(if ,(cadr s) (and ,@(cdr args)) #f)))))))))              
                

(define evalCond
    (lambda (s)
        (if (not (and (list? s) (equal? (car s) 'cond)))
            '(#f #f)
            (let* 
                ((args (cdr s))
                (test (caar args))
                (dit (cdar args)))
                (if 
                    (= 1 (length args))
                    (if (not (eq? test 'else))
                        `(#t ,(parse `(if ,test (begin ,@dit) ,(void))))
                        `(#t ,(parse `(begin ,@dit))))
                    `(#t ,(parse `(if ,test (begin ,@dit) (cond ,@(cdr args))))))))))           
                     
                
(define evalLetrec
    (lambda (s)
       (if (not (and (list? s) (equal? (car s) 'letrec)))
            '(#f #f)
            (let*
                ((bindings (cadr s))
                (params (map car bindings))
                (new-bindings (map (lambda (p) (cons p (list #f))) params))
                (set!s (map (lambda (bind) (cons 'set! bind)) bindings))
                (body (cddr s)))
                `(#t ,(parse `(let ,new-bindings ,@(append set!s (list (list `(lambda () ,@body)))))))))))
                
                
; (define parse-2 parse)