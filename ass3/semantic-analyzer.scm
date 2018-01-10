(define remove-applic-lambda-nil
    (lambda (parseExp)
        (cond ((or (null? parseExp) (not (list? parseExp))) parseExp)
            ((and (equal? (car parseExp) 'applic)
                (equal? (caadr parseExp) 'lambda-simple)
                (equal? (cadadr parseExp) '())
                (null? (caddr parseExp)))
                (remove-applic-lambda-nil (car (cddadr parseExp))))
                (else (map remove-applic-lambda-nil parseExp)))))

(define inner-is-bound
    (lambda (var exp counter)
        (cond ((or (null? exp) (not (pair? exp))) #f)
            ((and 
                (equal? exp (list 'var var))
                (> counter 0)) #t)
            ((equal? (car exp) 'lambda-simple)
                (if (member var (cadr exp)) #f
                    (ormap (lambda (x) (inner-is-bound var x (+ 1 counter))) (cddr exp))))
            ((equal? (car exp) 'lambda-opt)
                (if (or (member var (cadr exp)) (equal? var (caddr exp))) 
                    #f
                    (ormap (lambda (x) (inner-is-bound var x (+ 1 counter))) (cddr exp))))       
            (else (ormap (lambda (x) (inner-is-bound var x counter)) exp)))))
        
(define is-bound?
    (lambda (var exp)
        (inner-is-bound var exp 0)))
            
(define is-set?
    (lambda (var exp)
      (cond ((or (null? exp) (not (pair? exp))) #f)
            ((and 
                (equal? (car exp) 'lambda-opt) 
                (or (equal? var (caddr exp)) (member var (cadr exp))) #f))
            ((and 
                (equal? (car exp) 'lambda-simple) 
                (member var (cadr exp))) #f)
            ((and
                (equal? (car exp) 'set)
                (equal? (cadadr exp) var)) #t)
            (else (ormap (lambda (x) (is-set? var x)) exp)))))
        
(define is-get?
    (lambda (var exp)
        (cond ((or (null? exp) (not (pair? exp))) #f)
              ((equal? exp (list 'var var)) #t) 
              ((equal? (car exp) 'set) (is-get? var (cddr exp)))
              ((and (equal? (car exp) 'lambda-opt) 
                    (or (equal? var (caddr exp))
                        (member var (cadr exp))) #f))
              ((and (equal? (car exp) 'lambda-simple) (member var (cadr exp))) #f)
              (else (ormap (lambda (x) (is-get? var x)) exp))))) 

(define to-box?
    (lambda (var exp)
        (and 
            (is-bound? var exp)
            (is-set? var exp) 
            (is-get? var exp))))
            
(define seq?
    (lambda (exp)
        (and
            (list? exp)
            (not (null? exp))
            (equal? (car exp) 'seq))))
            
(define clean-seq
    (lambda (exp)
         (if (seq? exp) (cadr exp) (list exp))))

(define clean-all-after-seq
    (lambda (exp)
        (car (map clean-seq exp))))
        
(define boxing
    (lambda (var exp)
        (if (to-box? var exp)
            `((seq ((set (var ,var) (box (var ,var)))
            ,@(clean-all-after-seq (to-box-set var (to-box-get var exp))))))
            exp)))
            
(define to-box-get 
  (lambda (var exp)
        (cond ((or (null? exp) (not (pair? exp))) exp)
              ((equal? exp (list 'var var)) `(box-get (var ,var))) 
              ((equal? (car exp) 'set) `(set ,(cadr exp) ,@(to-box-get var (cddr exp))))
              ((and (equal? (car exp) 'lambda-opt) 
                    (or (equal? var (caddr exp))
                        (member var (cadr exp))) exp))
              ((and (equal? (car exp) 'lambda-simple) (member var (cadr exp))) exp)
              (else (map (lambda (x) (to-box-get var x)) exp))))) 

(define to-box-set
    (lambda (var exp)
      (cond ((or (null? exp) (not (pair? exp))) exp)
            ((and (equal? (car exp) 'lambda-opt) 
                (or (equal? var (caddr exp))
                    (member var (cadr exp))) exp))
            ((and (equal? (car exp) 'lambda-simple) (member var (cadr exp))) exp)
            ((and
                (equal? (car exp) 'set)
                (equal? (cadadr exp) var)) `(box-set ,(cadr exp) ,(to-box-set var (caddr exp))))
            (else (map (lambda (x) (to-box-set var x)) exp)))))
        
(define lambda-simple-boxer
    (lambda (args body)
        (if (null? args) 
            body
            (lambda-simple-boxer (cdr args) (boxing (car args) body)))))
        
(define box-set
    (lambda (parseExp)
        (cond 
            ((or (null? parseExp) (not (list? parseExp))) parseExp)
            ((equal? (car parseExp) 'lambda-simple)
                `(lambda-simple ,(cadr parseExp) ,@(box-set (lambda-simple-boxer (reverse (cadr parseExp)) (cddr parseExp)))))
            ((equal? (car parseExp) 'lambda-opt)
                `(lambda-opt ,(cadr parseExp) ,(caddr parseExp) ,@(box-set (lambda-simple-boxer (append (cadr parseExp) (cddr parseExp)) (cdddr parseExp)))))
            (else 
                (map box-set parseExp)))))

;; empty list or not pair (const, symbol,...)
(define atomic-exp?
    (lambda (exp)
        (or
            (null? exp)
            (not (list? exp))
        )))

(define lambda?
    (lambda (exp)
        (or (lambda-simple? exp) (lambda-opt? exp))))

(define lambda-simple?
    (lambda (exp)
        (and
            (list? exp)
            (not (null? exp))
            (equal? (car exp) 'lambda-simple))))
        
(define lambda-opt?
     (lambda (exp)
        (and
            (list? exp)
            (not (null? exp))
            (equal? (car exp) 'lambda-opt))))
            
(define inc-bound-in-tapple
    (lambda (tapple)
        (let ((var (car tapple))
              (bound-index (cadr tapple))
              (param-index (caddr tapple)))
        `(,var ,(+ 1 bound-index) ,param-index))))
            
(define update-bound-counter
    (lambda (scope)
        (map inc-bound-in-tapple scope)))
            
(define make-scope-tapple
    (lambda (param p-index)
        (list param -1 p-index)))
        
        
(define params->scope-tapples
    (lambda (params counter)
        (if (null? params)
            '()
            (cons (make-scope-tapple (car params) counter)
                  (params->scope-tapples (cdr params) (+ 1 counter))))))

(define var-token?
    (lambda (exp)
        (and
            (list? exp)
            (not (null? exp))
            (equal? (car exp) 'var))
        ))
        
(define element-index
    (lambda (element lst)
      (if (equal? (list? (memv element lst)) #t)
          (- (length lst) (length (memv element lst)))
          #f
          )
      ))

(define find-var-tapple-in-scope
    (lambda (var scope)
        (cond
            ((null? scope) #f)
            ((equal? (caar scope) var) (car scope))
            (else
                (find-var-tapple-in-scope var (cdr scope))))))
                  
(define inner-pe->lex-pe
  (lambda (exp scope params)
    (cond 
        ((atomic-exp? exp)
            exp)
        ((lambda? exp)
            (let* ((updated-scope (update-bound-counter scope))
                  (args (if (lambda-opt? exp)
                            (append (cadr exp) (list (caddr exp)))
                            (cadr exp)))
                  (updated-scope (append (params->scope-tapples args 0)
                                         updated-scope)))
                 (map (lambda (e) (inner-pe->lex-pe e updated-scope args)) exp)))
        ((var-token? exp)
            (let* ((var (cadr exp))
                    (index (element-index var params))
                    (tapple (find-var-tapple-in-scope var scope)))
                    (cond
                        (index `(pvar ,var ,index))
                        (tapple `(bvar ,@tapple))
                        (else `(fvar ,var)))))
        (else 
            (map (lambda (e) (inner-pe->lex-pe e scope params)) exp)))))
                    
                
(define pe->lex-pe
    (lambda (parseExp)
        (inner-pe->lex-pe parseExp '() '())))

(define annotate-tc
    (lambda (parseExp) 
        (annotate parseExp #f)))

(define annotate 
    (lambda (exp tp?)
        (cond
            ((atomic-exp? exp) exp)
            ((or 
                (equal? (car exp) 'const) 
                (equal? (car exp) 'var)) exp)
            ((or 
                (equal? (car exp) 'or)
                (equal? (car exp) 'seq))
                    (let* ((tag (if (equal? (car exp) 'or) 'or 'seq)) 
                        (exps (cadr exp))
                        (rev-lst (reverse exps))
                        (last-element (car rev-lst))
                        (new-lst (reverse (cdr rev-lst))))
                        `(,tag (,@(map (lambda (child) (annotate child #f)) new-lst) ,(annotate last-element tp?)))))
            ((equal? (car exp) 'if3)
                (let* ((test (cadr exp))
                    (dit (caddr exp))
                    (dif (cadddr exp)))
                    `(if3 ,(annotate test #f) ,(annotate dit tp?) ,(annotate dif tp?))))
            ((equal? (car exp) 'define)
                (let* ((var (cadr exp))
                    (rest (caddr exp)))
                    `(define ,var ,(annotate rest #f))))
            ((lambda-simple? exp)
                (let* ((args (cadr exp))
                    (body (caddr exp)))
                    `(lambda-simple ,args ,(annotate body #t))))
            ((lambda-opt? exp)
                (let* ((args (cadr exp))
                    (opt-args (caddr exp))
                    (all-args (append args (list opt-args)))
                    (body (cadddr exp)))
                    `(lambda-opt ,args ,opt-args ,(annotate body #t))))
            ((equal? (car exp) 'applic)
                (let* ((app (if tp? 'tc-applic 'applic))
                        (rest (cdr exp)))
                    `(,app ,@(map (lambda (child) (annotate child #f)) rest))))
           ((or 
                (equal? (car exp) 'set)
                (equal? (car exp) 'box-set))
                    (let* ((tag (if (equal? (car exp) 'set) 'set 'box-set))
                        (rest (cdr exp)))
                        `(,tag ,@(annotate rest #f)))) 
                    
            (else (map (lambda (child) (annotate child tp?)) exp)))))   
            
            

(define sem-parse
    (lambda (parsedExp)
        (annotate-tc (pe->lex-pe (box-set (remove-applic-lambda-nil parsedExp))))))
  
  
  
  
  
  