(load "pc.scm")

(define wrap-parser-with-skips
    (lambda (skiper parser)
        (new
            (*parser skiper) *star
            (*parser parser)
            (*parser skiper) *star
            (*caten 3)
            (*pack-with
                (lambda (skip-l sexp skip-r)
                    sexp))
            done)))
        
;;;;;;;;;;;;;;;;;;; <SEXPR>
(define <sexpr>
  (new
  
    (*delayed (lambda () (wrap-parser-with-skips <PrefixSkip> <Boolean>)))
    (*delayed (lambda () (wrap-parser-with-skips <PrefixSkip> <Char>)))
    (*delayed (lambda () (wrap-parser-with-skips <PrefixSkip> <Number>)))
    (*delayed (lambda () (wrap-parser-with-skips <PrefixSkip> <String>)))
    (*delayed (lambda () (wrap-parser-with-skips <PrefixSkip> <Symbol>)))
    (*delayed (lambda () (wrap-parser-with-skips <PrefixSkip> <ProperList>)))
    (*delayed (lambda () (wrap-parser-with-skips <PrefixSkip> <ImproperList>)))
    (*delayed (lambda () (wrap-parser-with-skips <PrefixSkip> <Vector>)))
    (*delayed (lambda () (wrap-parser-with-skips <PrefixSkip> <Quoted>)))
    (*delayed (lambda () (wrap-parser-with-skips <PrefixSkip> <QuasiQuoted>)))
    (*delayed (lambda () (wrap-parser-with-skips <PrefixSkip> <UnquoteAndSpliced>)))
    (*delayed (lambda () (wrap-parser-with-skips <PrefixSkip> <Unquoted>)))
    (*delayed (lambda () (wrap-parser-with-skips <PrefixSkip> <CBName>)))
    (*delayed (lambda () (wrap-parser-with-skips <PrefixSkip> <InfixExtension>)))
    (*disj 14)
  
  done))
;;;;;;;;;;;;;;;;;;;; <SEXPR>

;;;;;;;;;;;;;;;;;;;; <INF-EXPR>
(define <InfixExpression>
  (new
    (*delayed (lambda () <layer-1>))
    done))
;;;;;;;;;;;;;;;;;;;; <INF-EXPR>


(define <endOfLine>
    (new
        (*parser (char #\newline))
        done))
     
(define <endOfFile> <end-of-input>)
        
(define <whitespace> (range #\nul #\space))

(define ^<comments>
  (lambda (exp-parser)
    (new 
        (*parser (char #\;))
        (*parser <any-char>)
        (*parser <endOfLine>)
        (*parser <endOfFile>)
        (*disj 2)
        *diff 
        *star
        
        (*parser <endOfLine>)
        (*parser <endOfFile>)
        (*disj 2)
        (*caten 3)
        
        
        (*parser (word "#;"))
        (*delayed (lambda () exp-parser))
        (*caten 2)
        
        (*disj 2)
    done)))


(define <PrefixComments> (^<comments> <sexpr>))


(define <InfixComments> (^<comments> <InfixExpression>))

        
(define <PrefixSkip>
    (new
        (*parser <whitespace>)
        (*parser <PrefixComments>)
        (*disj 2)
        done))
        
(define <InfixSkip>
    (new
        (*parser <whitespace>)
        (*parser <InfixComments>)
        (*disj 2)
        done))


    
(define <Boolean>
    (new
        (*parser (word-ci "#t"))
    	(*parser (word-ci "#f"))
    	(*disj 2)
    	(*pack (lambda (list)
            (let ((l (char-downcase (cadr list))))
                (cond
                    ((char=? l #\t) #t)
                    ((char=? l #\f) #f)
                    (else (display "The value is no valid!"))))))

    	done))


(define <CharPrefix>
    (new
    	(*parser (word "#\\"))
    	(*pack 
            (lambda (_) _))
    	done))


(define <VisibleSimpleChar> (range #\! #\delete))


(define <digit-0-9> (range #\0 #\9))


(define <letter-a-f> (range-ci #\a #\f))


(define <letter-a-z> (range-ci #\a #\z))


(define <HexChar>
  (new
   (*parser <digit-0-9>)
   (*parser <letter-a-f>)
   (*disj 2)
   done))

   

;(define <HexUnicodeChar>
 ;   (new 
  ;  (*parser (char-ci #\x))
   ; (*parser <HexChar>)
    ;*plus
    ;(*caten 2)
    ;(*pack-with
    ;  (lambda (prefix hexNum)
     ;   hexNum))
    ;done))

(define <HexUnicodeChar>
    (new 
        (*parser (char-ci #\x))
        (*parser <HexChar>)
        *plus
        (*caten 2)
        (*pack-with 
            (lambda (ch hexch)
                (integer->char (string->number (list->string hexch) 16))))
       done))
    
(define ^<NamedChar>
    (lambda (str ch)
    (new (*parser (word-ci str)) 
        (*pack (lambda (_) ch))
    done)))

    
(define <NamedChar>
    (new
       (*parser (^<NamedChar> "space" #\space))
       (*parser (^<NamedChar> "nul" #\nul))
       (*parser (^<NamedChar> "newline" #\newline))
       (*parser (^<NamedChar> "return" #\return))
       (*parser (^<NamedChar> "tab" #\tab))
       (*parser (^<NamedChar> "page" #\page)) 
       (*parser (^<NamedChar> "lambda" (integer->char 955)))
       (*disj 7)
       done))


(define <Char>
    (new
        (*parser <CharPrefix>)
        (*parser <HexUnicodeChar>)
        (*parser <NamedChar>)
        (*parser <VisibleSimpleChar>)
        (*disj 3)
        (*caten 2)
        (*pack (lambda (list)
            (cadr list)))
        
        
        done))
                  
   
(define <Natural>
    (new
        (*parser <digit-0-9>)
        *plus
        (*parser <letter-a-z>)
        ; ;;;;;
        ; (*parser (char #\^))
        ; (*parser (char #\/))
        ; (*disj 3)
        ; ;;;;;
        *not-followed-by
         (*pack
            (lambda (numList)
                (string->number (list->string numList))))
        done))       
        
        
(define <op>
    (new

        (*parser (char #\+))
        (*parser (char #\-))

        (*disj 2)

        (*pack (lambda (op-char) ; transform the output
            (string->symbol (string op-char))))

    done))
        

(define <Integer>
    (new
        (*parser (maybe <op>))
        (*parser <Natural>)
        (*caten 2)
        
        (*pack-with
            (lambda (opList num)
                (let
                    ((opFound (car opList))
                     (operator (cadr opList)))
                     (if opFound
                        (cond
                            ((symbol=? operator '+) num)
                            ((symbol=? operator '-) (* -1 num))
                            (else (display "The value is no valid!")))
                    num))))
                    
        
        done))
        
        
(define <Fraction>
  (new (*parser <Integer>)
       (*parser (char #\/))
       (*parser <Natural>)
       (*caten 3)
       (*pack-with (lambda (integer div natural)
	  (/ integer natural)))
       done))
       
        
(define <Number>
    (new
        (*parser <Fraction>)
        (*parser <Integer>)
        
        (*disj 2)
    done))
    

(define <NotLiteral>
    (new 
        (*parser (char #\\))
        (*parser (char #\"))
        (*disj 2)
        done))


(define <StringLiteralChar>
  (new
    (*parser <any-char>)
    (*parser <NotLiteral>)
    *diff
    done))

    
(define ^<meta-char>
  (lambda (str ch)
    (new (*parser (word-ci str))
	 (*pack (lambda (_) ch))
	 done))) 
   

(define <StringMetaChar>
  (new (*parser (^<meta-char> "\\\\" #\\))
       (*parser (^<meta-char> "\\\"" #\"))
       (*parser (^<meta-char> "\\n" #\newline))
       (*parser (^<meta-char> "\\r" #\return))
       (*parser (^<meta-char> "\\t" #\tab))
       (*parser (^<meta-char> "\\f" #\page))
       (*disj 6)
       done)) 


(define <StringHexChar>
    (new
        (*parser (word-ci "\\x"))
        (*parser <HexChar>)
        *star
        (*parser (char #\;))
        (*caten 3)
        (*pack-with
         (lambda (x hex c) 
           (integer->char (string->number (list->string hex) 16))))
     done))


(define <StringChar>
  (new
   (*parser <StringLiteralChar>)
   (*parser <StringMetaChar>)
   (*parser <StringHexChar>)
   (*disj 3)
   done))

   
(define <String>
    (new
        (*parser (char #\"))
        (*parser <StringChar>)
        *star
        (*parser (char #\"))
        (*caten 3)
        (*pack-with
                (lambda (c1 str c2)
                (list->string str)))
  done))
  

(define <SymbolChar>
  (new
    (*parser <digit-0-9>)
    (*parser <letter-a-z>)
    (*parser (char #\!))
    (*parser (char #\$))
    (*parser (char #\^))
    (*parser (char #\*))
    (*parser (char #\-))
    (*parser (char #\_))
    (*parser (char #\=))
    (*parser (char #\+))
    (*parser (char #\<))
    (*parser (char #\>))
    (*parser (char #\?))
    (*parser (char #\/))
    (*disj 14)

   done))


(define <Symbol>
  (new
   (*parser <SymbolChar>)
   *plus 
   (*pack
    (lambda (symch)
      (string->symbol (string-downcase (list->string symch)))))
   done))
   

(define <sexpr-with-space>
    (new
        (*parser <sexpr>)
        (*parser (char #\space))
        (*caten 2)
        (*pack-with (lambda (sexp s)
            sexp))
        done))


(define <ProperList>
    (new
        (*parser (char #\())
        (*parser <PrefixSkip>) *star
        (*parser (char #\)))
        (*caten 3)
        (*pack (lambda (_) '()))
        
        (*parser (char #\())
        (*parser <sexpr>)
        *plus
        (*parser (char #\)))
        (*caten 3)
        (*pack-with
                (lambda (c1 sexp* c2) sexp*))
        (*disj 2)
                
    done))  


(define <ImproperList>
    (new
        (*parser (char #\())
        (*parser <sexpr>)
        *plus
        (*parser (char #\.))
        (*parser <sexpr>)
        (*parser (char #\)))
        (*caten 5)
        (*pack-with
                (lambda (c1 sexp1 dot sexp2 c2) `(,@sexp1 . ,sexp2)))          
    done))
    

(define <Vector>
    (new
        (*parser (word "#()"))
        (*pack (lambda (_) '#()))
        
        (*parser (char #\#))
        (*parser (char #\())
        (*parser <sexpr>)
        *star
        (*parser (char #\)))
        (*caten 4)
        (*pack-with
                (lambda (hash c1 sexp* c2) (list->vector sexp*)))
        (*disj 2)
                    
    done))  


(define <Quoted>  
    (new
        (*parser (char #\'))
        (*parser <sexpr>)
        (*caten 2)
        (*pack-with (lambda (qu sexp) `',sexp))
    done))
    
    
(define <QuasiQuoted>  
    (new
        (*parser (char #\`))
        (*parser <sexpr>)
        (*caten 2)
        (*pack-with (lambda (qu sexp) (list 'quasiquote `(,@sexp))))
        
    done))
    
    
(define <Unquoted>  
    (new
        (*parser (char #\,))
        (*parser <sexpr>)
        (*caten 2)
        (*pack-with (lambda (qu sexp) (list 'unquote `(,@sexp))))
        
    done))

    
(define <UnquoteAndSpliced>
  (new
    (*parser (word ",@"))
    (*parser <sexpr>)    
    (*caten 2)
    (*pack-with (lambda (unq-shtrudel sexp) (list 'unquote-splicing `(,@sexp))))
    
    done))
    

(define <CBNameSyntax1>
  (new
    (*parser (char #\@))
    (*parser <sexpr>)    
    (*caten 2)
    (*pack-with (lambda (at-mark sexp) `(cbname ,sexp)))
    done))


(define <CBNameSyntax2>
  (new
    (*parser (char #\{))
    (*parser <sexpr>)
    (*parser (char #\}))
    (*caten 3)
    (*pack-with (lambda (left-pret sexp right-pret) `(cbname ,sexp)))
    done))
    

(define <CBName>
  (new
   (*parser <CBNameSyntax1>)
   (*parser <CBNameSyntax2>)
   (*disj 2)
  done))
  

(define <InfixPrefixExtensionPrefix>
  (new
   (*parser (word "##"))
   (*parser (word "#%"))
   (*disj 2)
  done))


(define <infix-Special-Symbol>
  (new
    (*parser (char #\+))
    (*parser (char #\-))
    (*parser (char #\*))
    (*parser (word "**"))
    (*parser (char #\^))
    (*parser (char #\/))
    (*disj 6)
    done))


(define <InfixSymbol>
  (new
    (*parser <SymbolChar>)
    (*parser <infix-Special-Symbol>)
    *diff
    *plus
    (*pack
        (lambda (sym)
            (string->symbol (string-downcase (list->string sym)))))
    done))


(define <InfixSexprEscape>
  (new
    (*parser <InfixPrefixExtensionPrefix>)
    (*parser <sexpr>)
    (*caten 2)
    (*pack-with (lambda (prefix sexp) sexp))
    
    done))


(define <InfixExtension>
  (new
    (*parser <InfixPrefixExtensionPrefix>)
    (*parser <InfixExpression>)
    (*caten 2)
    (*pack (lambda (list)
                    (cadr list)))
    done))


(define func
  (lambda (list1 list2)
    (let
      ((op (car list2))
      (param (cdr list2)))
      (cons op (cons list1 param)))))


(define func2
  (lambda (list1 list2)
    (let
      ((op (car list2))
      (param (cadr list2)))
      (cons op (cons param `(,list1))))))


(define append-left
  (lambda (num lst)
    (fold-left func num lst)))


(define (reverse l) 
   (fold-left (lambda (i j) 
                (cons j i)) 
              '() 
              l)) 


(define append-temp
  (lambda (num lst)
    (let ((reversed-list (reverse (cons (list (caar lst) num) lst)))
            (last-element (car (reverse lst))))
      (fold-left func2 (cadr last-element) (cdr reversed-list)))))


(define <infix-operation-parser>
  (lambda (op-parser upper-layer-exp handler)
    (new
      (*parser upper-layer-exp)
      (*parser op-parser)
      (*parser upper-layer-exp)
      (*caten 2)
      *star

      (*caten 2)
      (*pack-with
        (lambda (num suffix)
          (if (null? suffix) num
          `(,@(handler num suffix)))))
            
    done)))


;; example: #\+ ==> parser of '+' that returns a symbol of '+'
(define ^<charOp->symbol>
  (lambda (op)
    (new
      (*parser (char op))
      (*pack (lambda (op-char) ; transform the output
          (string->symbol (string op-char))))
      done)))


(define <PowerSymbol>
  (new
    (*parser (char #\^))
    (*parser (word "**"))
    (*disj 2)
    (*pack (lambda (_) 'expt))
    
    done))


(define <op-mul> (^<charOp->symbol> #\*))
(define <op-div> (^<charOp->symbol> #\/))
(define <op-add> (^<charOp->symbol> #\+))
(define <op-sub> (^<charOp->symbol> #\-))


(define <layer-1-op>
  (new
    (*parser <op-add>)
    (*parser <op-sub>)
    (*disj 2)
    done))


(define <layer-2-op>
  (new
    (*parser <op-mul>)
    (*parser <op-div>)
    (*disj 2)
    done))


(define <layer-3-op>
  (new
    (*parser <PowerSymbol>)
    done))


(define <InfixNeg>
  (new

    (*parser <op-sub>)
    *maybe
    
    (*parser <InfixSkip>) *star
    (*parser <Number>)

    (*delayed (lambda () <bla>))
    (*parser <InfixSymbol>)
    (*delayed (lambda () <InfixParen>))
    
    (*parser <InfixSexprEscape>)
    (*disj 5)

    (*caten 3)
    (*pack-with
      (lambda (neg skip num)
        (if (car neg)
            (if (and (null? skip) (number? num))
                (* -1 num)
                `(,(cadr neg) ,num))
          num)))
    
    done))


(define <InfixParen>
  (new
    

    (*parser (char #\())
    (*parser <InfixExpression>)
    (*parser (char #\)))
    (*caten 3)
    (*pack-with
      (lambda (open exp close)
       exp))
    done))


(define <InfixArrayGet>
  (new
    
    (*parser (char #\[))
    (*parser <InfixExpression>)
    (*parser (char #\]))
    (*caten 3)
    
    (*pack-with
    (lambda (open exp close)
        (list 'vector-ref exp)))

    done))

(define <empty-function>
  (new

    (*parser (char #\())

    (*parser <InfixSkip>) *star
    (*parser (char #\)))
    (*caten 3)

    (*pack (lambda (_) '()))

    done))

(define <non-empty-function>
  (new

    (*parser (char #\()) 
    
    (*parser <InfixExpression>)
    
    (*parser (char #\,))
    (*parser <InfixExpression>)
    (*caten 2)
    (*pack-with 
        (lambda (ch exp) exp))
    *star
    
    (*caten 2)
    (*pack-with 
        (lambda (exp lexp) `(,exp ,@lexp)))
    
    (*parser <epsilon>)
    
    (*disj 2)
    
    (*parser (char #\)))
    (*caten 3)
    (*pack-with (lambda (open listExp close)
        `(,@listExp)))

    done))
    
(define <InfixFuncall>
  (new

    (*parser <empty-function>)
    (*parser <non-empty-function>)
    (*disj 2)

    done))


(define <layer-4-exp>
  (new
  
    (*parser <InfixNeg>)
    (*parser <InfixParen>)
    (*parser <InfixSexprEscape>)
    (*parser <InfixSymbol>)
    (*disj 4)

    
  done))


(define func3
    (lambda (name exp)
        (cons name exp)))



(define <bla>
  (new

    (*parser <InfixSymbol>)
    (*delayed (lambda () <InfixFuncall>))
    (*caten 2)
    (*pack-with
      (lambda (sym args)
        `(,sym ,@args)))
    
  done))


(define <layer-4>
  (new

    (*parser (wrap-parser-with-skips <InfixSkip> <layer-4-exp>))
    

    (*delayed (lambda () <layer-5>))
    *plus
    *maybe
    
    (*parser <InfixSkip>) *star

    (*caten 3)
    (*pack-with 
        (lambda (name exp skip)
            (if (car exp)
                (cond ((null? (caadr exp)) `(,name))
                    ((eq? (caaadr exp) 'vector-ref)
                        (fold-left func name (cadr exp)))
                  ((fold-left func3 name (cadr exp))))
                name)))
    done))


    
(define <layer-5>
  (new
      
    (*parser <InfixFuncall>)    
    (*parser <InfixArrayGet>)
    (*disj 2)

  done))


(define <layer-3> (<infix-operation-parser> <layer-3-op> <layer-4> append-temp))
(define <layer-2> (<infix-operation-parser> <layer-2-op> <layer-3> append-left))
(define <layer-1> (<infix-operation-parser> <layer-1-op> <layer-2> append-left))






