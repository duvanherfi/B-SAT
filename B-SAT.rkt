#lang eopl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*********************** Duvan Hernandez Figueroa  - 202010009  *******************
;*********************** Diego Fernando muñoz Arce - 202010032  *******************

;;*********************************Gramatitca**************************************

;;<BSAT>      ::= <expresion>

;;                <bsat-program (exp)>

;;<expresion>       ::= <numero>
;;                      <num-exp (datum)>
;;                  ::= x16( {<numero>}* )
;;                      <numerohex-exp (lsnum)>
;;                  ::= '<caracter>'
;;                      <caracter-exp (caracter)>
;;                  ::= "<cadena>"
;;                      <cadena-exp (cadena)>
;;                  ::= <identificador>
;;                      <identificador-exp (id)>
;;                  ::= $<identificador>
;;                      <refid-exp (id)>                  
;;                  ::= var {<identificador> = <expresion>}*(,) in <expresion>
;;                      <var-exp (ids exps cuerpo)>
;;                  ::= set <identificador> -> <expresion>
;;                      <set-exp (id exp)>
;;                  ::= cons {<identificador> = <expresion>}*(,)
;;                      <cons-exp (ids exps cuerpo)> in <expresion>
;;                  ::= rec  {<identificador> ({<identificador>}*(,)) = <expresion>}* in <expresion>
;;                      <rec-exp (lproc ids cuerpos cuerporec)>
;;                  ::= <lista>
;;                      <lista-exp (lista)>
;;                  ::= <vectorB>
;;                      <vector-exp (vector)>
;;                  ::= <registro>
;;                      <registro-exp (registro)>
;;                  ::= <exp-bool>
;;                      <bool-exp (exp-bool)>
;;                  ::= begin {<expresion>}+(;) end
;;                      <begin-exp (exp lexps)>
;;                  ::= if <expr-bool> then <expresion> else <expresion> end
;;                      <if-exp (expb exp1 exp2)>
;;                  ::= while <expr-bool> do <expresion> done
;;                      <while-exp (expb exp)>
;;                  ::= for <identificador> = <expresion> <to-odownto> <expresion> do <expresion> done
;;                      <for-exp (id exp1 to-odwto exp2 exp3)>
;;                  ::= create-reg(<identificador> = <expresion> , <registro>)
;;                      <create-reg (id exp reg)>
;;                  ::= set-vec ( <expresion> , <vectorB> , <expresion>)
;;                      <set-vec (pos vec val)>
;;                  ::= set-rec ( <expresion> , <registro> , <expresion>)
;;                      <set-rec (pos reg val)>
;;                  ::= <prim-bin> (expresion , expresion)
;;                      <primbin-exp (lexp)>
;;                  ::= <prim-un> (expresion)
;;                      <primun-exp (lexp)>
;;                  ::= proc({<identificador>}*(,)) <expresion>
;;                      <proc-exp (ids body)>
;;                  ::= (<expresion> {expression}*)
;;                      <app-exp (expresion lexps)>
;;                  ::= print (<expresion>)
;;                      <print-exp>
;;                  ::= FNC <numero> (<clausula-or>)+("and")
;;                      <fnc-exp (numero cla-or lcla-or)>
;;<clausula-or>     ::= (<numero>)+("or")
;;                      <clausula-or-exp (n lsn)>
;;-----------------------primitivas binarias------------------------
;;<prim-bin>        ::= + | - | * | % | / | +_16 | -_16 | *_16
;;                  ::= create-list | append | create-vec | create-reg
;;                  ::= ref-vec |  | ref-reg | set-reg | concat
;;-----------------------privimitivas unarias-----------------------
;;<prim-un>         ::= solveFNC | lenght 
;;                  ::= add1 | sub1 | add1_16 | sub1_16
;;                  ::= list? | head | tail
;;                  ::= vector?
;;                  ::= register?
;;------------------------------------------------------------------
;;<lista>           ::= empty
;;                      <empty-list>
;;                  ::= [{<expresion>}*(;)]
;;                      <lista1 (lexps)>
;;<vectorB>          ::= vector[{<expresion>}*(;)]
;;                      <vector1 (lexps)>
;;<registro>        ::= {{<identificador> = <expresion>}+(;)}
;;                      <registro1 (id exp lids lexps)>
;;<expr-bool>       ::= <pred-prim> (<expresion> , <expresion>)
;;                      <comparacion (pprim exp1 exp2)>
;;                  ::= <oper-bin-bool> (<expr-bool> , <expr-bool>)
;;                      <conjuncion (obbool expb1 expb1)>
;;                  ::= <bool>
;;                      <vlr-bool (bool)>
;;                  ::= <oper-un-bool> (<expr-bool>)
;;                      <op-comp (oubool expb)>
;;<to-odownto>      ::= to
;;                      <to>
;;                  ::= downto
;;                      <downto>
;;<pred-prim>       ::= <|>|<=|>=|==|<>
;;<oper-bin-bool>   ::= and|or
;;<oper-un-bool>    ::= not
;;<bool>            ::= true | false

;;*********************************Definición Lexico**************************************

(define lexico
  '(
    (espacioblanco (whitespace) skip)
    (comentario ("#" (not #\newline)) skip)
    (identificador ("@" letter (arbno (or letter digit))) symbol)
    (letras (letter) string)
    (letras (letter (arbno (or letter digit))) string)    
    (numero (digit (arbno digit)) number)
    (numero (digit (arbno digit) "." digit (arbno digit)) number)
    (numero ("-" digit (arbno digit)) number)
    (numero ("-" digit (arbno digit) "." digit (arbno digit)) number)
    )
  )

;;*********************************Definición Grámatica**********************************

(define gramatica
  '(    
    (BSAT (expresion) bsat-program)    
    (expresion (numero) num-exp)
    (expresion ("x_16(" (arbno numero) ")") numerohex-exp)
    (expresion ("'" letras "'") caracter-exp)
    (expresion ("\"" letras "\"") cadena-exp)
    (expresion (identificador) identificador-exp)
    (expresion ("$" identificador) refid-exp)
    (expresion ("var" (separated-list identificador "=" expresion ",") "in" expresion)  var-exp)
    (expresion ("set" identificador "->" expresion) asignar-exp)
    (expresion ("cons" (separated-list identificador "=" expresion ",") "in" expresion)  cons-exp)
    (expresion ("rec" (arbno identificador "(" (separated-list identificador ",") ")" "=" expresion)  "in" expresion) 
                rec-exp)
    (expresion ("begin" expresion (arbno ";" expresion) "end") begin-exp)
    (expresion ("for" identificador "=" expresion to-o-downto expresion "do" expresion "done") for-exp)    
    (expresion (prim-bin "(" expresion "," expresion ")") primbin-exp)
    (expresion (prim-un "(" expresion ")") primun-exp)
    (expresion ("proc" "(" (separated-list identificador ",") ")" expresion) proc-exp)
    (expresion ("(" expresion (arbno expresion) ")") app-exp)
    (expresion ("print" "(" expresion ")") print-exp)
    (expresion ("FNC" numero "(" clausula-or (arbno "and" clausula-or) ")") fnc-exp)
    (expresion ("if" expr-bool "then" expresion "else" expresion "end") if-exp)
    (expresion ("while" expr-bool "do" expresion "done") while-exp)
    (expresion ("set-vec" "(" expresion "," vectorB "," expresion ")") set-vec-exp)
    (expresion ("set-reg" "(" expresion "," registro "," expresion ")") set-reg-exp)
    (expresion ("create-reg" "(" identificador "=" expresion "," registro")") crear-reg-exp)
    (expresion (lista) lista-exp)
    (expresion (vectorB) vector-exp)
    (expresion (registro) registro-exp)
    (expresion (expr-bool) bool-exp)
    (lista ("empty") empty-list)
    (lista ("[" (separated-list expresion ",") "]") lista1)
    (vectorB ("vector" "[" (separated-list expresion ",") "]") vector1)
    (registro ("{" identificador "=" expresion (arbno ";" identificador "=" expresion)"}") registro1)
    (expr-bool (pred-prim "(" expresion "," expresion ")") comparacion)
    (expr-bool (oper-bin-bool "(" expr-bool "," expr-bool ")") conjuncion)
    (expr-bool (bool) vlr-bool)
    (expr-bool (oper-un-bool "(" expr-bool ")") op-comp) 
    (clausula-or ("(" numero (arbno "or" numero) ")" ) clausula-or-exp)
    (to-o-downto ("to") to)
    (to-o-downto ("downto") downto)
    (bool ("true") true-exp)
    (bool ("false") false-exp)

    ;------------primitivas unarias-------------
    (prim-un ("solveFNC") solve-fnc)
    (prim-un ("add1") add1)
    (prim-un ("sub1") sub1)    
    (prim-un ("add1_16") add1_16)
    (prim-un ("sub1_16") sub1_16)
    (prim-un ("lenght") lenght-exp)   
    (prim-un ("list?") lista?-exp)
    (prim-un ("head") cabeza-exp)
    (prim-un ("tail") cola-exp) 
    (prim-un ("register?") registros?-exp)
    (prim-un ("vector?") isvector-exp)
    
    ;------------primitivas binarias-------------
    (prim-bin ("%") moduloB)
    (prim-bin ("+") suma)
    (prim-bin ("-") resta)
    (prim-bin ("*") mult)
    (prim-bin ("/") division)
    (prim-bin ("+_16") suma16)
    (prim-bin ("-_16") resta16)
    (prim-bin ("*_16") mult16)
    (prim-bin ("concat") concat-exp)
    (prim-bin ("append") append-exp)
    (prim-bin ("create-list") crear-lista-exp)
    (prim-bin ("create-vec") crear-v-exp)
    (prim-bin ("ref-vec") ref-vec-exp)   
    (prim-bin ("ref-reg") ref-reg-exp)
        
    (pred-prim ("<") menor-exp)
    (pred-prim (">") mayor-exp)
    (pred-prim ("<=") menor=exp)
    (pred-prim (">=") mayor=exp)
    (pred-prim ("==") igual=exp)
    (pred-prim ("<>") diferente-exp)
    (oper-bin-bool ("and") and-exp)
    (oper-bin-bool ("or") or-exp)
    (oper-un-bool ("not") not-exp)
    )
  )
;-------------------------------------------------------------------------------------------
(sllgen:make-define-datatypes lexico gramatica)
;(sllgen:list-define-datatypes lexico gramatica)

;scan&parse
;-------------------------------------------------------------------------------------------

(define scan&parse
  (sllgen:make-string-parser lexico gramatica))

;ambiente
;-------------------------------------------------------------------------------------------
(define-datatype ambiente ambiente?
  (empty-env)
  (extend-env (lvar (list-of symbol?))
              (lvalor vector?)
              (env ambiente?)
              )  
  )

(define recursively-extended-env-record
  (lambda (proc-names lidss bodies old-env)
    (let*
        (
         (len (length proc-names))
         (vec (make-vector len))
         (env (extend-env proc-names vec old-env))
         )
      (letrec
          [
           (actualizar-vector
            (lambda (pos lidds lbodies)
              (cond
                [(null? lidds) env]
                [else
                 (begin
                   (vector-set! vec pos (direct-target (closure (car lidds) (car lbodies) env)))
                   (actualizar-vector (+ pos  1) (cdr lidds) (cdr lbodies))
                   )
                 ]
                )
              )
            )
           ]
        (actualizar-vector 0 lidss bodies)
       )
        )
      )
  )

;referencia
;-------------------------------------------------------------------------------------------
(define-datatype referencia referencia?  
  (a-ref (pos number?) (vec vector?))  
  )

;Funcion para validar si es un target de referencia
;-------------------------------------------------------------------------------------------
(define ref-to-direct-target?
  (lambda (val)
    (if (referencia? val)
        (cases referencia val
          (a-ref (pos vec)
                 (cases target (vector-ref vec pos)
                   (direct-target (expval) #t)
                   (cons-target (expval) #t)
                   (indirect-target (ref) (eopl:error "no puede pasar una referencia enre procedimientos")
                   )
                 )
                 )
          )
        #f
        )
    )
  )

;funcion que valida si puede ser un target directo
;-------------------------------------------------------------------------------------------
(define no-refid-exp?
  (lambda (exp)
    (cond
      [(number? exp) #t]
      [(expresion? exp)
       (cases expresion exp
         (refid-exp (id) #f)
         (else #t)
         )]
      )    
    )
  )

;target
;-------------------------------------------------------------------------------------------
(define-datatype target target?
  (direct-target (expval no-refid-exp?))
  (indirect-target (ref ref-to-direct-target?))
  (cons-target (expval no-refid-exp?))
  )


;Bignum
;-------------------------------------------------------------------------------------------
(define zero
  (lambda ()
    '()))

(define base 16)

(define is-zero?
  (lambda (n)
    (null? n)))

(define successor
  (lambda (n)
    (if (is-zero? n)
	'(1)
	(let ((t (+ (car n) 1)))
	  (if (= t base)
	      (cons 0 (successor (cdr n)))
	      (cons t (cdr n))
              )
          )
        )
    )
  )

(define predecessor
  (lambda (n)
    (cond
     ((is-zero? n) (eopl:error "cero no tiene predecesor"))
     ((>= (car n) base) (eopl:error "el valor debe ser menor que 16"))
     ((equal? n '(1)) '())
     ((zero? (car n))
      (if (null? (cdr n))
	  (eopl:error "cero no tiene predecesor")
	  (cons (- base 1) (predecessor (cdr n)))
          )
      )
      (else (cons (- (car n) 1) (cdr n)))
      )
    )
  )

(define suma-bignum
  (lambda (x y)
    (if (is-zero? x)
        y
        (successor (suma-bignum (predecessor x) y)))))

(define resta-bignum
  (lambda (x y)
    (if (is-zero? y)
        x
        (predecessor (resta-bignum  x (predecessor y))))))

(define mult-bignum
  (lambda (x y)
    (if (is-zero? x)
        (zero)
        (suma-bignum (mult-bignum (predecessor x) y) y))
    ))

;apply-env-ref
;-------------------------------------------------------------------------------------------
(define apply-env-ref
  (lambda (amb var)
    (cases ambiente amb
      (empty-env () (eopl:error "no se encontró la variable ~s" var))
      (extend-env (lvar vec env)
                  (letrec
                      [
                       (buscar-ambiente
                        (lambda (pos lids)
                          (cond
                            [(null? lids) (apply-env-ref env var)]
                            [(equal? var (car lids)) (a-ref pos vec)]
                            [else (buscar-ambiente (+ pos 1) (cdr lids))]
                            )
                          )
                        )
                       ]
                    (buscar-ambiente 0 lvar)
                      )
                  )
      )
    )
  )

;def-ref
;-------------------------------------------------------------------------------------------
(define def-ref
  (lambda (ref)
    (cases target (primitive-deref ref)
       (direct-target (exp-val) exp-val)
       (cons-target (exp-val) exp-val)
       (indirect-target (ref1)
                        (cases target (primitive-deref ref1)
                          (direct-target (exp-val) exp-val)
                          (cons-target (exp-val) exp-val)
                          (indirect-target (ref2)
                                           (eopl:error "solo se pueden de 1 referencia a otra")
                                           )
                          )
                        )
      )
    )
  )

;primitive-deref
;-------------------------------------------------------------------------------------------
(define primitive-deref
 (lambda (ref)
   (cases referencia ref
     (a-ref (pos vec)
            (vector-ref vec pos)
            )
     )
   )
 )

;set-ref!
;-------------------------------------------------------------------------------------------
(define set-ref!
  (lambda (ref val)
    (let
        ((ref
          (cases target (primitive-deref ref)
            (direct-target (exp-val) ref)
            (cons-target (exp-val) (eopl:error "No se puede cambiar el valor de una constante"))
            (indirect-target (ref1) ref1)
            )
          )
         )
      (primitive-setref! ref (direct-target val))
      )
    )
  )

;primitive-setref!
;-------------------------------------------------------------------------------------------
(define primitive-setref!
  (lambda (ref val)
    (cases referencia ref
      (a-ref (pos vec)
             (vector-set! vec pos val)
             )
      )
    )
  )

;apply-env
;-------------------------------------------------------------------------------------------
(define apply-env
  (lambda (env var)
    (def-ref (apply-env-ref env var))
   )
  )




;ambiente inicial
;-------------------------------------------------------------------------------------------
(define init-env  
  (extend-env (list '@x '@y '@z '@a)
              (list->vector (list (direct-target 4)
                                  (direct-target 2)
                                  (direct-target 5)
                                  (indirect-target (a-ref 0 (list->vector (list (direct-target 4)
                                  (direct-target 2)
                                  (direct-target 5)))))))
              (empty-env))
  )

;eval-binprim
;-------------------------------------------------------------------------------------------
;    (prim-bin ("append") append-exp)
;    (prim-bin ("create-list") crear-lista-exp)
;    (prim-bin ("create-vec") crear-v-exp)
;    (prim-bin ("ref-vec") ref-vec-exp)   
;    (prim-bin ("ref-reg") ref-reg-exp)


(define eval-binprim
  (lambda (op num1 num2)
    (cases prim-bin op
      (suma () (+ num1 num2))
      (resta () (- num1 num2))
      (moduloB () (modulo num1 num2))
      (mult () (* num1 num2))
      (division () (/ num1 num2))
      (suma16 () (suma-bignum num1 num2))
      (resta16 () (resta-bignum num1 num2))
      (mult16 () (mult-bignum num1 num2))
      (concat-exp () (string-append num1 num2))
      (else #f)
      )
    )
  )

; clousure
;-------------------------------------------------------------------------------------------
(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body expresion?)
   (env ambiente?)
   )
  )

(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
               (eval-expresion body (extend-env ids (list->vector args) env)))
      )
    )
  )

;eval-rand-pref
;-------------------------------------------------------------------------------------------
(define eval-rand-pref
  (lambda (x amb)
    (cases expresion x
      (refid-exp (id) (indirect-target
                     (let
                         (
                          (ref (apply-env-ref amb id))
                          )
                       (cases target (primitive-deref ref)
                         (direct-target (expval) ref)
                         (cons-target (expval) ref)
                         (indirect-target (ref1) ref1)
                         )
                         )
                     ))
      (else
       (direct-target (eval-expresion x amb))
       )
      )
    )
  )

;eval-expresion
;-------------------------------------------------------------------------------------------
(define eval-expresion
  (lambda (pgm env)
    (cases expresion pgm
      (num-exp (n) n)
      (numerohex-exp (lnum) lnum)
      (caracter-exp (caracter) (string->symbol caracter))
      (cadena-exp (cad) cad)
      (identificador-exp (id) (apply-env env id))
      (refid-exp (id) (indirect-target
                     (let
                         (
                          (ref (apply-env-ref env id))
                          )
                       (cases target (primitive-deref ref)
                         (direct-target (expval) ref)
                         (cons-target (expval) ref)
                         (indirect-target (ref1) ref1)
                         )
                         )
                     ))
      (var-exp (ids rands body)
               (let
                   ((rands-num (map (lambda (x) (eval-rand-pref x env)) rands)))
                 (eval-expresion body (extend-env ids (list->vector rands-num) env))                 
                   ))
      (asignar-exp (id exp)
                   (begin
                     (set-ref!
                      (apply-env-ref env id)
                      (eval-expresion exp env))
                     'OK!))
      (cons-exp (ids rands body)
                (let
                   ((rands-num (map (lambda (x) (cons-target (eval-expresion x env))) rands)))
                 (eval-expresion body (extend-env ids (list->vector rands-num) env))                 
                   ))
      (primbin-exp (op exp1 exp2)
                   (eval-binprim op
                                 (eval-expresion exp1 env)
                                 (eval-expresion exp2 env)))
      (proc-exp (ids body)
                (closure ids body env)
                )
      (app-exp (rator rands)
               (let
                   [
                    (proc (eval-expresion rator env))
                    (lrands (map (lambda (x) (eval-rand-pref x env))rands))
                    ]
                 
               (if (procval? proc)
                   (apply-procedure proc lrands)
                   (eopl:error "%s no es un procedimiento" proc)
                   )
                 )
               )
      (rec-exp (proc-names idss bodies body)
               (eval-expresion body (recursively-extended-env-record proc-names idss bodies env)))
      (else pgm)
      )
    )
  )

;eval-program
;-------------------------------------------------------------------------------------------

(define eval-program
  (lambda (pgm)
    (cases BSAT pgm
        (bsat-program (exp) (eval-expresion exp init-env))
        )
    )
  )

;-------------------------------------------------------------------------------------------

(define interpretador
  (sllgen:make-rep-loop
   ">>"
   (lambda (pgm) (eval-program pgm))
   (sllgen:make-stream-parser lexico gramatica)
   )
  )

(interpretador)

; pruebas de producciones
;-------------------------------------------------------------------------------------------

;(scan&parse "5");  numero-exp
;(scan&parse "x_16(4 5 3)");  numerohex-exp
;(scan&parse "'f'");  caracter-exp
;(scan&parse "\" hola \"");  cadena-exp
;(scan&parse "false");  bool-exp con false-exp
;(scan&parse "true");  bool-exp con true-exp
;(scan&parse "@x");   identificador-exp
;(scan&parse "var @x = 6 in add1(@x)");  var-exp
;(scan&parse "$@x"); refid-exp 
;(scan&parse "set @x -> 6");  asignar-exp
;(scan&parse "cons @x = 6 in print(@x)");  cons-exp
;(scan&parse "rec @f(@x)= add1(@x) in (@f 7)"); rec-exp 
;(scan&parse "begin print(\"hola\") ; print(\"mundo\") end");  begin-exp
;(scan&parse "for @x = 1 to 5 do print(@x) done");  for-exp con to
;(scan&parse "for @x = 5 downto 1 do print(@x) done");  for-exp con downto

;---------------------------------------------------------------------
;(scan&parse "solveFNC(FNC 3 ((1 or 2 or 3) and (3 or 2 or 1)))");  solve-fnc
;(scan&parse "+(2,3)"); prim-exp con +
;(scan&parse "-(2,3)"); prim-exp con - 
;(scan&parse "*(2,3)"); prim-exp con * 
;(scan&parse "/(2,3)"); prim-exp con / 
;(scan&parse "%(2,3)"); prim-exp con %
;(scan&parse "add1(2)");  prim-exp con add1  
;(scan&parse "sub1(2)");  prim-exp con sub1
;(scan&parse "+_16(x_16(4 5 3), x_16(1 1))");  prim-exp con +_16
;(scan&parse "-_16(x_16(4 5 3), x_16(1 2))");  prim-exp con -_16
;(scan&parse "*_16(x_16(4 5 3), x_16(1))");  prim-exp con *_16
;(scan&parse "add1_16(x_16(4 5 3))");  prim-exp con add1_16
;(scan&parse "sub1_16(x_16(4 5 3))");  prim-exp con sub1_16
;(scan&parse "lenght(\"cadena\")");  prim-exp con lenght
;(scan&parse "concat(\"cadena\",\"cadena\")");  prim-exp con concat
;(scan&parse "empty");  prim-exp con empty
;(scan&parse "create-list(5,[])");  prim-exp con crear-lista 
;(scan&parse "list?([@x,@y])");  prim-exp con lista?
;(scan&parse "head([@x,@y])");  prim-exp con cabeza
;(scan&parse "tail([@x,@y])");  prim-exp con cola
;(scan&parse "append([@x,@y],[@v,@w])");  prim-exp con append
;(scan&parse "vector?(vector[@x,@y])");  prim-exp con vector?
;(scan&parse "create-vec(5 , vector[])");  prim-exp con crear-vec
;(scan&parse "ref-vec(2,vector[@x,@y])");  prim-exp con ref-vec
;(scan&parse "register? ({@x=8})");  prim-exp con register?
;(scan&parse "create-reg(@x=8,{@d=3})");  prim-exp con crear-reg
;(scan&parse "ref-reg(@x,{@x=8})");  prim-exp con ref-reg

;---------------------------------------------------------------------------------

;(scan&parse "set-vec(3,vector[@x,@y],5)");  prim-exp con set-vec
;(scan&parse "set-reg(@x,{@x=8},9)");  prim-exp con set-reg
;(scan&parse "proc(@x) set @x->4");  proc-exp
;(scan&parse "(@x 5)");  app-exp paso por valor
;(scan&parse "(@x $@z)");  app-exp paso por referencia 
;(scan&parse "print(\"Hola\")"); print-exp
;(scan&parse "FNC 2 ((5 or 6) and (3 or 6))");  fnc-exp

;-----------------------------------if-exp----------------------------------------
;(scan&parse "if <(2,3) then 2 else 3 end");  if-exp con pred-prim
;(scan&parse "if >(2,3) then 2 else 3 end");  if-exp con pred-prim
;(scan&parse "if <=(2,3) then 2 else 3 end");  if-exp con pred-prim
;(scan&parse "if >=(2,3) then 2 else 3 end");  if-exp con pred-prim
;(scan&parse "if ==(2,3) then 2 else 3 end");  if-exp con pred-prim
;(scan&parse "if <>(2,3) then 2 else 3 end");  if-exp con pred-prim
;(scan&parse "if and(true,false) then 2 else 3 end");  if-exp con oper-bin-bool
;(scan&parse "if or(true,false) then 2 else 3 end");  if-exp con oper-bin-bool
;(scan&parse "if true then 2 else 3 end");  if-exp con true-exp
;(scan&parse "if false then 2 else 3 end");  if-exp con false-exp
;(scan&parse "if not(true) then 2 else 3 end");  if-exp con oper-un-bool

;---------------------------------------------------------------------------------
;(scan&parse "while not(true) do 2 done");  while-exp con oper-un-bool
;(scan&parse "[4,5]");  lista-exp
;(scan&parse "vector[4,5]");  vector-exp
;(scan&parse "{@x=2;@y=5}");  registro-exp
;(scan&parse "<(2,1)");  bool-exp
