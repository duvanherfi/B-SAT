#lang eopl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*********************** Duvan Hernandez Figueroa  - 202010009  *******************
;*********************** Diego Fernando muñoz Arce - 202010032  *******************

;;*********************************Gramatitca**************************************

;;<BSAT>      ::= <expresion>

;;                <bsat-program (exp)>

;;<expresion>       ::= <numero>
;;                      <numero-exp (datum)>
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
;;                  ::= <vector>
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
;;                  ::= set-vec ( <expresion> , <vector> , <expresion>)
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
;;                  ::= ref-vec |  | ref-reg | set-reg
;;-----------------------privimitivas unarias-----------------------
;;<prim-un>         ::= solveFNC | lenght | concat
;;                  ::= add1 | sub1 | add1_16 | sub1_16
;;                  ::= empty | list? | head | tail
;;                  ::= vector?
;;                  ::= register?
;;------------------------------------------------------------------
;;<lista>           ::= [{<expresion>}*(;)]
;;                      <lista1 (lexps)>
;;<vector>          ::= vector[{<expresion>}*(;)]
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
    (expresion (numero) numero-exp)
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
    (expresion ("set-vec" "(" expresion "," vector "," expresion ")") set-vec-exp)
    (expresion ("set-reg" "(" expresion "," registro "," expresion ")") set-reg-exp)
    (expresion ("create-reg" "(" identificador "=" expresion "," registro")") crear-reg-exp)
    (expresion (lista) lista-exp)
    (expresion (vector) vector-exp)
    (expresion (registro) registro-exp)
    (expresion (expr-bool) bool-exp)
    (lista ("[" (separated-list expresion ",") "]") lista1)
    (vector ("vector" "[" (separated-list expresion ",") "]") vector1)
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
    (prim-un ("concat") concat-exp)
    (prim-un ("empty") vacio-exp)    
    (prim-un ("list?") lista?-exp)
    (prim-un ("head") cabeza-exp)
    (prim-un ("tail") cola-exp) 
    (prim-un ("register?") registros?-exp)
    (prim-un ("vector?") vector?-exp)
    
    ;------------primitivas binarias-------------
    (prim-bin ("%") modulo)
    (prim-bin ("+") suma)
    (prim-bin ("-") resta)
    (prim-bin ("*") mult)
    (prim-bin ("/") division)
    (prim-bin ("+_16") suma16)
    (prim-bin ("-_16") resta16)
    (prim-bin ("*_16") mult16)
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
;-------------------------------------------------------------------------------------------

(define scan&parse
  (sllgen:make-string-parser lexico gramatica))

;-------------------------------------------------------------------------------------------

(define interpretador
  (sllgen:make-rep-loop
   ">>"
   (lambda (pgm) pgm)
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

;---------------------------------prim-exp------------------------------------
;(scan&parse "solveFNC(FNC 3 ((1 or 2 or 3) and (3 or 2 or 1)))");  solve-fnc
;(scan&parse "+(2,3)"); prim-exp con +
;(scan&parse "-(2,3)"); prim-exp con - 
;(scan&parse "*(2,3)"); prim-exp con * 
;(scan&parse "/(2,3)"); prim-exp con / 
;(scan&parse "%(2,3)"); prim-exp con %
;(scan&parse "add1(2)");  prim-exp con add1  
;(scan&parse "sub1(2)");  prim-exp con sub1
;(scan&parse "+_16(2)");  prim-exp con +_16
;(scan&parse "-_16(2)");  prim-exp con -_16
;(scan&parse "*_16(2)");  prim-exp con *_16
;(scan&parse "add1_16(2)");  prim-exp con add1_16
;(scan&parse "sub1_16(2)");  prim-exp con sub1_16
;(scan&parse "lenght(\"cadena\")");  prim-exp con lenght
;(scan&parse "concat(\"cadena\",\"cadena\")");  prim-exp con concat
;(scan&parse "vacio()");  prim-exp con vacio
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
;(scan&parse "proc(@x) @x");  proc-exp
;(scan&parse "(@x 5)");  app-exp
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
