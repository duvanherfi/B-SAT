#lang eopl
;;<BSAT>      ::= <expresion>

;;                <bsat-program (exp)>

;;<expresion>       ::= <numero>
;;                      <numero-exp (datum)>
;;                  ::= x16 ( {<numero>}* )
;;                      <numerohex-exp (lsnum)>
;;                  ::= <caracter>
;;                      <caracter-exp (caracter)>
;;                  ::= <cadena>
;;                      <cadena-exp (cadena)>
;;                  ::= <bool>
;;                      <bool-exp (bool)>
;;                  ::= <identificador>
;;                      <identificador-exp (id)>
;;                  ::= $<identificador>
;;                      <refid-exp (id)>                  
;;                  ::= var {<identificador> = <expresion>}*(,) in <expresion>
;;                      <var-exp (ids exps cuerpo)>
;;                  ::= <identificador> -> <expresion>
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
;;                      <begin-exp (lexps)>
;;                  ::= if <expr-bool> then <expresion> [else <expresion> ] end
;;                      <if-exp (expb exp1 exp2)>
;;                  ::= while <expr-bool> do <expresion> done
;;                      <while-exp (expb exp)>
;;                  ::= for <identificador> = <expresion> (to | downto) <expresion> do <expresion> done
;;                      <for-exp (id exp1 to-odwto exp2 exp3)>
;;                  ::= <primitiva> ({<expresion>}*(,))
;;                      <prim-exp (lexp)>
;;                  ::= proc({<identificador>}*(,)) <expresion>
;;                      <proc-exp (ids body)>
;;                  ::= (<expresion> {expression}*)
;;                      <app-exp (expresion lexps)>
;;                  ::= letrec  {<identificador> ({<identificador>}*(,)) = <expresion>}* in <expresion>
;;                      <letrec-exp proc-names idss bodies bodyletrec>
;;                  ::= imprimir (<expresion>)
;;                      <print-exp>
;;                  ::= FNC <numero> (<clausula-or>)+("and")
;;                      <fnc-exp (numero lc-or)>
;;<clausula-or>     ::= (<numero>)+("or")
;;<primitiva>       ::= + | - | * | % | / | add1 | sub1 | solveFNC
;;                  ::= +_16 | -_16 | *_16 | add1_16 | sub1_16
;;                  ::= lenght | concat
;;                  ::= vacia | crear-lista | lista? | cabeza | cola | append
;;                  ::= vector? | crear-vec | ref-vec | set-vec
;;                  ::= registros? | crear-reg | ref-reg | set-reg
;;<lista>           ::= [{<expresion>}*(;)]
;;                      <lista (lexps)>
;;<vector>          ::= vector[{<expresion>}*(;)]
;;                      <vector (lexps)>
;;<registro>        ::= {{<identificador> = <expresion>}+(;)}
;;                      <registro (lids lexps)>
;;<expr-bool>       ::= <pred-prim> (<expresion> , <expresion>)
;;                      <comparacion (pprim exp1 exp2)>
;;                  ::= <oper-bin-bool> (<expr-bool> , <expr-bool>)
;;                      <union (obbool expb expb)>
;;                  ::= <bool>
;;                      <vlr-bool (bool)>
;;                  ::= <oper-un-bool> (<expr-bool>)
;;                      <op-comp (oubool expb)>
;;<pred-prim>       ::= <|>|<=|>=|==|<>
;;<oper-bin-bool>   ::= and|or
;;<oper-un-bool>    ::= not
;;<bool>            ::= true | false

(define lexico
  '(
    (espacioblanco (whitespace) skip)
    (comentario ("#" (not #\newline)) skip)
    (identificador (letter (arbno (or letter digit))) symbol)    
    (numero (digit (arbno digit)) number)
    (numero (digit (arbno digit) "." digit (arbno digit)) number)
    (numero ("-" digit (arbno digit)) number)
    (numero ("-" digit (arbno digit) "." digit (arbno digit)) number)
    )
  )

(define gramatica
  '(    
    (BSAT (expresion) bsat-program)    
    (expresion (numero) numero-exp)
    (expresion ("x_16(" (arbno numero) ")") numerohex-exp)
    (expresion ("'" identificador "'") caracter-exp)
    (expresion ("\"" identificador "\"") cadena-exp)
    (expresion (expr-bool) bool-exp)
    (expresion (identificador) identificador-exp)
    (expresion ("$" identificador) refid-exp)
    (expresion ("var" (separated-list identificador "=" expresion ",") "in" expresion)  var-exp)
    (expresion ("set" identificador "->" expresion) asignar-exp)
    (expresion ("cons" (separated-list identificador "=" expresion ",") "in" expresion)  cons-exp)
    (expresion ("rec" (arbno identificador "(" (separated-list identificador ",") ")" "=" expresion)  "in" expresion) 
                rec-exp)
    (expresion ("begin" expresion (arbno ";" expresion) "end") begin-exp)
    (expresion ("for" identificador "=" expresion to-o-downto expresion "do" expresion "done") for-exp)    
    (expresion (primitiva "(" (separated-list expresion ",") ")") prim-exp)
    (expresion ("proc" "(" (separated-list identificador ",") ")" expresion) proc-exp)
    (expresion ("(" expresion (arbno expresion) ")") app-exp)
    (expresion ("imprimir" "(" expresion ")") print-exp)
    (expresion ("FNC" numero "(" clausula-or (arbno "and" clausula-or) ")") fnc-exp)
    (expresion ("if" expr-bool "then" expresion "[" "else" expresion "]" "end") if-exp)
    (expresion ("while" expr-bool "do" expresion "done") while-exp)
    (expresion (lista) lista-exp)
    (expresion (vector) vector-exp)
    (expresion (registro) registro-exp)
    (lista ("[" (separated-list expresion ",") "]") lista1)
    (vector ("vector" "[" (separated-list expresion ",") "]") vector1)
    (registro ("{" identificador "=" expresion (arbno ";" identificador "=" expresion)"}") registro1)
    (expr-bool (pred-prim "(" expresion "," expresion ")") comparacion)
    (expr-bool (oper-bin-bool "(" expr-bool "," expr-bool ")") union-comp)
    (expr-bool (bool) vlr-bool)
    (expr-bool (oper-un-bool "(" expr-bool ")") op-comp) 
    (clausula-or ("(" numero (arbno "or" numero) ")" ) clausula-or-exp)
    (to-o-downto ("to") to)
    (to-o-downto ("downto") downto)
    (bool ("true") true-exp)
    (bool ("false") false-exp)
    (primitiva ("solveFNC") solve-fnc)
    (primitiva ("+") suma)
    (primitiva ("-") resta)
    (primitiva ("*") mult)
    (primitiva ("/") division)
    (primitiva ("%") modulo)
    (primitiva ("add1") add1)
    (primitiva ("sub1") sub1)
    (primitiva ("+_16") suma16)
    (primitiva ("-_16") resta16)
    (primitiva ("*_16") mult16)
    (primitiva ("add1_16") add1_16)
    (primitiva ("sub1_16") sub1_16)
    (primitiva ("lenght") lenght-exp)
    (primitiva ("concat") concat-exp)
    (primitiva ("vacio") vacio-exp)
    (primitiva ("crear-lista") crear-lista-exp)
    (primitiva ("lista?") lista?-exp)
    (primitiva ("cabeza") cabeza-exp)
    (primitiva ("cola") cola-exp)
    (primitiva ("append") append-exp)
    (primitiva ("vector?") vector?-exp)
    (primitiva ("crear-vec") crear-v-exp)
    (primitiva ("ref-vec") ref-vec-exp)
    (primitiva ("set-vec") set-vec-exp)
    (primitiva ("registros?")registros?-exp)
    (primitiva ("crear-reg") crear-reg-exp)
    (primitiva ("ref-reg") ref-reg-exp)
    (primitiva ("set-reg") set-reg-exp)
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

(sllgen:make-define-datatypes lexico gramatica)
;(sllgen:list-define-datatypes lexico gramatica)

(define scan&parse
  (sllgen:make-string-parser lexico gramatica))

(define interpretador
  (sllgen:make-rep-loop
   ">>"
   (lambda (pgm) pgm)
   (sllgen:make-stream-parser lexico gramatica)
   )
  )


;; pruebas de producciones

;(scan&parse "5");  numero-exp
;(scan&parse "x_16(4 5 3)");  numerohex-exp
;(scan&parse "'f'");  caracter-exp
;(scan&parse "\" hola \"");  cadena-exp
;(scan&parse "false");  bool-exp con false-exp
;(scan&parse "true");  bool-exp con true-exp
;(scan&parse "x");   identificador-exp
;(scan&parse "$x"); refid-exp
;;-----------------------------------------------------------------------
;(scan&parse "var x = 6 in add1(x)");  var-exp Para qué es el in?
;;-----------------------------------------------------------------------
;(scan&parse "set x -> 6");  asignar-exp
;(scan&parse "cons x = 6 in imprimir(x)");  cons-exp
;(scan&parse "rec f(x)= add1(x) in (f 7)"); rec-exp 
;(scan&parse "begin imprimir(\"hola\") ; imprimir(\"mundo\") end");  begin-exp
;(scan&parse "for x = 1 to 5 do imprimir(x) done");  for-exp con to
;(scan&parse "for x = 5 downto 1 do imprimir(x) done");  for-exp con downto
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
;         ;(scan&parse "crear-lista(append 5 [])");  prim-exp con crear-lista 
;(scan&parse "lista?([x,y])");  prim-exp con lista?
;(scan&parse "cabeza([x,y])");  prim-exp con cabeza
;(scan&parse "cola([x,y])");  prim-exp con cola
;(scan&parse "append([x,y],[v,w])");  prim-exp con append
;(scan&parse "vector?(vector[x,y])");  prim-exp con vector?
         ;(scan&parse "crear-vec(append 5 vector[])");  prim-exp con crear-vec
;(scan&parse "ref-vec(2,vector[x,y])");  prim-exp con ref-vec
;(scan&parse "set-vec(3,vector[x,y],5)");  prim-exp con set-vec
;(scan&parse "registros?({x=8})");  prim-exp con registros?
;         ;(scan&parse "crear-reg(x=8,{})");  prim-exp con crear-reg
;(scan&parse "ref-reg(x,{x=8})");  prim-exp con ref-reg
;(scan&parse "set-reg(x,{x=8},9)");  prim-exp con set-reg
;(scan&parse "solveFNC(FNC 3 ((1 or 2 or 3) and (3 or 2 or 1)))");  solve-fnc
;(scan&parse "proc(x) y");  proc-exp
;(scan&parse "(5)");  app-exp
;(scan&parse "imprimir(\"Hola\")"); print-exp
;;------------------------------------------------------------------
;(scan&parse "FNC 2 ((5 or 6))");  fnc-exp No funciona sin el doble paréntesis
;------------------------------------------------------------------
;(scan&parse "FNC 2 ((5 or 6) and (3 or 6))");  fnc-exp 
;(scan&parse "if <(2,3) then 2 [else 3] end");  if-exp con pred-prim
;(scan&parse "if >(2,3) then 2 [else 3] end");  if-exp con pred-prim
;(scan&parse "if <=(2,3) then 2 [else 3] end");  if-exp con pred-prim
;(scan&parse "if >=(2,3) then 2 [else 3] end");  if-exp con pred-prim
;(scan&parse "if ==(2,3) then 2 [else 3] end");  if-exp con pred-prim
;(scan&parse "if <>(2,3) then 2 [else 3] end");  if-exp con pred-prim
;(scan&parse "if and(true,false) then 2 [else 3] end");  if-exp con oper-bin-bool
;(scan&parse "if or(true,false) then 2 [else 3] end");  if-exp con oper-bin-bool
;(scan&parse "if true then 2 [else 3] end");  if-exp con true-exp
;(scan&parse "if false then 2 [else 3] end");  if-exp con false-exp
;(scan&parse "if not(true) then 2 [else 3] end");  if-exp con oper-un-bool
;(scan&parse "while not(true) do 2 done");  while-exp con oper-un-bool
;(scan&parse "[4,5]");  lista-exp
;(scan&parse "vector[4,5]");  vector-exp
;(scan&parse "{x=2;y=5}");  registro-exp

;(interpretador)
