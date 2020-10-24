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
;;                  ::= <bool-exp (bool)>
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
;;                  ::= <bool-exp (exp-bool)>
;;                  ::= begin {<expresion>}+(;) end
;;                      <begin-exp (lexps)>
;;                  ::= if <expr-bool> then <expresion> [else <expresion> ] end
;;                      <if-exp (expb exp1 exp2)>
;;                  ::= while <expr-bool> do <expresion> done
;;                      <while-exp (expb exp)>
;;                  ::= for <identificador> = <expresion> (to | downto) <expresion> do <expresion> done
;;                      <for-exp (id exp1 exp2 exp3)>
;;                  ::= <primitiva> ({<expresion>}*(,))
;;                      <prim-exp (lexp)>
;;                  ::= proc({<identificador>}*(,)) <expresion>
;;                      <proc-exp (ids body)>
;;                  ::= letrec  {<identificador> ({<identificador>}*(,)) = <expresion>}* in <expresion>
;;                      <letrec-exp proc-names idss bodies bodyletrec>
;;                  ::= imprimir (<expresion>)
;;                      <print-exp>
;;                  ::= FNC <numero> (<clausula-or>)+("and")
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
;;                      <bool-exp (bool)>
;;                  ::= <oper-un-bool> (<expr-bool>)
;;                      <op-comp (oubool expb)>
;;<pred-prim>       ::= <|>|<=|>=|==|<>
;;<oper-bin-bool>   ::= and|or
;;<oper-un-bool>    ::= not
;;<bool>            ::= true
;;                  ::= false

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
    (expresion ("x16" "(" (arbno numero) ")") numerohex-exp)
    (expresion ("'" identificador "'") caracter-exp)
    (expresion ("\"" identificador "\"") cadena-exp)
    (expresion (expr-bool) bool-exp)
    (expresion (identificador) identificador-exp)
    (expresion ("$" identificador) refid-exp)
    (expresion ("var" identificador "=" expresion (arbno "," identificador "=" expresion) "in" expresion)  var-exp)
    (expresion ("set" identificador "->" expresion) asignar-exp)
    (expresion ("cons" identificador "=" expresion (arbno "," identificador "=" expresion) "in" expresion)  cons-exp)
    (expresion ("rec" (arbno identificador "(" (separated-list identificador ",") ")" "=" expresion)  "in" expresion) 
                rec-exp)
    (expresion ("begin" expresion (arbno ";" expresion) "end") begin-exp)
    (expresion ("for" identificador "=" expresion to-o-downto expresion "do" expresion "done") for-exp)    
    (expresion (primitiva "(" (separated-list expresion ",") ")") prim-exp)
    (expresion ("proc" "(" (separated-list identificador ",") ")" expresion) proc-exp)    
    (expresion ("imprimir" "(" expresion ")") print-exp)
    (expresion ("FNC" numero "(" clausula-or (arbno "and" clausula-or) ")") fnc-exp)
    (expresion ("if" expr-bool "then" expresion "[" "else" expresion "]" "end") if-exp)
    (expresion ("while" expr-bool "do" expresion "done") while-exp)
    (expresion (lista) lista-exp)
    (expresion (vector) vector-exp)
    (espresion (registro) registro-exp)
    (lista ("[" (separated-list expresion ",") "]") lista1)
    (vector ("vector" "[" (separated-list expresion ",") "]") vector1)
    (registro ("{" identificador "=" expresion (arbno ";" identificador "=" expresion)"}") registro1)
    (expr-bool (pred-prim "(" expresion "," expresion ")") comparacion)
    (expr-bool (oper-bin-bool "(" expr-bool "," expr-bool ")") union-comp)
    (expr-bool (bool) vlr-bool)
    (expr-bool (oper-un-bool "(" expr-bool ")") op-comp) 
    (clausula-or (numero (arbno "or" numero)) clausula-or-exp)
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
    (primitiva ("vacia") vacia-exp)
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

;;                  ::= rec  {<identificador> ({<identificador>}*(,)) = <expresion>}* in <expresion>
;;                      <rec-exp (lproc ids cuerpos cuerporec)>
;;(expresion ("rec" (arbno identificador "(" (separated-list identificador ",") ")" "=" expresion)  "in" expresion) 
                ;;rec-exp)





