#lang eopl
;;<BSAT>      ::= <expresion>
;;                <bsat-program (exp)>

;;<expresion>       ::= <numero>
;;                      <numero-exp (datum)>
;;                  ::= <caracter>
;;                      <caracter-exp (caracter)>
;;                  ::= <cadena>
;;                      <cadena-exp (cadena)>
;;                  ::= <bool>
;;                  ::= <bool-exp (bool)>
;;                  ::= <identificador>
;;                      <identificador-exp (id)>
;;                  ::= $<identificador>
;;                      <identificadorR-exp (id)>                  
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
;;                  ::= <prim-hex> ({<expresion>}*(,))
;;                      <primhex-exp (lexp)>
;;                  ::= <primitiva-cad> ({<expresion>}*(,))
;;                      <primcad-exp (lexp)>
;;                  ::= <primitiva-list> ({<expresion>}*(,))
;;                      <primls-exp (lexp)>
;;                  ::= <primitica-vect> ({<expresion>}*(,))
;;                      <primvec-exp (lexp)>
;;                  ::= <primitica-reg> ({<expresion>}*(,))
;;                      <primreg-exp (lexp)>
;;                  ::= proc({<identificador>}*(,)) <expresion>
;;                      <proc-exp (ids body)>
;;                  ::= letrec  {<identificador> ({<identificador>}*(,)) = <expresion>}* in <expresion>
;;                      <letrec-exp proc-names idss bodies bodyletrec>
;;                  ::= imprimir ({<expresion>})
;;                      <print-exp>
;;                  ::= FNC <numero> (<clausula-or>)+("and")
;;<clausula-or>     ::= (<numero>)+("or")
;;<primitiva>       ::= + | - | * | % | / | add1 | sub1
;;<primitiva-hex>   ::= + | - | * | add1 | sub1
;;<primitiva-cad>   ::= lenght | concat
;;<primitiva-list>  ::= vacia | crear-lista | lista? | cabeza | cola | append
;;<primitica-vect>  ::= vector? | crear-vec | ref-vec | set-vec
;;<primitica-reg>   ::= registros? | crear-reg | ref-reg | set-reg
;;<lista>           ::= [{<expresion>}*(;)]
;;                      <lista (lexps)>
;;<vector>          ::= vector[{<expresion>}*(;)]
;;                      <vector (lexps)>
;;<registro>        ::= {{<identificador> = <expresion>}+(;)}
;;                      <registro (lids lexps)>
;;<expr-bool>       ::= <pred-prim> (<expresion> , <expresion>)
;;                      <name (pprim exp1 exp2)>
;;                  ::= <oper-bin-bool> (<expr-bool> , <expr-bool>)
;;                      <name (obbool expb expb)>
;;                  ::= <bool>
;;                      <name (bool)>
;;                  ::= <oper-un-bool> (<expr-bool>)
;;                      <name (oubool expb)>
;;<pred-prim>       ::= <|>|<=|>=|==|<>
;;<oper-bin-bool>   ::= and|or
;;<oper-un-bool>    ::= not

(define lexico
  '(
    (espacioblanco (whitespace) skip)
    (comentario ("#" (not "#/newline")) skip)
    (caracter ((or letter digit)) string)
    (alfanumerico (letter (arbno (or letter digit "?"))) string)
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
    (expresion ("'" caracter "'") caracter-exp)
    (expresion ("\"" alfanumerico "\"") cadena-exp)
    (expresion (identificador) identificador-exp)
    (expresion ("$" identificador) refid-exp)
    (expresion ("var" (separated-list identificador "=" expresion ",") "in" expresion)  var-exp)
    (expresion (identificador "->" expresion) asignar-exp)
    (expresion ("cons" (separated-list identificador "=" expresion ",") "in" expresion)  var-exp)
    (expresion ("rec" (arbno identificador "(" (separated-list identificador ",") ")" "=" expresion)  "in" expresion) 
                rec-exp)
    (expresion ("begin" expresion (arbno ";" expresion) "end") begin-exp)
    (expresion ("for" identificador "=" expresion to-o-downto expresion "do" expresion "done") for-exp)
    (to-o-downto ("to") to)
    (to-o-downto ("downto") downto)
    (expresion (primitiva "(" (separated-list expresion ",") ")") prim-exp)    
    (primitiva ("+") suma)
    (primitiva ("-") resta)
    (primitiva ("*") multiplicacion)
    (primitiva ("/") division)
    (primitiva ("%") modulo)
    (primitiva ("add1") add1)
    (primitiva ("sub1") sub1)
    ;;              ::= <bool>
;;                  ::= <bool-exp (bool)>;;                  
;;                  
;;                  ::= <lista>
;;                      <lista-exp (lista)>
;;                  ::= <vector>
;;                      <vector-exp (vector)>
;;                  ::= <registro>
;;                      <registro-exp (registro)>
;;                  ::= <exp-bool>
;;                  ::= <bool-exp (exp-bool)>
;;                  ::= if <expr-bool> then <expresion> [else <expresion> ] end
;;                      <if-exp (expb exp1 exp2)>
;;                  ::= while <expr-bool> do <expresion> done
;;                      <while-exp (expb exp)>
;;                  ::= <prim-hex> ({<expresion>}*(,))
;;                      <primhex-exp (lexp)>
;;                  ::= <primitiva-cad> ({<expresion>}*(,))
;;                      <primcad-exp (lexp)>
;;                  ::= <primitiva-list> ({<expresion>}*(,))
;;                      <primls-exp (lexp)>
;;                  ::= <primitica-vect> ({<expresion>}*(,))
;;                      <primvec-exp (lexp)>
;;                  ::= <primitica-reg> ({<expresion>}*(,))
;;                      <primreg-exp (lexp)>
;;                  ::= proc({<identificador>}*(,)) <expresion>
;;                      <proc-exp (ids body)>
;;                  ::= letrec  {<identificador> ({<identificador>}*(,)) = <expresion>}* in <expresion>
;;                      <letrec-exp proc-names idss bodies bodyletrec>
;;                  ::= imprimir ({<expresion>})
;;                      <print-exp>
;;                  ::= FNC <numero> (<clausula-or>)+("and")
;;<clausula-or>     ::= (<numero>)+("or")

;;<primitiva-hex>   ::= + | - | * | add1 | sub1
;;<primitiva-cad>   ::= lenght | concat
;;<primitiva-list>  ::= vacia | crear-lista | lista? | cabeza | cola | append
;;<primitica-vect>  ::= vector? | crear-vec | ref-vec | set-vec
;;<primitica-reg>   ::= registros? | crear-reg | ref-reg | set-reg
;;<lista>           ::= [{<expresion>}*(;)]
;;                      <lista (lexps)>
;;<vector>          ::= vector[{<expresion>}*(;)]
;;                      <vector (lexps)>
;;<registro>        ::= {{<identificador> = <expresion>}+(;)}
;;                      <registro (lids lexps)>
;;<expr-bool>       ::= <pred-prim> (<expresion> , <expresion>)
;;                      <name (pprim exp1 exp2)>
;;                  ::= <oper-bin-bool> (<expr-bool> , <expr-bool>)
;;                      <name (obbool expb expb)>
;;                  ::= <bool>
;;                      <name (bool)>
;;                  ::= <oper-un-bool> (<expr-bool>)
;;                      <name (oubool expb)>
;;<pred-prim>       ::= <|>|<=|>=|==|<>
;;<oper-bin-bool>   ::= and|or
;;<oper-un-bool>    ::= not
    )
  )


(sllgen:make-define-datatypes lexico gramatica)








