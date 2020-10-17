#lang eopl
;;<BSAT>      ::= <expresion>
;;                <bsat-exp (exp)>

;;<expresion> ::= <numero>
;;                <numero-exp (datum)>
;;            ::= <caracter>
;;                <caracter-exp (caracter)>
;;            ::= <cadena>
;;                <cadena-exp (cadena)>
;;            ::= <bool>
;;            ::= <bool-exp (bool)>
;;            ::= <identificador>
;;                <identificador-exp (id)>
;;            ::= var {<identificador> = <expresion>}*(,)
;;                <var-exp (ids exps)> in <expresion>
;;            ::= cons {<identificador> = <expresion>}*(,)
;;                <cons-exp (ids exps)> in <expresion>
;;            ::= rec  {<identificador> ({<identificador>}*(,)) = <expresion>}* in <expresion>
;;                <rec-exp (proc ids cuerpos cuerporec)>
;;            ::= <lista>
;;                <lista-exp (lista)>
;;            ::= <vector>
;;                <vector-exp (vector)>
;;            ::= <registro>
;;                <registro-exp (registro)>
;;            ::= <exp-bool>
;;            ::= <bool-exp (exp-bool)>
;;<lista>     ::= [{<expresion>}*(;)]
;;                <lista (lexps)>
;;<vector>    ::= vector[{<expresion>}*(;)]
;;                <vector (lexps)>
;;<registro>  ::= {{<identificador> = <expresion>}+(;)}
