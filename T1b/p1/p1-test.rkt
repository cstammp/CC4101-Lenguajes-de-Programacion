#lang play
(require "p1.rkt")
(require "env.rkt")
(print-only-errors)

;; CC4101 Lenguajes de Programación
;; Cristóbal Stamm

#|    TESTS    |#

;; Literales
(test (run '{43}) (numV 43))
(test (run '{6}) (numV 6))
(test (run '{#f}) (boolV #f))
(test (run '{#t}) (boolV #t))
(test (run '{{cons 1 {cons 2 3}}}) (pairV (numV 1) (pairV (numV 2) (numV 3))))
(test (run '{{cons 1 #t}}) (pairV (numV 1) (boolV #t)))

;; Operaciones
(test (run '{{add1 9}}) (numV 10))
(test (run '{{+ 7 8}}) (numV 15))
(test (run '{{- 20 5}}) (numV 15))
(test (run '{{< 3 5}}) (boolV #t))
(test (run '{{= 7 8}}) (boolV #f))
(test (run '{{! #t}}) (boolV #f))
(test (run '{{&& #f #f}}) (boolV #f))
(test (run '{{|| #t #f}}) (boolV #t))


;; fst y snd
(test (run '{{fst {cons 10 20}}}) (numV 10))
(test (run '{{snd {cons 10 20}}}) (numV 20))

;; if
(test (run '{{if #t 1 2}}) (numV 1))
(test (run '{{if #f 1 2}}) (numV 2))

;; with
(test (run '{{with {{x 3} {y 4}} {+ x y}}}) (numV 7))
(test (run '{{with {{x 1}} {with {{y 2}} {+ x y}}}}) (numV 3))

;; Aplicación de funciones
(test (run '{
             {define {square x} {* x x}}
             {define {sum3 a b c} {+ a {+ b c}}}
             {sum3 1 2 3}
             })
      (numV 6))

(test (run '{
             {define {sum x y z} {+ x {+ y z}}}
             {define {cadr x} {fst {snd x}}}
             {with {{x 9} {y {cons 1 {cons 3 4}}}}
                   {sum x {fst y} {cadr y}} }
             })
      (numV 13))

(test (run '{
             {with {{x 5} {y 23} {z {cons 11 -3}}}
                   z}
             })
      (pairV (numV 11) (numV -3)))

(test (run '{
             {define {triple x} {+ x {+ x x}}}
             {define {add2 x} {+ 2 x}}
             {add2 {triple 2}}
             })
      (numV 8))

(test (run '{
             {with {{x 3}
                    {y {+ 1 2}}}
                   {if {= x y} x y}}
             })
      (numV 3))

(test (run '{
             {define {max a b}
               {if {< a b} b a}}
             {define {min a b}
               {if {< a b} a b}}
             {with {{x 7} {y 12}}
               {+ {max x y} {min x y}}}
           })
      (numV 19))

;; test/exn

;;  Identificador libre (alcance léxico).
(test/exn
 (run '{
   {define {f x} {+ x n}}
   {with {{n 10}}
     {f 5}}})
 "env-lookup: free identifier: n")

;; Error de tipo: add1 con booleano
(test/exn (run '{{add1 #t}}) "Runtime type error: expected Number")

;; Error de tipo: suma con booleano
(test/exn (run '{{+ 1 #f}}) "Runtime type error: expected Number")

;; Error de tipo: comparación con booleano
(test/exn (run '{{< 1 #t}}) "Runtime type error: expected Number")

;; Error de tipo: eq con booleano
(test/exn (run '{{= 1 #t}}) "Runtime type error: expected Number")

;; Error de tipo: ! con número
(test/exn (run '{{! 1}}) "Runtime type error: expected Boolean")

;; Error de tipo: && con número
(test/exn (run '{{&& 1 #t}}) "Runtime type error: expected Boolean")

;; Error de tipo: || con número
(test/exn (run '{{|| 1 #t}}) "Runtime type error: expected Boolean")

;; Error de tipo: fst con número
(test/exn (run '{{fst 5}}) "Runtime type error: expected Pair")

;; Error de tipo: snd con booleano
(test/exn (run '{{snd #f}}) "Runtime type error: expected Pair")

;; Error de tipo: if cond no booleana
(test/exn (run '{{if 0 1 2}}) "Runtime type error: expected Boolean")

;; Error de aridad
(test/exn (run '{
                  {define {sum2 x y} {+ x y}}
                  {sum2 1}
                 }) "Arity mismatch: function bar expected 2 arguments, received 1")

;; Tests Env

;; Test env-lookup.
(test (env-lookup 'x (extend-env 'x (numV 10) empty-env))
      (numV 10))
(define long-env
  (extend-env 'x (numV 1)
    (extend-env 'y (boolV #t)
      (extend-env 'z (pairV (numV 2) (numV 3))
        empty-env))))

(test (env-lookup 'x long-env) (numV 1))
(test (env-lookup 'y long-env) (boolV #t))
(test (env-lookup 'z long-env) (pairV (numV 2) (numV 3)))

;; Error si no existe la variable
(test/exn (env-lookup 'a long-env)
          "free identifier: a")

;; env-lookup en ambiente vacío.
(test/exn (env-lookup 'x empty-env)
          "free identifier: x")

;; env-lookup con shadowing.
(test (env-lookup 'x (extend-env 'x (numV 20)
                         (extend-env 'x (numV 10) empty-env)))
      (numV 20))