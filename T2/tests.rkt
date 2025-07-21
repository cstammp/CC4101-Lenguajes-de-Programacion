#lang play
(require "surface-base.rkt")
(require "core-base.rkt")
(require "env.rkt")
(print-only-errors #t)

;; CC4101 Lenguajes de Programación
;; Cristóbal Stamm

#|    TESTS    |#

;; ========================================================= ;;

;;;;;;;;;;;;;;;;
;;     p1     ;;
;;;;;;;;;;;;;;;;

;; Tests print-log
(test (print-log (box '(3 2 1))) '(1 2 3))
(test (print-log (box '())) '())
(test (print-log (box '(#f))) '(#f))

;; Tests interp-p
(test (interp-p (parse-cl '{with {addn {fun {n}
                                {fun {m}
                                     {+ n m}}}}
                     {{addn 10} 4}}))
      (result (numV 14) '()))

(test
 (interp-p (parse-cl '{
   with {f {fun {x} {+ {printn x} x}}}
     {+ {f 3} {f 4}}}))
 (result (numV 14) '(3 4)))

(test
 (interp-p (parse-cl '{
   with {f {fun {x} {+ {printn x} x}}}
     {+ {f 3} {f 3}}}))
 (result (numV 12) '(3 3)))

(test
 (interp-p (parse-cl '{
   with {x 10}
     {with {y {printn x}}
       {+ x y}}}))
 (result (numV 20) '(10)))

;; ========================================================= ;;

;;;;;;;;;;;;;;;;
;;     p2     ;;
;;;;;;;;;;;;;;;;

;; Tests mfun
(test (interp-p (parse-cl '{with {f {mfun {x}
                        {+ {printn 10} 100}}}
                     {+ {f 0} {f 0}}}))
     (result (numV 220) '(10)))

(test (interp-p (parse-cl '{with {f {mfun {x}
                    {+ {printn 10} 100}}}
                {f 0}}))
      (result (numV 110) '(10)))

(test (interp-p (parse-cl
  '{with {f {mfun {x} {+ {printn x} x}}}
         {+ {+ {f 5} {f 5}} {f 5}}}))
      (result (numV 30) '(5)))

;; ========================================================= ;;

;;;;;;;;;;;;;;;;
;;     p3     ;;
;;;;;;;;;;;;;;;;

;; Test parse-type
(test (parse-type '({lazy Num} -> Num)) (TFun 'lazy (TNum) 'eager (TNum)))
(test (parse-type '({name Num} -> Num)) (TFun 'name (TNum) 'eager (TNum)))
(test (parse-type '(Num -> Num)) (TFun 'eager (TNum) 'eager (TNum)))

;; test type-mod
(test (type-mod (parse-type '({lazy Num} -> Num))) 'lazy)
(test (type-mod (parse-type '({name Num} -> Num))) 'name)
(test (type-mod (parse-type '(Num -> Num))) 'eager)

;; test type->str
(test (type->str (parse-type '({name Num} -> Num))) "{name Num -> eager Num}")

;; Test compatible?
(test (compatible? 
          (TFun 'lazy (TNum) 'eager (TNum))   ; {lazy Num} -> Num
          (TFun 'eager (TNum) 'eager (TNum))) ; Num -> Num
      #false)

(test (compatible? 
          (TFun 'lazy 
                (TFun 'eager (TNum) 'eager (TNum)) ; {Num -> Num}
                'lazy 
                (TNum))
          (TFun 'eager 
                (TFun 'eager (TNum) 'eager (TNum)) ; Num -> Num
                'eager 
                (TNum)))
      #true)

(test (compatible? 
          (TFun 'lazy 
                (TFun 'eager (TNum) 'eager (TNum)) ; {Num -> Num}
                'lazy 
                (TNum))
          (TFun 'name 
                (TFun 'lazy (TNum) 'eager (TNum)) ; Num -> Num
                'name 
                (TNum)))
      #false)

;; Test parse-sl
(test (parse-sl '{with {f {fun {x : Num} -> Num : {+ {printn x} x}}}
       {+ {f 3} {f 4}}})
     (swith #f 'f (sfun (TFun 'eager (TNum) 'eager (TNum)) 'x (sadd #f (sprintn #f (sid #f 'x)) (sid #f 'x))) (sadd #f (sapp #f (sid #f 'f) (snum #f 3)) (sapp #f (sid #f 'f) (snum #f 4))))
)

(test (parse-sl '{with {f {fun {x : (lazy Num)} -> Num : {+ {printn x} x}}}
       {+ {f 3} {f 4}}})
     (swith #f 'f (sfun (TFun 'lazy (TNum) 'eager (TNum)) 'x (sadd #f (sprintn #f (sid #f 'x)) (sid #f 'x))) (sadd #f (sapp #f (sid #f 'f) (snum #f 3)) (sapp #f (sid #f 'f) (snum #f 4))))
)

;; Test type-ast
(test (type-ast (parse-sl '{with {f {fun {x : (lazy Num)} -> Num : {+ {printn x} x}}}
       {+ {f 3} {f 4}}}) empty-env)
     (swith (TNum) 'f (sfun (TFun 'lazy (TNum) 'eager (TNum)) 'x (sadd (TNum) (sprintn (TNum) (sid (TNum) 'x)) (sid (TNum) 'x))) (sadd (TNum) (sapp (TNum) (sid (TFun 'lazy (TNum) 'eager (TNum)) 'f) (snum (TNum) 
3)) (sapp (TNum) (sid (TFun 'lazy (TNum) 'eager (TNum)) 'f) (snum (TNum) 4)))))