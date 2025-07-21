#lang play

(require "t1a.rkt")
(print-only-errors)

;; CC4101 Lenguajes de Programación
;; Cristóbal Stamm


#|    TESTS    |#

;;;;;;;;;;;;;;;;
;; parse-expr ;;
;;;;;;;;;;;;;;;;

; Basic types
(test (parse-expr '15) (num 15))
(test (parse-expr '-119) (num -119))
(test (parse-expr 'xy) (id 'xy))
(test (parse-expr '#f) (bool #f))
(test (parse-expr '#t) (bool #t))

; unary number primitives
(test (parse-expr '{add1 x}) (add1 (id 'x)))
(test (parse-expr '{add1 {+ x -2}}) (add1 (add (id 'x) (num -2))))
(test (parse-expr '{! #f}) (not (bool #f)))
(test (parse-expr '{! {< 1 2}}) (not (lt (num 1) (num 2))))

; binary number primitives
(test (parse-expr '{+ {add1 12} -2})
      (add (add1 (num 12)) (num -2)))
(test (parse-expr '{- x y})
      (sub (id 'x) (id 'y)))
(test (parse-expr '{- {+ 2 3} {+ z 2}})
      (sub (add (num 2) (num 3)) (add (id 'z) (num 2))))
(test (parse-expr '{add1 {add1 {+ var 2}}})
      (add1 (add1 (add (id 'var) (num 2)))))
(test (parse-expr '{< 8 15})
      (lt (num 8) (num 15)))
(test (parse-expr '{< x y})
      (lt (id 'x) (id 'y)))
(test (parse-expr '{= {+ 1 6} 3})
      (eq (add (num 1) (num 6)) (num 3)))
(test (parse-expr '{= a b})
      (eq (id 'a) (id 'b)))
(test (parse-expr '{&& {< 1 2} {= 9 3}})
      (and (lt (num 1) (num 2)) (eq (num 9) (num 3))))
(test (parse-expr '{|| {= x y} {! #f}})
      (or (eq (id 'x) (id 'y)) (not (bool #f))))

; if
(test (parse-expr '{if {< x 5} x 0})
      (if (lt (id 'x) (num 5))
          (id 'x)
          (num 0)))
(test (parse-expr '{if z {add1 x} {- y 2}})
      (if (id 'z)
          (add1 (id 'x))
          (sub (id 'y) (num 2))))

; with
(test (parse-expr '{with {{x 3}} {add1 x}})
      (with (list (binding (id 'x) (num 3)))
            (add1 (id 'x))))
(test (parse-expr '{with {{x 3}
                          {y {+ 1 2}}}
                     {if {= x y} x y}})
      (with (list (binding (id 'x) (num 3))
                  (binding (id 'y) (add (num 1) (num 2))))
            (if (eq (id 'x) (id 'y))
                (id 'x)
                (id 'y))))
(test
 (parse-expr '{with {{x 9} {y 10} {z 11}}
                    {+ x {+ y z}}})
 (with (list (binding (id 'x) (num 9))
             (binding (id 'y) (num 10))
             (binding (id 'z) (num 11)))
       (add (id 'x) (add (id 'y) (id 'z)))))

; Function applications with zero and more arguments.
(test (parse-expr '{func})
      (app 'func '()))
(test (parse-expr '{firstf {secondf {+ x 1}}})
      (app 'firstf (list (app 'secondf (list (add (id 'x) (num 1)))))))
(test (parse-expr '{add1 {f {! 1} (< 2 3)}})
      (add1 (app 'f (list (not (num 1)) (lt (num 2) (num 3))))))
(test (parse-expr '{sum3 {if (< 4 5) 1 0} {! {= x y}} {+ 7 8}})
      (app 'sum3 (list (if (lt (num 4) (num 5)) (num 1) (num 0))
                       (not (eq (id 'x) (id 'y)))
                       (add (num 7) (num 8)))))
(test (parse-expr '{mix {twice 2} {triple {+ a 1}}})
      (app 'mix (list (app 'twice (list (num 2)))
                      (app 'triple (list (add (id 'a) (num 1)))))))
(test (parse-expr '{fooz {barz 15} {bazz x} 145})
      (app 'fooz (list (app 'barz (list (num 15)))
                      (app 'bazz (list (id 'x)))
                      (num 145))))

; An expression that combines everything.
(test (parse-expr '{+ {- {fib 4} {add1 x}} {add1 {fact 5}}})
      (add (sub (app 'fib (list (num 4))) (add1 (id 'x))) (add1 (app 'fact (list (num 5))))))
(test (parse-expr'{if {< x 10} {with {{y {add1 x}}} {+ y {fact 5}}} {add1 {fib 4}}})
      (if (lt (id 'x) (num 10))
            (with (list (binding (id 'y) (add1 (id 'x))))
                  (add (id 'y) (app 'fact (list (num 5)))))
            (add1 (app 'fib (list (num 4))))))
(test (parse-expr '{if {&& {< 1 2} {|| {= x y} {! #f}}}
       {with {{a 5}
              {b {add1 a}}
              {c {+ b 3}}
              {d {- c 1}}}
             {add1 {my-func d #t}}}
       {+ 100 {- 50 25}}})
 (if (and (lt (num 1) (num 2)) (or (eq (id 'x) (id 'y)) (not (bool #f))))
     (with (list (binding (id 'a) (num 5))
                 (binding (id 'b) (add1 (id 'a)))
                 (binding (id 'c) (add (id 'b) (num 3)))
                 (binding (id 'd) (sub (id 'c) (num 1))))
           (add1 (app 'my-func (list (id 'd) (bool #t)))))
     (add (num 100) (sub (num 50) (num 25)))))

;;;;;;;;;;;;;;;;;;
;; parse-fundef ;;
;;;;;;;;;;;;;;;;;;

; Function with no parameters.
(test (parse-fundef '{define {bye} {- 10 4}})
      (fundef 'bye '() (sub (num 10) (num 4))))
(test (parse-fundef '{define {abbaa} {add1 5}})
      (fundef 'abbaa '() (add1 (num 5))))

; Functions with one or more parameters.
(test (parse-fundef '{define {add3 x} {add1 {add1 {add1 x}}}})
      (fundef 'add3 '(x) (add1 (add1 (add1 (id 'x))))))
(test (parse-fundef '{define {mult y} {+ y 2}})
      (fundef 'mult '(y) (add (id 'y) (num 2))))
(test (parse-fundef '{define {lo x y z} {+ x {- x {! z}}}})
      (fundef 'lo '(x y z) (add (id 'x) (sub (id 'x) (not (id 'z))))))
(test (parse-fundef '{define {>= x y} {! {< x y}}})
 (fundef '>= '(x y) (not (lt (id 'x) (id 'y)))))
(test (parse-fundef '{define {relu x} {if {>= x 0} x 0}})
 (fundef 'relu '(x) (if (app '>= (list (id 'x) (num 0))) (id 'x) (num 0))))
(test (parse-fundef '{define {in-range lo x hi} {&& {<= lo x} {< x hi}}})
 (fundef 'in-range'(lo x hi) (and (app '<= (list (id 'lo) (id 'x))) (lt (id 'x) (id 'hi)))))

;;;;;;;;;;;;;;;;
;; parse-prog ;;
;;;;;;;;;;;;;;;;

; Programs with no function definitions.
(test (parse-prog '{#f}) (prog '() (bool #f)))
(test (parse-prog '{voidbz}) (prog '() (id 'voidbz)))
(test
 (parse-prog '{
               {- 3 {! #f}}
               })
 (prog '() (sub (num 3) (not (bool #f)))))

; Programs with fundefs without parameters
(test
 (parse-prog '{
               {define {uno} 1}
               {+ 9 {uno}}
               })
 (prog (list (fundef 'uno '() (num 1)))
       (add (num 9) (app 'uno '()))))
(test
 (parse-prog '{
               {define {a} 2}
               {define {b} 3}
               {+ {a} {b}}
               })
 (prog
  (list (fundef 'a '() (num 2))
        (fundef 'b '() (num 3)))
  (add (app 'a '()) (app 'b '()))))

; Programs with fundef with parameters
(test
 (parse-prog '{
               {define {masgrande? x y} {< x y}}
               {define {decide a b} {if a b #f}}
               {decide {masgrande? 2 1} 42}
               })
 (prog
  (list (fundef 'masgrande? '(x y) (lt (id 'x) (id 'y)))
        (fundef 'decide '(a b) (if (id 'a) (id 'b) (bool #f))))
  (app 'decide (list (app 'masgrande? (list (num 2) (num 1))) (num 42)))))
(test
 (parse-prog '{
               {define {triple x} {+ x {+ x x}}}
               {define {add2 x} {+ 2 x}}
               {add2 {triple 2}}
               })
 (prog
  (list (fundef 'triple '(x) (add (id 'x) (add (id 'x) (id 'x))))
        (fundef 'add2 '(x) (add (num 2) (id 'x))))
  (app 'add2 (list (app 'triple (list (num 2)))))))

(test (parse-prog '{
                     {define {f x} {+ x 1}}
                     {with {{y 2}} {f y}}
                   })
      (prog (list (fundef 'f '(x) (add (id 'x) (num 1))))
            (with (list (binding (id 'y) (num 2)))
                  (app 'f (list (id 'y))))))


;;;;;;;;;;;;;;;;;;;
;; parse-binding ;;
;;;;;;;;;;;;;;;;;;;

; simple bindings
(test (parse-binding '(x 113))
      (binding (id 'x) (num 113)))
(test (parse-binding '(y {add1 2}))
      (binding (id 'y) (add1 (num 2))))
(test (parse-binding '(z {+ 14 {- 4 21}}))
      (binding (id 'z) (add (num 14) (sub (num 4) (num 21)))))
(test (parse-binding '(pap {abba x y}))
      (binding (id 'pap) (app 'abba (list (id 'x) (id 'y)))))
(test (parse-binding '(hxh {if {< x 10} x 0})) 
      (binding (id 'hxh) (if (lt (id 'x) (num 10)) (id 'x) (num 0))))
(test (parse-binding '(ok {&& {< x y} {= x 5}}))
 (binding (id 'ok) (and (lt (id 'x) (id 'y)) (eq (id 'x) (num 5)))))

; binding inside binding
(test (parse-binding '(result {with {{a {with {{b 2}} b}}} a}))
      (binding (id 'result)
               (with (list (binding (id 'a)
                        (with (list (binding (id 'b) (num 2))) (id 'b)))) (id 'a))))

; A binding that combines every expresion.
(test (parse-binding '(all {if {< x 5} {with {{a 1}} {add1 {+ a 2}}} {|| #f {= x y}}}))
 (binding (id 'all) (if (lt (id 'x) (num 5))
      (with (list (binding (id 'a) (num 1)))
            (add1 (add (id 'a) (num 2))))
      (or (bool #f)
          (eq (id 'x) (id 'y))))))
