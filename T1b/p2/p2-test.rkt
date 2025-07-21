#lang play
(print-only-errors)
(require "p2.rkt")

;; CC4101 Lenguajes de Programación
;; Cristóbal Stamm

#|    TESTS    |#

;; Test typecheck

;; Literales

(test (typecheck (parse-prog '{3})) (numT))
(test (typecheck (parse-prog '{#f})) (boolT))
(test (typecheck (parse-prog '{{cons 1 2}})) (pairT (numT) (numT)))
(test (typecheck (parse-prog '{{cons 42 #t}})) (pairT (numT) (boolT)))

;; id
(test (typecheck (parse-prog '{
                                {with {{x : Num 10}}
                                      x}
                  })) (numT))
(test (typecheck (parse-prog '{
                                {with {{x : Bool #f}}
                                      x}
                  })) (boolT))
(test (typecheck (parse-prog '{
                                {with {{x : {Pair Num Bool} {cons 42 #f}}}
                                      x}
                              })) (pairT (numT) (boolT)))

;; operaciones

(test (typecheck (parse-prog '{
  {add1 41}
})) (numT))
(test (typecheck (parse-prog '{
  {+ 1 2}
})) (numT))
(test (typecheck (parse-prog '{
  {- 5 3}
})) (numT))
(test (typecheck (parse-prog '{
  {< 3 4}
})) (boolT))
(test (typecheck (parse-prog '{
  {= 3 3}
})) (boolT))
(test (typecheck (parse-prog '{
  {! #t}
})) (boolT))
(test (typecheck (parse-prog '{
  {&& #t #f}
})) (boolT))
(test (typecheck (parse-prog '{
  {|| #t #f}
})) (boolT))
(test (typecheck (parse-prog '{
  {fst {cons 1 2}}
})) (numT))
(test (typecheck (parse-prog '{
  {snd {cons 1 #t}}
})) (boolT))

;; if

(test (typecheck (parse-prog '{
  {if #t 1 2}
})) (numT))
(test (typecheck (parse-prog '{
  {define {chooser {b : Bool} {x : Num} {y : Num}} : Num {if b x y}}
  {chooser #t 1 2}
})) (numT))

;; with con y sin tipos explícitos
(test (typecheck (parse-prog '{
  {with {{x : Num 5}} x}
})) (numT))

(test (typecheck (parse-prog '{
  {with {{x : Num 5} {y : Num 10}}
        {+ x y}}
})) (numT))

(test (typecheck (parse-prog '{
  {with {{x 5}}
        {with {{y : Num {+ x 1}}}
              {- x y}}
  }
})) (numT))


;; Prog con argumentos con tipos explicitos

(test (typecheck (parse-prog '{
  {define {f {x : Num}} : Num {+ x 1}}
  {f 5}
})) (numT))

(test  (typecheck (parse-prog '{{define {f {p : Bool}} : Num {if p 23 42}}
                                {f {< 3 4}}}))
       (numT))

(test (typecheck (parse-prog '{
  {define {add-pair {p : {Pair Num Num}} {x : Num}} : {Pair Num Num}
    {cons {+ {fst p} x} {+ {snd p} x}}}
  {add-pair {cons 1 1} 1}
})) (pairT (numT) (numT)))

(test (typecheck (parse-prog '{
                               {with {{x : Num 5} {y : Num 10}}
                                     {+ x y}}
                               }))
      (numT))

(test (typecheck (parse-prog '{
                               {with {{x 5}}
                                     {with {{y : Num {+ x 1}}}
                                           {+ x y}}
                                     }}))
      (numT))

(test (typecheck (parse-prog '{
                               {define {add-pair {p : {Pair Num Num}} {x : Num}} : {Pair Num Num}
                                 {cons {+ {fst p} x} {+ {snd p} x}}}
                               {add-pair {cons 1 1} 1}
                               }))
      (pairT (numT) (numT)))

(test (typecheck (parse-prog '{
                               {define {id {x : Num}} : Num x}
                               {id 5}
                               }))
      (numT))

(test (typecheck (parse-prog '{
                               {define {sum {x : Num} {y : Num} {z : Num}} : Num
                                 {+ x {+ y z}}}
                               {define {cadr {x : {Pair Num {Pair Num Num}}}} : Num
                                 {fst {snd x}}}
                               {with {{x 9} {y {cons 1 {cons 3 4}}}}
                                     {sum x {fst y} {cadr y}} }
                               }))
      (numT))

(test (typecheck (parse-prog '{
  {define {sum {x : Num} {y : Num} {z : Num}} : Num
    {+ x {+ y z}}}
  {define {cadr {x : {Pair Num {Pair Num Num}}}} : Num
    {fst {snd x}}}
  {with {{x 9} {y {cons 1 {cons 3 4}}}}
        {sum x {fst y} {cadr y}}}
})) (numT))

;; Test Run

(test (run '{
  {define {triple {x : Num}} : Num {+ x {+ x x}}}
  {define {add2 {x : Num}} : Num {+ 2 x}}
  {add2 {triple 2}}
}) (numV 8))

(test (run '{
  {define {max {a : Num} {b : Num}} : Num
    {if {< a b} b a}}
  {define {min {a : Num} {b : Num}} : Num
    {if {< a b} a b}}
  {with {{x 7} {y 12}}
    {+ {max x y} {min x y}}}
}) (numV 19))

(test (run '{
  {with {{x 3}
         {y {+ 1 2}}}
    {if {= x y} x y}}
}) (numV 3))

;; test/exn

;; Literales
(test/exn
  (run '{
    {with {{x : Num #t}} x}
  })
  "Static type error: expected Num found Bool")

;; Operadores

(test/exn
  (run '{{add1 #f}})
  "Static type error: expected Num found Bool")

(test/exn
  (run '{{+ 1 #f}})
  "Static type error: operator + expected Num found Bool")

(test/exn
  (run '{{- 10 #t}})
  "Static type error: operator - expected Num found Bool")

(test/exn
  (run '{{< 1 #t}})
  "Static type error: operator < expected Num found Bool")

(test/exn
  (run '{{= 1 #t}})
  "Static type error: operator = expected Num found Bool")

(test/exn
  (run '{{! 1}})
  "Static type error: operator ! expected Bool found Num")

(test/exn
  (run '{{&& #t 1}})
  "Static type error: operator && expected Bool found Num")

(test/exn
  (run '{{|| #f 0}})
  "Static type error: operator || expected Bool found Num")

(test/exn
  (run '{{fst 1}})
  "Static type error: expected Pair found Num")

(test/exn
  (run '{{snd #t}})
  "Static type error: expected Pair found Bool")

;; if

(test/exn
  (run '{{if 42 1 2}})
  "Static type error: expected Bool found Num")

(test/exn 
  (run '{{if 73 #t #t}})
  "Static type error: expected Bool found Num")

(test/exn
  (run '{{if #t 1 #f}})
  "las ramas del if deben tener el mismo tipo")

;; Prog

(test/exn
  (run '{
    {define {f {x : Num}} : Num {+ x 1}}
    {f 1 2}
  })
  "Arity mismatch: function bar expected 1 arguments, received 2")

(test/exn
  (run '{
    {define {f {x : Num}} : Num {+ x 1}}
    {f #f}
  })
  "Static type error: expected Num found Bool")

(test/exn
  (run '{
    {define {f {x : Num}} : Bool x}
    {f 10}
  })
  "Static type error in function f: expected Bool found Num")
