#lang play
(require "main.rkt")
(print-only-errors #t)

;; CC4101 Lenguajes de Programación
;; Cristóbal Stamm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                 TESTS BASE                                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test (run-val '{+ 1 2}) 3)
(test (run-val '{< 1 2}) #t)
(test (run-val '{- 2 1}) 1)
(test (run-val '{* 2 3}) 6)
(test (run-val '{= {+ 2 1} {- 4 1}}) #t)
(test (run-val '{and #t #f}) #f)
(test (run-val '{or #t #f}) #t)
(test (run-val '{not {not #t}}) #t)
(test (run-val '{if {not #f} {+ 2 1} 4}) 3)
(test (run-val '{with {{x 5} {y 3}}
                      {begin {+ x 1}
                             {+ x y}}}) 8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                TESTS ENUNCIADO                              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test/exn (run-val '{begin {with {{A {class {x y x}}}}
                     10}
                   {new A}})
            "error: duplicate fields")

(test/exn (run-val '{begin {with {{A {class {x}
                               {def init {init-x} {set x init-x}}
                               {def init {init-x} {set x 12}}}}}
                     10}
                   {new A}})
            "error: same arity constructor")

(test/exn (run-val '{begin {with {{A {class {x}
                               {def foo {a} {set x a}}
                               {def foo {b} {set x {+ b 1}}}}}}
                     10}
                   {new A}})
            "error: overloading method foo with the same arity")

(test/exn (run-val 'self)
            "error: self outside of method")

(test/exn (run-val '{with {{x 10}
                   {A {class {x}}}
                   {o {new A x}}}
              1})
            "error: constructor not found")

(test/exn (run-val '{with {{C {class {x}
                             {def init {init-x} {set x init-x}}}}}
                  {new C 1 2}})
                  "error: constructor not found")

(test/exn (run-val '{with {{A {class {}}}
                   {o {new A}}}
              {-> o m}})
            "error: method m not found")

(test/exn (run-val '{with {{A {class {x}
                        {def set-x {val-x} {set x val-x}}}}
                   {o {new A}}}
              {-> o set-x 10 20}})
            "error: no overload of 2 arguments was found for method set-x")

(test/exn (run-val '{set x 1})
            "error: set outside of method")

(test/exn (run-val '{with {{A {class {}}}
                   {o {new A}}}
              {get o z}})
            "error: field z not found")

(test/exn (run-val '{with {{A {class {x y} {def init {init-x init-y} {set x init-x}}}}
                   {o {new A 1 2}}}
              {get o y}})
            "error: field y not initialized")

(test (run-val '{with {{c {class {x y}
                              {def init {}
                                {begin {set x 1} {set y 2}}}
                              {def init {init-x init-y}
                                {begin {set x init-x} {set y init-y}}}  
                              {def sum {z} {+ {get self x} {+ {get self y} z}}}
                              {def set-x {val} {set x val}}}}
                         {o {new c 3 4}}}
                        {begin
                          {-> o set-x {+ 1 3}}
                          {+ {-> o sum 3} {get o y}}}})
        15)

(test (run-val '{with {{A {class {}
                              {def apply {c} {-> {new c} m}}}}
                         {o {new A}}}
                        {-> o apply {class {x}
                                      {def init {} {set x 2}}
                                      {def m {} {get self x}}}}})
        2)

(test (run-val '{with {{x 10}
                   {A {class {}
                        {def m {y} {+ x y}}}}
                   {o {new A}}}
              {-> o m 1}})
        11)

(test/exn (run-val '{begin {with {{A {class {x}
                               {def init {} {set x 2}}
                               {def init {init-x} {set x init-x}}
                               {def m {} {get self x}}}}}
                     10}
                   {new A}})
            "free identifier: A")

(test (run-val '{{fun {x} {+ x 1}} 2})
            3)

(test (run-val '{with {{add1 {fun {x} {+ x 1}}}}
                  {+ {add1 2} {add1 4}}})
              8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                  SUS TESTS                                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Creación de clases (new) y objetos
(test (run-val '{with {{A {class {}}}} 
                  {new A}})
        (objV (classV '() '()) (make-hash)))

; Método constructor sin args
(test (run-val '{with {{C {class {x y}
                             {def init {} {begin {set y 10} {set x 25}}}}}
                       {o {new C}}}
                {- {+ {get o x} {get o y}} 20}})
      15)

; Método constructor con args
(test (run-val '{with {{C {class {x y}
                             {def init {c d} {begin {set y c} {set x d}}}}}
                       {o {new C 10 20}}}
                {+ {- {get o x} {get o y}} 20}})
      30)

; Llamados a Métodos
(test (run-val '{with {{C {class {x}
                             {def init {n} {set x n}}
                             {def double {} {+ {get self x} {get self x}}}}}
                       {o {new C 5}}}
                {-> o double}})
      10)

; Self
(test (run-val '{with {{C {class {x}
                             {def init {n} {set x n}}
                             {def triple {} {+ {+ {get self x} {get self x}} {get self x}}}}}
                       {o {new C 5}}}
                {-> o triple}})
      15)

; Get y Set
(test (run-val '{with {{C {class {x}
                             {def init {n} {set x n}}
                             {def set-x {a} {set x a}}
                             {def double {} {+ {get self x} {get self x}}}}}
                       {o {new C 5}}}
                {begin 
                    {-> o set-x 25}
                    {-> o double}}})
      50)

; Constructor con distinta aridad
(test (run-val '{with {{C {class {x}
                             {def init {} {set x 10}}
                             {def init {n} {set x n}}}}
                       {o {new C}}}
                {get o x}})
      10)

(test (run-val '{with {{C {class {x}
                             {def init {} {set x 10}}
                             {def init {n} {set x n}}}}
                       {o {new C 30}}}
                {get o x}})
      30)

; Metodo con distinta aridad
(test (run-val '{with {{C {class {x}
                             {def init {} {set x 10}}
                             {def profepongameun7 {n} {set x n}}
                             {def profepongameun7 {a c} {set x {+ a c}}}}}
                       {o {new C}}}
                {begin {-> o profepongameun7 55}
                       {get o x}
                }})
      55)

(test (run-val '{with {{C {class {x}
                             {def init {} {set x 10}}
                             {def profepongameun7 {n} {set x n}}
                             {def profepongameun7 {a c} {set x {+ a c}}}}}
                       {o {new C}}}
                {begin {-> o profepongameun7 2 5}
                       {get o x}
                }})
      7)

; Lambdas
(test (run-val '{with {{triple {fun {a} {+ a {+ a a}}}}}
                  {triple 3}})
      9)

(test (run-val '{with {{fsum-sqr {fun {n}
                                   {with {{sum-sqr {fun {i acc}
                                                     {if {< i 1}
                                                          acc
                                                          {sum-sqr {- i 1} {+ acc {* i i}}}}}}}
                                         {sum-sqr n 0}}}}}
                      {fsum-sqr 5}})
      55)

; Errores

(test/exn (run-val '{with {{triple {fun {a} {+ a {+ a a}}}}}
                  {add 3}})
      "env-lookup: free identifier: add")

(test/exn (run-val '{with {{C {class {x}
                               {def init {} {set x 5}}
                               {def get-x {} {get self y}}}}
                          {o {new C}}}
                    {-> o get-x}})
          "error: field y not found")

(test/exn (run-val '{with {{C {class {x y}
                               {def init {a} {set x a}}}}
                          {o {new C 5}}}
                    {get o y}})
          "error: field y not initialized")

(test/exn (run-val '{with {{C {class {} {def met {} 1}}}
                          {o {new C}}}
                    {-> o n}})
          "error: method n not found")

(test/exn (run-val '{with {{C {class {x}
                               {def init {a} {set x a}}
                               {def init {b} {set x b}}}}}
                    {new C 5}})
          "error: same arity constructor")

(test/exn (run-val '{with {{C {class {}
                               {def bombastik {x} x}}}
                          {o {new C}}}
                    {-> o bombastik}})
          "error: no overload of 0 arguments was found for method bombastik")

(test/exn (run-val '{with {{C {class {}
                               {def foo {x} x}
                               {def foo {x y} {+ x y}}}}
                          {o {new C}}}
                    {-> o foo 1 2 3}})
          "error: no overload of 3 arguments was found for method foo")

(test/exn (run-val '{with {{x 5}} self})
          "error: self outside of method")
  
(test/exn (run-val '{with {{C {class {x x}
                             {def init {} {set x 10}}
                             {def profepongameun7 {n} {set x n}}
                             {def profepongameun7 {a c} {set x {+ a c}}}}}
                       {o {new C}}}
                {begin {-> o profepongameun7 2 5}
                       {get o x}
                }})
      "error: duplicate fields")

(test/exn (run-val '{with {{C {class {x y}
                               {def init {x} {set x x}}}}}
                    {new C 1 2}})
          "error: constructor not found")

; Test funciones auxiliares

; repeated?
(test (repeated? '()) #f)
(test (repeated? '(a b c)) #f)
(test (repeated? '(1 2 3 2 4)) 2)
(test (repeated? '((a 1) (b 2) (a 1))) '(a 1))

; findl
(test (findl even? '(1 3 4 6)) 4)
(test (findl (lambda (x) (> x 10)) '(1 5 9 12)) 12)

; exists-name?
(define test-methds
  (list (method 'foo '(x) 1 (num 1))
        (method 'bar '(y) 1 (num 2))))

(test (exists-name? 'foo test-methds) #t)
(test (exists-name? 'bar test-methds) #t)
(test (exists-name? 'aaa test-methds) #f)

; init-zero?
(define class-init-zero
  (classV '(x) (list
                 (method 'init '() 0 (num 1))
                 (method 'm '(x) 1 (num 2)))))

(define class-normal
  (classV '(x) (list
                 (method 'init '(x) 1 (num 1))
                 (method 'm '(y) 1 (num 2)))))

(test (init-zero? (objV class-init-zero (make-hash))) #t)
(test (init-zero?(objV class-normal (make-hash))) #f)

