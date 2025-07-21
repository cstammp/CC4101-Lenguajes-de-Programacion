#lang play
(print-only-errors)

;; CC4101 Lenguajes de Programación
;; Cristóbal Stamm

#|
=====================================
Gramática BNF del lenguaje extendido:

<prog>   ::= {<fundef>* <expr>}

<fundef> ::= {define {<id> <id>*} <expr>}

<expr> ::= <num>
           | <id>
           | {add1 <expr>}
           | {+ <expr> <expr>}
           | {- <expr> <expr>}
           | {<id> <expr>*}
           | <bool>
           | {< <expr> <expr>}
           | {= <expr> <expr>}
           | {! <expr>}
           | {&& <expr> <expr>}
           | {|| <expr> <expr>}
           | {if <expr> <expr> <expr>}
           | {with {<binding>*} <expr>}

<binding> ::= {<id> <expr>}

=====================================
|#

; Definiciones de los Tipos de Datos

(deftype Fundef
  (fundef name args body))

(deftype Expr
  (num n)
  (id s)
  (bool b)
  (add1 e)
  (add l r)
  (sub l r)
  (app name args)
  (lt l r)
  (eq l r)
  (not x)
  (and l r)
  (or l r)
  (if c l r)
  (with binds expr))

(deftype Prog
  (prog funs body))

(deftype Binding
  (binding id expr))

; Parser

; parse-fundef :: s-expr -> Fundef
; Parses a function definition into a Fundef structure.
(define (parse-fundef src)
  (match src
    [(list 'define (list name) e) (fundef name '() (parse-expr e))]
    [(list 'define (cons name args) e) (fundef name args (parse-expr e))]
    [_ (error 'parse-fundef "Expresión invalida: ~a" src)]
    ))


; parse-expr :: s-expr -> Expr
; Parses an symbolic-expression into an Expr structure.
(define (parse-expr src)
  (match src
    [(? number?) (num src)]
    [(? symbol?) (id src)]
    [(? boolean?) (bool src)]
    [(list 'add1 e) (add1 (parse-expr e))]
    [(list '+ l r) (add (parse-expr l) (parse-expr r))]
    [(list '- l r) (sub (parse-expr l) (parse-expr r))]
    [(list '< l r) (lt (parse-expr l) (parse-expr r))]
    [(list '= l r) (eq (parse-expr l) (parse-expr r))]
    [(list '! e) (not (parse-expr e))]
    [(list '&& l r) (and (parse-expr l) (parse-expr r))]
    [(list '|| l r) (or (parse-expr l) (parse-expr r))]
    [(list 'if c l r) (if (parse-expr c) (parse-expr l) (parse-expr r))]
    [(list 'with binds expr) (with (map parse-binding binds) (parse-expr expr))]
    [(cons func args) (app func (map parse-expr args))]
    [_ (error 'parse-expr "Expresión invalida: ~a" src)]
    ))


; parse-prog :: s-expr -> Prog
; Parses a list of optional function definitions into a Prog structure
(define (parse-prog src)
  (match src
    [(list e) (prog '() (parse-expr e))]
    [(list defs ... e) (prog (map parse-fundef defs) (parse-expr e))]
    [_ (error 'parse-prog "Expresión invalida: ~a" src)]
    ))


; parse-binding :: s-expr -> Binding
; Parses a binding of the form (id expr) into a Binding structure.
(define (parse-binding src)
  (match src
    [(list i e) (binding (parse-expr i) (parse-expr e))]
    [_ (error 'parse-binding "Expresión invalida: ~a" src)]
    ))