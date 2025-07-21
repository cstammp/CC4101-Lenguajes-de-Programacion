#lang play

(require "env.rkt")

;; CC4101 Lenguajes de Programación
;; Cristóbal Stamm

;; Parte 1

#|

Gramática BNF de la sintaxis concreta del lenguaje.

<prog>   ::= {<fundef>* <expr>}

<fundef> ::= {define {<id> <id>*} <expr>}

<expr>   ::= <num>
           | <id>
           | <bool>
           | {cons <expr> <expr>}
           | {add1 <expr>}
           | {+ <expr> <expr>}
           | {- <expr> <expr>}
           | {< <expr> <expr>}
           | {= <expr> <expr>}
           | {! <expr> <expr>}
           | {&& <expr> <expr>}
           | {|| <expr> <expr>}
           | {fst <expr>}
           | {snd <expr>}
           | {if <expr> <expr> <expr>}
           | {with {{<id> <expr>}*} <expr>}
           | {<id> <expr>*}

<binding> ::= {<id> <expr>}

<val> ::=  NumV | BoolV | {PairV <val> <val>}

|#

;;;;;;;;;;;;;
;;   AST   ;;
;;;;;;;;;;;;;


(deftype Fundef
  (fundef name args body))

(deftype Binding
  (binding id expr))

(deftype Expr
  (num n)
  (id s)
  (bool b)
  (pair l r)
  (add-one e)
  (add l r)
  (sub l r)
  (lt l r)
  (eq l r)
  (not-new b)
  (and-new l r)
  (or-new l r)
  (fst p)
  (snd p)
  (if-new c t f)
  (with bindings body)
  (app name args))

(deftype Prog
  (prog fundefs main))

(deftype Val
  (numV n)
  (boolV b)
  (pairV lV rV))


;;;;;;;;;;;;;;
;;  PARSER  ;;
;;;;;;;;;;;;;;

; parse-fundef :: s-expr -> Fundef
; Parsea una funcion a una estrucutra Fundef.
(define (parse-fundef src)
  (match src
    [(list 'define (list fname args ...) body) (fundef fname args (parse-expr body))]))

; parse-binding :: s-expr -> Binding
; Parsea un bind a una estrucutra Binding.
(define (parse-binding src)
  (match src
    [(list id expr) (binding id (parse-expr expr))]))

; parse-expr :: s-expr -> Expr
; Parsea una expresion a una estructura Expr.
(define (parse-expr src)
  (match src
    [(? number?) (num src)]
    [(? symbol?) (id src)]
    [(? boolean?) (bool src)]
    [(list 'cons l r) (pair (parse-expr l) (parse-expr r))]
    [(list 'add1 e) (add-one (parse-expr e))]
    [(list '+ l r) (add (parse-expr l) (parse-expr r))]
    [(list '- l r) (sub (parse-expr l) (parse-expr r))]
    [(list '< l r) (lt (parse-expr l) (parse-expr r))]
    [(list '= l r) (eq (parse-expr l) (parse-expr r))]
    [(list '! b) (not-new (parse-expr b))]
    [(list '&& l r) (and-new (parse-expr l) (parse-expr r))]
    [(list '|| l r) (or-new (parse-expr l) (parse-expr r))]
    [(list 'fst p) (fst (parse-expr p))]
    [(list 'snd p) (snd (parse-expr p))]
    [(list 'if c t f) (if-new (parse-expr c)
                              (parse-expr t)
                              (parse-expr f))]
    [(list 'with (list bindings ...) body) (with (map parse-binding bindings) (parse-expr body))]
    [(list name args ...) (app name (map parse-expr args))]))

; parse-prog :: s-expr -> Prog
; Parsea una lista de definiciones de funciones (optional) a una estrucutra Prog.
(define (parse-prog src)
  (match src
    [(list fundefs ... expr) (prog (map parse-fundef fundefs) (parse-expr expr))]))


;;;;;;;;;;;;;;;;;;;;;;;;;
;;   PRETTY-PRINTING   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;


; pp-val :: Val -> String
; Dado un valor del lenguaje, retorna
; su representación como String.
(define (pp-val val)
  (match val
    [(numV n) (number->string n)]
    [(boolV b) (format "~a" b)]
    [(pairV f s) (format "{cons ~a ~a}" (pp-val f) (pp-val s))]
    ))

;;;;;;;;;;;;;;;;;;;;
;;   INTÉRPRETE   ;;
;;;;;;;;;;;;;;;;;;;;

;; lookup-fundef :: Symbol List[Fundef] -> Fundef | error
;; Dado un nombre de función y una lista de funciones (Fundefs) retorna
;; la función completa (fundef). Lanza un error si la función no se encuentra en la lista.
(define (lookup-fundef fname fundefs)
  (match fundefs
    ['() (error "undefined function:" fname)]
    [(cons fd fds) (if (symbol=? (fundef-name fd) fname) fd (lookup-fundef fname fds))]
  ))

;; checktype :: (Val -> Boolean) List[Val] -> Val | #f
;; Dado un predicado y una lista de valores, retorna el primer valor
;; que no cumple con el predicado. Si todos los valores cumplen retorna #f.
(define (checktype pred list)
  (ormap (lambda (value) (if (pred value) #f value)) list))

;; interp :: Expr Env List[Fundef] -> Val | error
;; Evalua una expresión parseada del lenguaje en un ambiente y lista de funciones dadas, retorna
;; un Val o lanza un error si existe algun problema en la expresión.
(define (interp e env funs)
  (match e
    [(num n) (numV n)]
    [(id x) (env-lookup x env)]
    [(bool b) (boolV b)]
    [(pair l r) (pairV (interp l env funs) (interp r env funs))]
    [(add-one e)
          (match (interp e env funs)
            [(numV nv) (numV (+ nv 1))]
            [e (error (format "Runtime type error: expected Number found ~a" (pp-val e)))])]
    [(add l r)
          (match (list (interp l env funs) (interp r env funs))
            [(list (numV nv1) (numV nv2)) (numV (+ nv1 nv2))]
            [e (error (format "Runtime type error: expected Number found ~a" (pp-val (checktype numV? e))))])]
    [(sub l r)
          (match (list (interp l env funs) (interp r env funs))
            [(list (numV nv1) (numV nv2)) (numV (- nv1 nv2))]
            [e (error (format "Runtime type error: expected Number found ~a" (pp-val (checktype numV? e))))])]
    [(lt l r)
          (match (list (interp l env funs) (interp r env funs))
            [(list (numV nv1) (numV nv2)) (boolV (< nv1 nv2))]
            [e (error (format "Runtime type error: expected Number found ~a" (pp-val (checktype numV? e))))])]
    [(eq l r)
          (match (list (interp l env funs) (interp r env funs))
            [(list (numV nv1) (numV nv2)) (boolV (= nv1 nv2))]
            [e (error (format "Runtime type error: expected Number found ~a" (pp-val (checktype numV? e))))])]
    [(not-new b)
          (match (interp b env funs)
            [(boolV bv) (boolV (not bv))]
            [e (error (format "Runtime type error: expected Boolean found ~a" (pp-val e)))])]
    [(and-new l r)
          (match (list (interp l env funs) (interp r env funs))
            [(list (boolV bv1) (boolV bv2)) (boolV (and bv1 bv2))]
            [e (error (format "Runtime type error: expected Boolean found ~a" (pp-val (checktype boolV? e))))])]
    [(or-new l r)
          (match (list (interp l env funs) (interp r env funs))
            [(list (boolV bv1) (boolV bv2)) (boolV (or bv1 bv2))]
            [e (error (format "Runtime type error: expected Boolean found ~a" (pp-val (checktype boolV? e))))])]
    [(fst p)
          (match (interp p env funs)
            [(pairV lv rv) lv]
            [e (error (format "Runtime type error: expected Pair found ~a" (pp-val e)))])]
    [(snd p)
          (match (interp p env funs)
            [(pairV lv rv) rv]
            [e (error (format "Runtime type error: expected Pair found ~a" (pp-val e)))])]
    [(if-new c t f)
          (match (interp c env funs)
            [(boolV #t) (interp t env funs)]
            [(boolV #f) (interp f env funs)]
            [e (error (format "Runtime type error: expected Boolean found ~a" (pp-val e)))])]
    [(with bindings body) (interp body (foldr (lambda (bind env) (extend-env (binding-id bind) (interp (binding-expr bind) env funs) env)) env bindings) funs)]
    [(app f exprs)
      (match (lookup-fundef f funs)
        [(fundef _ arg-names body)
              (let* ([arg-vals (map (lambda (e) (interp e env funs)) exprs)])
                (if (not (= (length arg-names) (length arg-vals)))
                  (error (format "Arity mismatch: function bar expected ~a arguments, received ~a" (length arg-names) (length arg-vals)))
                  (let* ([env-extended (foldr (lambda (bind env)(extend-env (car bind) (cdr bind) env))
                                        empty-env ;; scope léxico
                                        (map cons arg-names arg-vals))])
        (interp body env-extended funs))))])]
    [_ (error "not yet implemented")]
    ))

;; run :: s-expr -> Val | error
;; Recibe un programa en notación s-expr, lo parsea, lo interpreta y retorna
;; su resultado como un Val. Lanza errores si el programa no es válido.
(define (run src)
  (let* ([p (parse-prog src)]
         [expr (prog-main p)]
         [fundefs (prog-fundefs p)])
    (interp expr empty-env fundefs)))
