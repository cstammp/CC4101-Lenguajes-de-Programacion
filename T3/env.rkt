#lang play

;; CC4101 Lenguajes de Programación
;; Cristóbal Stamm

#|
Environment abstract data type

empty-env        :: Env
env-lookup       :: Sym Env -> Val
multi-extend-env :: List<Sym> List<Val> Env -> Env
extend-frame-env! :: Sym Val Env -> Env


representation BNF:
<env> ::= (mtEnv)
        | (aEnv <id> <val> <env>)
|#

(deftype Env
  (mtEnv)
  (aEnv id val env))
 
(def empty-env  (mtEnv))
 
(def extend-env aEnv)

#|
env-lookup:: Sym Env -> Val
Busca un símbolo en el ambiente, retornando su valor asociado.
|#
(define (env-lookup x env)
  (match env
    [(mtEnv) (error 'env-lookup "free identifier: ~a" x)]
    [(aEnv id val rest)
     (if (symbol=? id x)
         val
         (env-lookup x rest))]))

#|
multi-extend-env:: List(Sym) List(Expr) Env -> Env
Crea un nuevo ambiente asociando los símbolos a sus valores.
|#
(define (multi-extend-env ids exprs env)
  (match (cons ids exprs)
    [(cons '() '()) env]
    [(cons (cons id id-rest) (cons val val-rest)) (aEnv id val (multi-extend-env id-rest val-rest env))]
    [else (error "wrong_input, mismatched lengths")]))