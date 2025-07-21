
#lang play

#|-----------------------------
Environment abstract data type

empty-env  :: Env
extend-env :: Sym Val Env -> Env


representation BNF:
<env> ::= (mtEnv)
| (aEnv <id> <val> <env>)
|#

(deftype Env
  (mtEnv)
  (aEnv id val env))

;; empty-env  :: Env
;; Crea un ambiente vacio.
(def empty-env  (mtEnv))

;; extend-env :: Sym Val Env -> Env
;; Agrega un (Sym Val) al ambiente.
(def extend-env aEnv)

;; env-lookup :: Sym Env -> Val
;; Busca el valor de un Sym en el ambiente.
(define (env-lookup x env)
  (match env
    [(mtEnv) (error 'env-lookup "free identifier: ~a" x)]
    [(aEnv id val rest)
     (if (symbol=? id x)
         val
         (env-lookup x rest))]))