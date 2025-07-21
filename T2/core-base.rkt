#lang play

(print-only-errors #t)

(require "env.rkt")

;; CC4101 Lenguajes de Programación
;; Cristóbal Stamm

#|
<CL> ::= <num>
         | {+ <CL> <CL>}
         | {if0 <CL> <CL> <CL>}
         | {with {<sym> <CL>} <CL>}
         | <id>
         | {<CL> <CL>}
         | {fun {<sym>} <CL>}
         | {printn <CL>}
         | {mfun {<id>} <CL>}
|#

(deftype CL
  (num n)
  (add l r)
  (if0 c t f)
  (fun id body)
  (id s)
  (app fun-expr arg-expr)
  (printn e)
  (mfun id body))

;; parse :: s-expr -> CL
(define (parse-cl s-expr)
  (match s-expr
    [(? number?) (num s-expr)]
    [(? symbol?) (id s-expr)]
    [(list '+ l r) (add (parse-cl l) (parse-cl r))]
    [(list 'if0 c t f) (if0 (parse-cl c)
                            (parse-cl t)
                            (parse-cl f))]
    [(list 'with (list x e) b)
     (app (fun x (parse-cl b)) (parse-cl e))]
    [(list 'fun (list x) b) (fun x (parse-cl b))]
    [(list 'printn e) (printn (parse-cl e))]
    [(list f a) (app (parse-cl f) (parse-cl a))]
    [(list 'mfun (list x) b) (mfun x (parse-cl b))]))

;; values
(deftype Val
  (numV n)
  (closV id body env)
  (mclosV id body env mem))

;; interp :: Expr Env -> Val
(define (interp expr env)
  (match expr
    [(num n) (numV n)]
    [(fun id body) (closV id body env)]
    [(mfun id body) (mclosV id body env (make-hashalw))]
    [(add l r) (num+ (interp l env) (interp r env))]
    [(if0 c t f)
     (if (num-zero? (interp c env))
         (interp t env)
         (interp f env))]
    [(id x) (env-lookup x env)]
    [(printn e) 
      (def (numV n) (interp e env))
      ((printn-par) n)
      (numV n)]
    [(app fun-expr arg-expr)
     (match (interp fun-expr env)
       [(closV id body fenv)
        (interp body
                (extend-env id
                            (interp arg-expr env)
                            fenv))]
       [(mclosV id body fenv mem)
        (define arg-expr-val (interp arg-expr env))
        (if (hash-has-key? mem arg-expr-val)
          (hash-ref mem arg-expr-val)
          (let ([val (interp body
                (extend-env id
                            arg-expr-val
                            fenv))])
           (hash-set! mem arg-expr-val val)
           val))])]
  ))

(define (num+ n1 n2)
  (numV (+ (numV-n n1) (numV-n n2))))
 
(define (num-zero? n)
  (zero? (numV-n n)))
 
;; interp-top :: CL -> number
;; interpreta una expresión y retorna el valor final
(define (interp-top expr)
  (match (interp expr empty-env)
    [(numV n) n]
    [_ 'procedure]))
    
;; run-cl :: s-expr -> number
(define (run-cl prog)
  (interp-top (parse-cl prog)))

;; ========================================================= ;;

;;;;;;;;;;;;;;;;
;;     p1     ;;
;;;;;;;;;;;;;;;;

(deftype Result 
    (result val log))

;; print-log :: Box[List[Number]] -> List[Number]
;; Retorna el log
(define (print-log logn)
  (reverse (unbox logn)))

#|
(define log (box '()))

(define (reset-log)
  (set-box! log '()))

(define (println-g n)
  (set-box! log (cons n (unbox log))))

(define (interp-g expr)
  (reset-log)
  (result (interp expr empty-env) (print-log log)))
|#

;; printn-par :: Parameter[(Number -> Void)]
;; Parámetro que define la función que usa printn
(define printn-par (make-parameter println))

;; log-local :: Parameter[Box[List[Number]]]
;; Parámetro que contiene la caja donde se guarda el log
(define log-local (make-parameter (box '())))

;; println-p :: Number -> Void
;; Agrega un número al log actual
(define (println-p n)
  (set-box! (log-local) (cons n (unbox (log-local)))))

;; interp-p :: CL -> Result
;; Evalúa una expresión CL y retorna su resultado junto al log
(define (interp-p expr)
  (parameterize ([printn-par println-p]
                 [log-local (box '())])
    (result (interp expr empty-env) (print-log (log-local)))))

;; ========================================================= ;;