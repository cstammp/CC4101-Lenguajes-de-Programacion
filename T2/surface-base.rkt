#lang play
(print-only-errors #t)

(require "env.rkt")
(require "core-base.rkt")

;; CC4101 Lenguajes de Programación
;; Cristóbal Stamm

#| SL: Surface Language

<SL> ::= <num>
         | {+ <SL> <SL>}
         | {if0 <SL> <SL> <SL>}
         | {with {<sym> <SL>} <SL>}
         | <id>
         | {<SL> <SL>}
         | {fun {<sym> : <mtype>} → <mtype> : <SL>}

         | {printn <SL>}

<mtype> ::= {<mod> <type>} ; tipo con modificador
         | <type>          ; azúcar para evaluación temprana

<mod> ::= lazy   ; call-by-need / lazy
        | name   ; call-by-name

<type> ::= Num | {<mtype> -> <mtype>}


|#

(deftype SL         
  ; los constructores empiezan con 's' para diferenciar con el AST de CL
  ; tp es el tipo de cada nodo
  (snum tp n)
  (sadd tp l r)
  (sif0 tp c t f)
  (swith tp x e b)
  (sfun tp p b)
  (sprintn tp e)
  (sid tp s)
  (sapp tp f e))

(deftype Type
  (TNum mod)
  (TFun mod dom cod))

; sl-type : SL -> Type
; accesor polimórfico a la info de tipo de una expr
(define (sl-type sl)
  (match sl
    [(snum tp _) tp]
    [(sadd tp _ _) tp]
    [(sif0 tp _ _ _) tp]
    [(swith tp _ _ _) tp]
    [(sfun tp _ _) tp]
    [(sprintn tp _) tp]
    [(sid tp _) tp]
    [(sapp tp _ _) tp]))

; parse-sl : s-expr -> SL
; parsea expresiones SL
(define (parse-sl s-expr)
  (match s-expr
    [(? number?) (snum #f s-expr)]
    [(? symbol?) (sid #f s-expr)]
    [(list '+ l r) (sadd #f (parse-sl l) (parse-sl r))]
    [(list 'if0 c t f) (sif0 #f (parse-sl c) (parse-sl t) (parse-sl f))]
    [(list 'with (list x e) b) (swith #f x (parse-sl e) (parse-sl b))]
    [(list 'printn e) (sprintn #f (parse-sl e))]
    [(list 'fun (list x ': dom-type) '-> cod-type ': b) (sfun (parse-mtype (list dom-type '-> cod-type)) x (parse-sl b))]
    [(list f a) (sapp #f (parse-sl f) (parse-sl a))]))

;; ========================================================= ;;

;;;;;;;;;;;;;;;;
;;     p3     ;;
;;;;;;;;;;;;;;;;

#|
; parse-type : s-expr -> Type (deprecated)
;; Parsea una s-expresión a un tipo
(define (parse-type s-expr)
  (match s-expr
    ['Num (TNum)]
    [(list dom '-> cod) (TFun (car (parse-mtype dom)) (cdr (parse-mtype dom)) (car (parse-mtype cod)) (cdr (parse-mtype cod)))]
    [_ (error "Tipo inválido")]))
|#

; parse-mtype : s-expr -> Type
;; Parsea una s-expresión a un tipo
(define (parse-mtype s-expr)
  (match s-expr
    ['Num (TNum 'eager)]
    [(list dom '-> cod) (TFun 'eager (parse-mtype dom) (parse-mtype cod))]
    [(list 'lazy t) 
      (match t
        ['Num (TNum 'lazy)]
        [(list dom '-> cod) (TFun 'lazy (parse-mtype dom) (parse-mtype cod))])] 
    [(list 'name t) 
      (match t
        ['Num (TNum 'name)]
        [(list dom '-> cod) (TFun 'name (parse-mtype dom) (parse-mtype cod))])]
    [_ (error "Tipo inválido")]))

; type-mod : Type -> Symbol
; Retorna el modificador del un tipo
(define (type-mod t)
  (match t
    [(TFun mod _ _) mod]
    [(TNum mod) mod]))

; sl-mod : SL -> Symbol
; Retorna el modificador del tipo de una expresión SL
(define (sl-mod sl)
  (type-mod (sl-type sl)))

;; ========================================================= ;;

; type->str : Type -> String
; representación en string de un tipo
(define (type->str t)
  (match t
    [(TNum mod) (string-append "Num" (symbol->string mod))]
    [(TFun mod t1 t2) (string-append "{" (symbol->string mod) " " (type->str t1)
                                      " -> " (type->str t2) "}")]))

; check-type : Type Type -> Void
; falla si los dos tipos no son iguales
(define (check-type expected actual)
  (when (not (equal? expected actual))  ; when es como un if con una sola rama (si la condicion es falsa no evalua nada)
    (error (format "type error: expected ~a, got ~a"
                   (type->str expected) (type->str actual)))))

; check-function-type : Type -> Void
; falla si el tipo no es un tipo función
(define (check-function-type t)
  (when (not (TFun? t))
    (error (format "type error: expected a function type, got ~a"
                   (type->str t)))))

; tipo num por defecto (usado por type-ast)
(define tnum (TNum 'eager))



(define (strip-mod t)
  (match t
    [(TFun mod dom cod) (TFun 'eager dom cod)]
    [(TNum mod) (TNum 'eager)]))

(define (compatible? t1 t2)
  (equal? (strip-mod t1) (strip-mod t2)))

(parse-mtype '{Num -> Num})

(compatible? (parse-mtype '{lazy {Num -> Num}}) (parse-mtype '{Num -> Num}))
(compatible? (parse-mtype '{{lazy Num} -> Num}) (parse-mtype '{Num -> Num}))

(define (set-type expr type)
  (match expr 
    [(snum _ n) (snum type n)]
    [(sadd _ l r) (sadd type l r)]
    [(sfun _ x b) (sfun type x b)]
    [(sid _ x) (sid type x)]
    [(sapp _ f a) (sapp type f a)]
    [(swith _ x e b) (swith type x e b)]
    [(sif0 _ c t f) (sif0 type c t f)]
    [(sprintn _ e) (sprintn type e)]
  )
)


; type-ast : SL Env -> SL
; retorna el ast decorado con tipos (o falla si la expr no es válida)
; se usa Env como un ambiente de tipos (mapea identificadores a tipos)
(define (type-ast expr tenv)
  (match expr 
    [(snum _ n) (snum tnum n)]
    
    [(sadd _ l r) (def tl (type-ast l tenv))
                  (def tr (type-ast r tenv))
                  (compatible? tnum (sl-type tl))
                  (compatible? tnum (sl-type tr))
                  (sadd tnum tl tr)]

    [(sfun t x b)  (check-function-type t)
                   (def tb (type-ast b (extend-env x (TFun-dom t) tenv)))
                   (compatible? (TFun-cod t) (sl-type tb))
                   (sfun t x tb)]
    
    [(sid _ x) (sid (env-lookup x tenv) x)]

    [(sapp _ f a) (def tf (type-ast f tenv))
                  (def t (sl-type tf))
                  (check-function-type t)
                  (def ta (type-ast a tenv))
                  (compatible? (TFun-dom t) (sl-type ta))
                  (sapp (TFun-cod t) tf (set-type ta (TFun-dom t)))]

    [(swith _ x e b) (def te (type-ast e tenv))
                     (def tb (type-ast b (extend-env x (sl-type te) tenv)))
                     (swith (sl-type tb) x te tb)]

    [(sif0 _ c t f) (def tc (type-ast c tenv))
                    (compatible? tnum (sl-type tc))
                    (def tt (type-ast t tenv))
                    (def tf (type-ast f tenv))
                    (compatible? (sl-type tt) (sl-type tf))
                    (sif0 (sl-type tt) tc tt tf)]

    [(sprintn _ e) (def te (type-ast e tenv))
                   (compatible? tnum (sl-type te))
                   (sprintn tnum te)]))

; set-mod :: SL -> CL
(define (set-mod expr param)
  (match (sl-mod expr)
    ['lazy (mfun '_ param)]
    ['name (fun '_ param)]
    ['eager param]))

(define (force-app expr)
  (match expr
    [(fun '_ param) (app (force-app param) (num 0))]
    [(mfun '_ param) (app (force-app param) (num 0))]
    [_ expr]))

; transform : SL -> CL
; transforma un programa SL a un programa CL equivalente
; ignora la información de tipo, y traduce un `with` a un aplicación de lambda
(define (transform expr)
  (match expr
    [(snum _ n)      (set-mod expr (num n))]

    [(sid _ x)       (set-mod expr (id x))]

    ;; [(swith _ x e b) (set-mod expr (app (fun x (transform b)) (transform e)))]

    [(swith _ x e b) (app (fun x (transform b)) (set-mod e (transform e)))]

    [(sadd _ l r)    (set-mod expr (add (force-app (transform l)) (force-app (transform r))))]

    [(sif0 _ c t f)  (set-mod expr (if0 (force-app (transform c)) (transform t) (transform f)))]

    [(sfun _ x b)    (set-mod expr (fun x (transform b)))]

    [(sapp _ f a)    (set-mod expr (app (force-app (transform f)) (transform a)))]

    [(sprintn _ e)   (set-mod expr (printn (transform e)))])) 


;; run-p-sl : s-expr -> Result
;; Igual que run-sl, pero con interp-p
(define (debugg prog)
  (transform (type-ast (parse-sl prog) empty-env)))

(define (run-sl prog)
  (interp-top (transform (type-ast (parse-sl prog) empty-env))))

;; run-p-sl : s-expr -> Result
;; Igual que run-sl, pero con interp-p
(define (run-p-sl prog)
  (interp-p (transform (type-ast (parse-sl prog) empty-env))))

(run-p-sl '{with {f {fun {x : Num} -> Num : {+ x x}}}   
                     {f {printn 10}}}) 

(run-p-sl '{with {f {fun {x : {lazy Num}} -> Num : {+ x x}}}   
                     {f {printn 10}}})

(run-p-sl '{with {f {fun {x : {name Num}} -> Num : {+ x x}}} 
                     {f {printn 10}}})

(run-p-sl '{with {f {fun {x : {lazy Num}} -> Num : 1}}   
                     {f {printn 10}}})