#lang play

;; CC4101 Lenguajes de Programación
;; Cristóbal Stamm

(require "env.rkt")

#|
  <expr>  ::= <num>
            | <sym>
            | <bool>
            | {if <expr> <expr> <expr>}
            | {+ <expr> <expr>}
            | {< <expr> <expr>}
            | {* <expr> <expr>}
            | {= <expr> <expr>}
            | {- <expr> <expr>}
            | {and <expr> <expr>}
            | {or <expr> <expr>}
            | {not <expr> <expr>}
            | {begin <expr> <expr>}
            | {with {<binding>*} <expr>}

<binding> ::= {<sym> <expr>}

; EXTENSIÓN PARA CLASE Y OBJETOS
 <expr>  ::= ... (expresiones del lenguaje entregado) ...
           | {class {<sym>*} <method>*}
           | {new <expr> <expr>*}
           | {get <expr> <sym>}
           | {set <sym> <expr>}
           | {-> <expr> <sym> <expr>*}
           | self

<method> ::= {def <sym> {<sym>*} <expr>}

; EXTENSIÓN PARA LAMBDAS
; SÓLO COMO AZÚCAR SINTÁCTICA. NO DEBEN MODIFICAR AST NI INTERP.
 <expr>  ::= ... (lenguaje base + clases y objetos ) ...
           | {fun {<sym>*} <expr>}
           | {<expr> <expr>*}
|#


(deftype Expr
  (num n)
  (bool b)
  (id s)   
  (binop f l r)
  (unop f s)
  (my-if c tb fb)  
  (begn expr1 expr2)  
  (with defs body)
  (class flds mthds)
  (new cls flds)
  (get expr id)
  (set fld expr)
  (-> exp-class id exp-args))

(deftype Binding
  (binding id expr))

; Methods
; Mantenemos la aridad como optimización
(deftype Method
  (method name params arity body))

;; values
(deftype Val
  (numV n)
  (boolV b)
  (classV flds mthds)
  (objV cls flds))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; parse :: s-expr -> Expr
; Parse the given s-expr into a Expr
(define (parse s-expr)
  (match s-expr
    [(? number?) (num s-expr)]
    [(? symbol?) (id s-expr)]
    [(? boolean?) (bool s-expr)]
    [(list '* l r) (binop * (parse l) (parse r))]
    [(list '+ l r) (binop + (parse l) (parse r))]
    [(list '- l r) (binop - (parse l) (parse r))]
    [(list '< l r) (binop < (parse l) (parse r))]
    [(list '= l r) (binop = (parse l) (parse r))]
    [(list 'or l r) (binop (λ (i d) (or i d)) (parse l) (parse r))]
    [(list 'and l r) (binop (λ (i d) (and i d)) (parse l) (parse r))]
    [(list 'not b) (unop not (parse b))]
    [(list 'if c t f) (my-if (parse c)
                             (parse t)
                             (parse f))]
    [(list 'begin e1 e2) (begn (parse e1) (parse e2))]
    [(list 'with (list e ...)  b)
     (with (map parse-binding e) (parse b))]
    ; Extensiones
    [(list 'class (list fields ...) methods ...)
     (def mthds (map parse-method methods))
     (class fields mthds)]
    [(list 'new expr args ...) (new (parse expr) (map parse args))]
    [(list 'get expr fld) (get (parse expr) fld)]
    [(list 'set fld expr) (set fld (parse expr))]
    [(list '-> expr mthd args ...) (-> (parse expr) mthd (map parse args))]
    [(list 'fun (list params ...) expr) (new (class '() (list (method 'lmbda params (length params) (parse expr)))) '())]
    [(list f args ...) (-> (parse f) 'lmbda (map parse args))]
    ))


;; parse-binding :: s-expr -> Def
(define (parse-binding s-expr)
  (match s-expr
    [(list id b) (binding id (parse b))]))

;; parse-method :: s-expr -> Method
(define (parse-method s-expr)
  (match s-expr
    ;; setting the arity immediately
    [(list 'def name params body) (method name params (length params) (parse body))]
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;              WELL-FORMED                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; well-formed :: Expr -> Boolean or error
;; Static check on an expression
(define (well-formed expr)
  (define (well-formed-expr expr in-method)
  (match expr
    [(num _) #t]
    [(bool _) #t]
    [(id name) (if (eq? name 'self)
                        (if in-method
                              #t
                              (error "error: self outside of method"))
                        #t)]
    [(binop _ l r) (begin (well-formed-expr l in-method) (well-formed-expr r in-method))]
    [(unop _ s) (well-formed-expr s in-method)]
    [(my-if c t f) (for-each (lambda (e) (well-formed-expr e in-method)) (list c t f))]
    [(begn e1 e2) (begin (well-formed-expr e1 in-method) (well-formed-expr e2 in-method))]
    [(with defs body)
     (begin (for-each (lambda (e) (well-formed-expr e in-method)) (map binding-expr defs))
            (well-formed-expr body in-method))]
    ; Extensiones
    [(class f m)
        (begin 
          ;; Verifica parametros repetidos (e.j. {class {x y x} ...})
          (if (repeated? f)
              (error "error: duplicate fields")
              #t)
          
          ;; Verifica aridad de constructores (init) repetida
          (let ([arity-list (map (lambda (met) (cons (method-name met) (method-arity met)))
                                    (filter (lambda (met)
                                        (eq? (method-name met) 'init))
                                        m))])
            (if (repeated? arity-list)
                (error "error: same arity constructor")
                #t))
          
          ;; Verifica aridad de metodos (=! init) repetida
          (let ([arity-list (map (lambda (met) (cons (method-name met) (method-arity met)))
                                    (filter (lambda (met)
                                        (not (eq? (method-name met) 'init)))
                                        m))])
            (let ([rep (repeated? arity-list)])
              (if rep
                  (error (format "error: overloading method ~a with the same arity" (car rep)))
                  #t)))

          ;; Verifica que los metodos esten bien formados
          (for-each (lambda (met) (well-formed-expr (method-body met) #t)) m)
    )]
    [(new c f) (begin (well-formed-expr c in-method) (for-each (lambda (arg) (well-formed-expr arg in-method)) f))]
    [(-> o _ args) (begin (well-formed-expr o in-method) (for-each (lambda (arg) (well-formed-expr arg in-method)) args))]
    [(get e _) (well-formed-expr e in-method)]
    [(set _ _) (if in-method
                      #t
                      (error "error: set outside of method"))]
    [_ (error "not implemented")]))
  (well-formed-expr expr #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;          FUNCIONES AUXILIARES           ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; repeated? :: List[A] -> (A | False)
;; Returns the first repeated element in the list, or #f if none is found.
(define (repeated? l)
  (define seen (make-hash))
  (define (check rest)
    (cond
      [(empty? rest) #f]
      [(hash-has-key? seen (car rest)) (car rest)]
      [else (begin (hash-set! seen (car rest) #f)
                   (check (cdr rest)))]))
  (check l))

;; findl :: (A -> Bool) List[A] -> (A | False)
;; Returns the first element in the list that satisfies the given predicate, or #f if none is found.
(define (findl p l)
  (cond
    [(empty? l) #f]
    [(p (car l)) (car l)]
    [else (findl p (cdr l))]))

;; exists-name? :: Sym List[Method] -> Boolean
;; Checks if a method exists in the list of methods.
(define (exists-name? name methds)
  (ormap (lambda (m) (eq? (method-name m) name)) methds))

;; init-zero? :: Val -> Boolean
;; Checks if an object has init method with arity 0.
(define (init-zero? obj)
  (match obj
    [(objV cls _) 
      (match cls
        [(classV _ met)
           (cond 
                [(findl (lambda (m) (and (eq? 'init (method-name m))
                                          (zero? (method-arity m))))
                                          met) #t]
                [else #f])])]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                 INTERP                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; interp :: Expr Env -> Val
;; Evaluate the expression in an environment.
(define (interp expr env)
  (match expr
    [(num n) (numV n)]    
    [(bool b) (boolV b)]    
    [(binop f l r) (make-val (f (open-val (interp l env))
                                (open-val (interp r env))))]
    [(unop f s) (make-val (f (open-val (interp s env))))]
    [(my-if c t f)
     (def (boolV cnd) (interp c env))
     (if cnd
         (interp t env)
         (interp f env))]
    [(id x) (env-lookup x env)]        
    [(begn expr1 expr2) (begin 
                          (interp expr1 env)
                          (interp expr2 env))]
    [(with defs body)
     (interp body
             (foldl (λ (x acc-env)
                      (def (binding id expr) x)
                      (extend-env id (interp expr acc-env) acc-env))
                    env defs))]
    [(class f m) (classV f m)]
    [(new c f) (let* ([cls (interp c env)]
                      [arg-vals (map (lambda (a) (interp a env)) f)])
        (match cls
          [(classV flds _)
           (let* ([fld-table (make-hash)]
                  [obj (objV cls fld-table)])
             ;; inicializa los campos como not_initialized
             (for-each (λ (fld) (hash-set! fld-table fld 'not_initialized)) flds)
             ;; Intenta llamar a init
             (if (and (not (init-zero? obj)) (empty? arg-vals))
                 obj ; sin constructor
                 (begin
                   (invoke-method obj 'init arg-vals env)
                   obj)))]))]
    [(-> o mname args) (let* ([obj (interp o env)]
                              [arg-vals (map (lambda (a) (interp a env)) args)])
                                    (invoke-method obj mname arg-vals env))]
    [(get o fld) (let ([obj (interp o env)])
                  (match obj
                    [(objV _ fld-table)
                        (if (hash-has-key? fld-table fld)
                            (let ([val (hash-ref fld-table fld)])
                                (if (equal? val 'not_initialized)
                                    (error (format "error: field ~a not initialized" fld))
                                    val))
                            (error (format "error: field ~a not found" fld)))]))]
    [(set fld id) (let ([obj (env-lookup 'self env)] 
                          [expr (interp id env)])
                  (match obj
                    [(objV _ fld-table)
                        (if (hash-has-key? fld-table fld)
                            (hash-set! fld-table fld expr)
                            (error (format "error: field ~a not found" fld)))]))]
    [_ (error "not implemented")]))

;; open-val :: Val -> Scheme Value
(define (open-val v)
  (match v
    [(numV n) n]
    [(boolV b) b]
    ))

;; make-val :: Scheme Value -> Val
(define (make-val v)
  (match v
    [(? number?) (numV v)]
    [(? boolean?) (boolV v)]
    ))

;; invoke-method :: Val Sym List<Val> Env -> Val
;; Invokes the method "mname" on the object
(define (invoke-method obj mname fld env)
  (match obj
    [(objV cls _) 
      (match cls
        [(classV _ met)
           (let* ([arity (length fld)]
                  [found (findl (lambda (m) (and (eq? mname (method-name m))
                                                 (= arity (method-arity m))))
                                               met)])
                          (cond
                            ; El método/constructor existe
                            [found
                                (match found
                                  [(method _ params _ body)
                                   (interp body (multi-extend-env params fld (extend-env 'self obj env)))])]
                            ; No existe el constructor
                            [(eq? mname 'init) (error "error: constructor not found")]
                            ; Existe el metodo pero no con esa aridad
                            [(exists-name? mname met) (error (format "error: no overload of ~a arguments was found for method ~a" arity mname))]
                            ; Else: No existe el método
                            [else (error (format "error: method ~a not found" mname))]))])]))

;; run :: s-expr -> Val
(define (run s-expr)
  (interp (parse s-expr) empty-env))

#|
run-val:: s-expr -> Scheme-Val + Val
Versión alternativa de run, que retorna valores de scheme para primitivas y
valores de MiniScheme para clases y objetos
|#
(define (run-val s-expr)
  (def parsed-expr (parse s-expr))
  (begin
    (well-formed parsed-expr)
    (match (interp parsed-expr empty-env)
      [(numV n) n]
      [(boolV b) b]
      [x x])))