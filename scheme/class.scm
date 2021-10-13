(define (create-class create)
  (define constructors '())
  (define methods '())
  (define getters '())

  (define (def-cons proc arg-count)
    (cond
      ((< arg-count 0) (error "def-cons" "" arg-count proc))
      ((not (procedure? proc)) (error "def-cons" "" arg-count proc))
      (else (set! constructors (append constructors (list arg-count proc))))))

  (define (def-method name proc)
    (cond 
      ((not (symbol? name)) (error "def-method" "" name proc))
      ((not (procedure? proc)) (error "def-method" "" name proc))
      (else (set! methods (append methods (list name proc))))))

  (define (def-getter name proc)
    (cond 
      ((not (symbol? name)) (error "def-getter" "" name proc))
      ((not (procedure? proc)) (error "def-getter" "" name proc))
      (else (set! getters (append getters (list name proc))))))

  (define (call-constructor args)
    (define (find-constructor ls arg-count)
      (if (null? ls)
        '()
        (if (eq? (car ls) arg-count)
          (cadr ls)
          (find-constructor (cdr ls) arg-count))))

    (define (try-call-constructor args arg-count)
      (let ([proc (find-constructor constructors arg-count)])
        (if (null? proc)
          (error "call-constructor" "" args)
          (apply proc args))))

    (let ([cons-count (length constructors)]
          [arg-count (length args)])
      (if (> cons-count 0)
        (try-call-constructor args arg-count)
        (if (> arg-count 0)
          (error "call-constructor" "" args)))))

  (define (dispatch op-name . args)
    (if (= (length args) 0)
      (let ([proc (find-proc getters op-name)])
        (if (null? proc)
          (try-call-method op-name args)
          (call-proc proc args)))
      (try-call-method op-name args)))
  
  (define (try-call-method method-name args)
    (let ([proc (find-proc methods method-name)])
      (if (null? proc)
        (error "try-call-method" "" op-name args)
        (call-proc proc args))))

  (define (find-proc proc-list name)
    (if (null? proc-list)
      '()
      (if (eq? (car proc-list) name)
        (cadr proc-list)
        (find-proc (cdr proc-list) name))))

  (define (call-proc proc args)
    (apply proc args))

  (lambda args
    (create def-cons def-method def-getter)
    (call-constructor args)
    dispatch))



(define Vector2
  (create-class
    (lambda (def-cons def-method def-getter)
      (define x 0)
      (define y 0)

      (def-cons (lambda () '()) 0)

      (def-cons
        (lambda (pair)
          (set! x (car pair))
          (set! y (cdr pair)))
        1)

      (def-cons
        (lambda (_x _y)
          (set! x _x)
          (set! y _y))
        2)

      (def-getter 'x (lambda () x))
      (def-method 'x (lambda (_x) (set! x _x)))

      (def-getter 'y (lambda () y))
      (def-method 'y (lambda (_y) (set! y _y)))

      (def-method 'pair
        (lambda () (cons x y))))))


(define vec (Vector2))
(display (vec 'pair))
(newline)

(define vec1 (Vector2 '(5 6)))
(display (vec1 'y))
(newline)

(define vec2 (Vector2 3 4))
(display (vec2 'x))
(newline)
(vec2 'x 10)
(display (vec2 'x))
(newline)
(display (vec2 'pair))
