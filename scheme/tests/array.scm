(define (make-array dimensions)
  (let ((currd (car dimensions))
        (restd (cdr dimensions)))
    (let ((vec (make-vector currd)))
      (if (null? restd)
          (for i in (0 to currd)
            (vector-fill! vec '()))
          (for i in (0 to currd)
            (vector-set! vec i (make-array restd))))
      (if (null? restd)
          vec
          (cons 'array_type (cons dimensions vec))))))

(define (array-dimensions array)
  (if (vector? array)
      (list (vector-length array))
      (car (cdr array))))
        
(define (array-content array)
  (if (vector? array)
      array
      (cdr (cdr array))))
      
(define (array? x)
  (or (vector? x)
      (and (pair? x) (eq? (car x) 'array_type))))

(define (array-ref array indexs)
  (cond ((null? indexs) array)
        ((null? (cdr indexs)) (vector-ref (array-content array) (car indexs)))
        (else (array-ref
                (vector-ref (array-content array) (car indexs))
                (cdr indexs)))))

(define (array-set! array indexs value)
  (cond ((null? (cdr indexs)) (vector-set! array (car indexs) value))
        (else (array-set!
                (vector-ref (array-content array) (car indexs))
                (cdr indexs)
                value))))

#|
;; test
(define a1 (make-array '(2)))
(define a2 (make-array '(2 3)))
(define a3 (make-array '(2 3 4)))
(array? a1)
(array? a2)
(array? a3)
(array-dimensions a1)
(array-dimensions a2)
(array-dimensions a3)
(array-content a2)
(array-set! a1 '[0] 1)
(array-ref a1 '[0])
(array-set! a2 '[1 2] 5)
(array-ref a2 '[1 2])
|#