(define (test-alist color type locations)
  (cons
   (cons 'color color)
   (cons
    (cons 'type type)
    (cons
     (cons 'locations
           (cons (car locations)
                 (cons (list-ref locations 1)
                       '())))
     '()))))

;(test-alist 'green 'fruit '(ny calif))


(define (find-day n)
  (if (or (< n 1) (> n 7))
      'BAD-INPUT
      (vector-ref
       #(Monday Tuesday Wednesday Thursday Friday Saturday Sunday)
       (- n 1))))

;(find-day 3)
;(find-day 30)


(define (nest s n)
  (if (= n 0)
      s
      (nest (list s) (- n 1))))

;(nest "hello" 5)


(define (print-atoms lst)
  (if (pair? lst)
      (begin
        (display (car lst))
        (print-atoms (cdr lst)))
      (if (not (null? lst))
          (display lst))))

;(print-atoms '(1 2 3))
;(print-atoms '(1 2 . 3))