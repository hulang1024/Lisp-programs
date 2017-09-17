;(load "scheme\\stdlib\\lib.scm")
;(load "scheme\\tests\\array.scm")

(define (draw-rect array x y w h value)
  (for hi in (0 to h)
    (for wi in (0 to w)
      (array-set! array (list (+ x wi) (+ y hi)) value))))

(define (draw-line-x array x y sign len value)
  (for i in (0 to len)
    (array-set! array (list (+ x (* i sign)) y) value)))
    
(define (draw-circle array x y radius value)
  (let ((bx (+ (- (+ x 1) radius) (/ (- radius 1) 2) 1))
        (by (- y (/ (+ (- radius 2) radius) 2))))
    (for i in (0 to (/ radius 2) 1)
      (draw-line-x array (dec! bx) by +1 (+ (* 2 i) radius) value)
      (inc! by))
    (dec! bx)
    (for j in (0 to radius)
      (draw-line-x array bx by +1 (- (* radius 2) 1) value)
      (inc! by))
    (for k in (0 to (/ radius 2) 1)
      (draw-line-x array (inc! bx) by +1 (- (* 2 (- radius 1)) 1 (* 2 k)) value)
      (inc! by))))

(define (clear-array array value)
  (for y in (0 to (cadr (array-dimensions array)))
    (for x in (0 to (car (array-dimensions array)))
      (array-set! array (list y x) value))))

(define (print-ascii-art array)
  (for y in (0 to (cadr (array-dimensions array)))
    (for x in (0 to (car (array-dimensions array)))
      (display (array-ref array (list x y))))
      (newline)))
      
(define (draw-diagonals array value)
  (let ((w (car (array-dimensions array)))
        (h (cadr (array-dimensions array))))
    (for i in (0 to (if (< w h) m h))
      (array-set! array (list i i) value)
      (array-set! array (list i (- w i 1)) value))))
      
(define (ascii-test)
  (let ((array (make-array (list 19 19))))
    (clear-array array #\.) ; bkg
    (draw-diagonals array #\*) ; leg
    (draw-rect array 0 0 19 9 #\|) ; sky
    (draw-circle array 9 8 7 #\+) ; head
    (draw-circle array 9 8 5 #\space) ; face
    (draw-rect array 7 7 1 1 #\O) ; eye
    (draw-rect array 11 7 1 1 #\O) ; eye
    (draw-rect array 7 6 1 1 #\~) ; eyebrow
    (draw-rect array 11 6 1 1 #\~) ; eyebrow
    (draw-rect array 9 9 1 1 #\V) ; nose
    (draw-rect array 8 11 3 1 #\~) ; mouth
    (print-ascii-art array)))
(ascii-test)