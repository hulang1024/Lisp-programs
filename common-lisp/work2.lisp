(print
  (mapcar (lambda (lst)
            (if (= (mod (length lst) 2) 0) :even :odd))
          '((a b c) (1 2) (10 c) (3 a))))
;(:odd :even :even :even)

(defun count-chars (char lst)
  (reduce (lambda (sum l) (+ sum (count char l))) lst :initial-value 0))
(print (count-chars #\e '("these" "cities" "are" "not" "big"))) ;4


;;concisely
#|
(let ((results nil))
  (loop for x from 30 to 50
    do (setq results (append (list (code-char x)) results)))
  results)
|#
(loop for x from 50 above 30 append (list (code-char x)))

#|
(let ((sum 0))
  (loop for x from 0 below 100
    do (when (zerop (mod x 5))
         (incf sum (expt x 2))))
  sum)
|#
(loop for x from 0 below 100
  when (zerop (mod x 5))
  sum (expt x 2))

#| 
(loop for x in '((1 a) (2 b) (3 c) (4 d) (5 e) (6 f))
  for first = (car x)
  for second = (second x)
  do (format t "~%~a: ~a" (+ first 5) second))
|#
(loop for x in '((1 a) (2 b) (3 c) (4 d) (5 e) (6 f))
  do (format t "~%~a: ~a" (+ (car x) 5) (second x)))
  
  
;;draw 
(defun draw-rect (array x y w h value)
  (dotimes (hi h)
    (dotimes (wi w)
      (setf (aref array (+ x wi) (+ y hi)) value))))

(defun draw-line-x (array x y sign len value)
  (dotimes (i len)
    (setf (aref array (+ x (* i sign)) y) value)))
    
(defun draw-circle (array x y radius value)
  (let ((bx (+ (- (+ x 1) radius) (/ (- radius 1) 2) 1))
        (by (- y (/ (+ (- radius 2) radius) 2))))
    (loop for i from 0 to (- (/ radius 2) 1) do
      (draw-line-x array (decf bx) by +1 (+ (* 2 i) radius) value)
      (incf by))
    (decf bx)
    (loop for j from 0 to (- radius 1) do
      (draw-line-x array bx by +1 (- (* radius 2) 1) value)
      (incf by))
    (loop for k from 0 to (- (/ radius 2) 1) do
      (draw-line-x array (incf bx) by +1 (- (* 2 (- radius 1)) 1 (* 2 k)) value)
      (incf by))))

(defun clear-array (array value)
  (dotimes (y (second (array-dimensions array)))
    (dotimes (x (first (array-dimensions array)))
      (setf (aref array y x) value))))

(defun print-ascii-art (array)
  (dotimes (y (second (array-dimensions array)))
    (dotimes (x (first (array-dimensions array)))
      (princ (aref array x y)))
      (terpri)))

(defun draw-diagonals (array value)
  (let ((w (first (array-dimensions array)))
        (h (second (array-dimensions array))))
    (dotimes (i (min w h))
      (setf (aref array i i) value)
      (setf (aref array i (- w i 1)) value))))

(defun ascii-test ()
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
#|
|||||||||||||||||||
|||||||||||||||||||
||||||+++++++||||||
|||||+++++++++|||||
||||+++     +++||||
|||+++       +++|||
|||++  ~   ~  ++|||
|||++  O   O  ++|||
|||++         ++|||
...++    V    ++...
...++         ++...
...+++  ~~~  +++...
....+++     +++....
.....+++++++++.....
....*.+++++++.*....
...*...........*...
..*.............*..
.*...............*.
*.................*
|#


(defun flatten (lst)
  (reduce (lambda (result x)
            (append result
                    (if (listp x) (flatten x) (list x))))
          lst
          :initial-value nil))
(flatten '((a (b c (d e) f g)) h (i j))) ;(A B C D E F G H I J)

(defun str-append (strings &optional separator)
  (if (null (cdr strings))
      (car strings)
      (concatenate 'string (car strings)
        separator
        (str-append (cdr strings) separator))))
(str-append '("foo" "bar" "baz") ", ") ;"foo, bar, baz"
(str-append '("foo" "bar" "baz")) ;"foobarbaz"

(defun partition (test lst)
  (if (eql test #'eql)
    (setf test (lambda (x) x)))
  (let ((ht (make-hash-table))
        (key nil) (tmp nil))
    (dolist (obj lst)
      (setf key (funcall test obj))
      (setf tmp (gethash key ht))
      (if (null tmp)
          (setf (gethash key ht) (list obj))
          (setf (gethash key ht) (append tmp (list obj)))))
      (let ((result '()))
        (maphash (lambda (k v)
                   (setf result (append result (list v)))) ht)
        result)))
(partition #'eql '(2 3 4 3 4 5 2 54)) ;((2 2) (54) (3 3) (5) (-4 4))
(partition (lambda (x) (mod x 3)) '(1 2 3 4 5 6)) ;((1 4) (6 3) (2 5))
(partition #'length '("it" "was" "not" "in" "the" "room")) ;(("it" "in") ("room") ("was" "not" "the"))