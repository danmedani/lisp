; Reverse List Funcs
(defun revlist_ (list new)
       ( COND 
       	 ( (EQ () list) new)
	 ( T (revlist_ (CDR list) (CONS (CAR list) new)))
	 ))
(defun revlist (list) 
       ( revlist_ list () ))

; Concat Funcs
(defun concat_ (revone two) 
       ( COND
       	 ( (EQ () revone) two)
	 ( T (concat_ (CDR revone) (CONS (CAR revone) two)))))
(defun concat ( one two ) 
       ( concat_ (revlist one) two))

; Quick Sort Helpers
(defun lqsort_ ( left pivot list )
  ( COND
    ( (EQ () list) left )
    ( (> pivot (car list)) (lqsort_ (CONS (car list) left) pivot (CDR list)))
    ( T (lqsort_ left pivot (CDR list)))))
(defun lqsort ( pivot list )
  ( lqsort_ () pivot list))
(defun rqsort_ ( right pivot list )
  ( COND
    ( (EQ () list) right )
    ( (<= pivot (car list)) (rqsort_ (CONS (car list) right) pivot (CDR list)))
    ( T (rqsort_ right pivot (CDR list)))))
(defun rqsort ( pivot list )
  ( rqsort_ () pivot list))

; Quick Sort Main (Always chooses car as pivot)
(defun qsort ( list ) 
       ( COND
	 ( (EQ NIL list) ())
	 ( (EQ () list) ())
       	 ( (EQ () (cdr list)) list )
	 ( T (concat
	      (qsort (lqsort (car list) (CDR list)))
	      (concat
	       (cons (car list) ())
	       (qsort (rqsort (car list) (CDR list))))))))

; Reverse List Generators
(defun genRevList_ (list count top)
  ( COND 
    ((> count top) list)
    (T (genRevList_ (CONS count list) (+ 1 count) top))
    ))
(defun genRevList (top) (genRevList_ () 1 top))
(defun testQRev (count) (qsort (genLargeRevList count)))

; Random List Generators
(defun genRandoList_ (list count top)
  ( COND 
    ((eq count 0) list)
    (T (genRandoList_ (CONS (random top) list) (- count 1) top))
    ))
(defun genRandoList (count top) (genRandoList_ () count top))
(defun testQRando (count top) (qsort (genRandoList count top)))
