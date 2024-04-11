;(set-option :produce-models true)
(set-logic ALL)
(set-info :status sat)
(declare-datatypes () (
  ( RealTree 
    ( Node 
      (left RealTree) 
		  (elem Real) 
		  (right RealTree)) 
    (Leaf)
   )
))


; (declare-catamorphism SumTree (RealTree) Real)
(define-fun-rec SumTree ((foo RealTree)) Real 
	(ite 
		(is-Leaf foo) 
		0.0
		(+ (SumTree (left foo)) (elem foo) (SumTree (right foo)))
		))

(declare-fun v1 () Real)
(declare-fun l1 () RealTree)
(declare-fun l2 () RealTree)


(assert (= l1 (Node l2 5.0 l2)))
(assert (= (SumTree l1) 5.0))
(check-sat)
;(print-model)
(exit)