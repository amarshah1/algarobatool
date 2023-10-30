(set-logic ALL)
(set-info :source | SMT-COMP'06 organizers |)
(set-info :smt-lib-version 2.0)
;(set-info :category "check")
(set-info :status unsat)

(declare-datatypes () (
  ( RealTree 
      ( Node 
	      (left RealTree) 
		  (elem Real) 
		  (right RealTree)) 
	  (Leaf)
  )
))

(define-fun-rec SumTree ((foo RealTree)) Real 
	(ite 
		(is-Leaf foo) 
		0.0
		(+ (SumTree (left foo)) (elem foo) (SumTree (right foo)))
		))

(declare-fun v1 () Real)
(declare-fun l1 () RealTree)
(declare-fun l4 () RealTree)
(declare-fun l2_1 () RealTree)
(declare-fun l2_2 () RealTree)
(declare-fun l3_1 () RealTree)
(declare-fun l3_2 () RealTree)


(assert (= l1   (Node l2_1 1.0 l2_2)))
(assert (= l2_1 (Node l2_2 2.0 l2_2)))
(assert (= l2_2 (Node l2_1 2.0 l2_1)))
(assert (= (SumTree l1) 3.0))
(check-sat)
(exit)
