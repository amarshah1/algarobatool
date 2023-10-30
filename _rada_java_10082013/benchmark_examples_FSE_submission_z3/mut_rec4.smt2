(set-logic ALL)
(set-info :source | SMT-COMP'06 organizers |)
(set-info :smt-lib-version 2.0)
(set-info :category "check")
(set-info :status unsat)

; (declare-datatype RealTree)
(declare-datatypes () (
  ( Even (ECons (EElem Real) (OTl Odd)) (ENil))
  ( Odd  (OCons (OElem Real) (ETl Even)) (ONil))))

(define-funs-rec (
  (SumOdd ((foo Odd)) Real)
  (SumEven ((foo Even)) Real))
  (
    (ite
      (is-ONil foo) 
      0.0 
      (+ (OElem foo) (SumEven (ETl foo))))
    (ite 
      (is-ENil foo) 
      0.0 
      (+ (EElem foo) (SumOdd (OTl foo)))
    )
  )
)



(declare-fun v1 () Real)
(declare-fun v2 () Real)
(declare-fun e1 () Even)
(declare-fun e2 () Even)
(declare-fun e3 () Even)
(declare-fun o1 () Odd)
(declare-fun o2 () Odd)
(declare-fun o3 () Odd)

;unsat
(assert (= e1 (ECons 5.0 o2)))
(assert (= e2 (ECons 5.0 o3)))
(assert (= o1 (OCons 5.0 e2)))
(assert (= o2 (OCons 5.0 e3)))
(assert (= (SumEven e3) 5.0))
(assert (= (SumOdd o3) 5.0))
(assert (= (SumEven e1) 8.0))
(check-sat)
(exit)
