(set-logic ALL)
(set-info :source | SMT-COMP'06 organizers |)
(set-info :smt-lib-version 2.0)
(set-info :category "check")
(set-info :status sat)

; (declare-datatype RealTree)
(declare-datatypes ((Even 0) (Odd 0)) (
  ((ECons (EElem Real) (OTl Odd)) (ENil))
  ((OCons (OElem Real) (ETl Even)) (ONil))))

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
(declare-fun o1 () Odd)
(declare-fun o2 () Odd)

;satisfiable
(assert (= e1 (ECons 5.0 o2)))
(assert (= o1 (OCons 5.0 e2)))
(assert (= (SumEven e1) 5.0))
;(assert (= (SumEven e1) (SumOdd o1)))
(check-sat)
(exit)
