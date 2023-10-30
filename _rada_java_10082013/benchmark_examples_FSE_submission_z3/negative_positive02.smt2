;(set-option :produce-models true)
(set-logic ALL)
;(set-info :status unsat)
(declare-datatypes () (
  ( RealTree
    ( Node
      (left RealTree)
      (elem Real)
      (right RealTree))
    (Leaf))))

(declare-datatypes (T1 T2) ((Pair (mk-pair (first T1) (second T2)))))

; Maps a tree to a pair of boolean values, in which the first element indicates
; if the tree is negative and the second element shows if the tree is positive.
(define-fun-rec NegAndPosTree ((foo RealTree)) (Pair Bool Bool)
  (ite
    (is-Leaf foo)
    (mk-pair true true)
    (mk-pair
      (and (first (NegAndPosTree (left  foo)))
           (< (elem foo) 0.0)
           (first (NegAndPosTree (right foo))))
      (and (second (NegAndPosTree (left  foo)))
           (> (elem foo) 0.0)
           (second (NegAndPosTree (right foo)))))))

(declare-fun l1 () RealTree)
(declare-fun result () (Pair Bool Bool))
(assert (= l1 (Node
                (Node
                  (Node
                    (Node
                      (Node
                        (Node Leaf 5.0 Leaf)
                        1.0
                        (Node Leaf 2.0 Leaf))
                      3.0
                      Leaf)
                    7.0
                    Leaf)
                  11.0
                  Leaf)
                12.0
                Leaf)))
(assert (= result (NegAndPosTree l1)))
; unsat: negative must be false and positive must be true
(assert (not (and (not (first result)) (second result))))
(check-sat)
(exit)