;(set-option :produce-models true)
(set-logic ALL)
(declare-datatypes () (
  (RealTree
    (Node
      (left RealTree)
      (elem Real)
      (right RealTree))
    (Leaf))))

    
; definition of is-bad: suppose an element value is bad if it is equal to 1.0
(define-fun is-negative ((e Real)) Bool
  (ite (= e 1.0)
       true
       false))
(define-fun is-bad ((e Real)) Bool
  (is-negative e))

(define-fun pr ((e Real)) Bool
  (is-bad e))

(define-fun delta_ngn ((e Real)) Int
  1)

;;
;; Definition of PAC catamorphism NGN
;;
(define-fun-rec PAC_ngn ((foo RealTree) (cleaf_ngn Int) (cpr_ngn Int) (rec Bool)) Int
  (ite (is-Leaf foo)
       cleaf_ngn
       (ite (not (pr (elem foo)))
            (+ (PAC_ngn  (left foo) cleaf_ngn cpr_ngn rec)
               (delta_ngn (elem foo))
               (PAC_ngn (right foo) cleaf_ngn cpr_ngn rec))
            (ite rec
                 (+ (PAC_ngn  (left foo) cleaf_ngn cpr_ngn rec)
                    cpr_ngn
                    (PAC_ngn (right foo) cleaf_ngn cpr_ngn rec))
                 cpr_ngn))))

;;
;; Ngn is a PAC catamorphism with
;;   + delta(e) = 1
;;   + cleaf = 0
;;   + cpr = 0
;;   + pr = is-bad
;;   + rec = false
;;
(declare-fun cleaf_ngn_const () Int)
(declare-fun cpr_ngn_const () Int)
(declare-fun rec_const () Bool)
(assert (= cleaf_ngn_const 0))
(assert (= cpr_ngn_const 0))
(assert (= rec_const false))


;;
;; Suppose we want to compute the number of good nodes in tree t1:
;;
(declare-fun t1 () RealTree)
(assert (= t1 (Node
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
                    (Node
                      (Node Leaf 5.0 Leaf)
                      1.0
                      (Node Leaf 2.0 Leaf)))
                  11.0
                  Leaf)
                12.0
                Leaf)))

; the result is SAT: there are 4 good nodes in t1 (including 12.0, 11.0, 7.0, and 3.0)
(assert (= (PAC_ngn t1 cleaf_ngn_const cpr_ngn_const rec_const) 4))
(check-sat)
(exit)