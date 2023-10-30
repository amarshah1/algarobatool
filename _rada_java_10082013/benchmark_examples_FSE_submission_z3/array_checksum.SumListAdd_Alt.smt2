;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Datatype declarations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare-datatypes () 
  ((ArrayList (ArrayList_nil) (ArrayList_cons (destArrayList_cons ArrayList_cons_recd)))

   (ArrayList_cons_recd (ArrayList_cons_recd (ArrayList_cons_recd_hd (Array Int Int) )
  (ArrayList_cons_recd_tl ArrayList)))
))

(declare-datatypes () 
  ((ArrayTree (ArrayTree_Leaf (destArrayTree_Leaf (Array Int Int) )) (ArrayTree_Node (destArrayTree_Node ArrayTree_Node_recd)))

   (ArrayTree_Node_recd (ArrayTree_Node_recd (ArrayTree_Node_recd_left ArrayTree)
  (ArrayTree_Node_recd_elem Int) (ArrayTree_Node_recd_right ArrayTree)))
))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Uninterpreted symbols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare-fun L () ArrayList)
(declare-fun v0 () ArrayList)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Catamorphisms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-fun-rec SumElementsFn ((A (Array Int Int))) Int
  (+ (+ (+ (+ (select A 0) (select A 1)) (select A 2)) (select A 3)) (select A 4)))

(define-fun-rec concatFn ((L1 ArrayList) (L2 ArrayList)) ArrayList
  (ite (is-ArrayList_nil L1) L2 (ArrayList_cons (ArrayList_cons_recd (ArrayList_cons_recd_hd (destArrayList_cons L1)) (concatFn (ArrayList_cons_recd_tl (destArrayList_cons L1)) L2)))))

(define-fun-rec TreeListFn ((t ArrayTree)) ArrayList
  (ite (is-ArrayTree_Leaf t) (ArrayList_cons (ArrayList_cons_recd (destArrayTree_Leaf t) ArrayList_nil)) (concatFn (TreeListFn (ArrayTree_Node_recd_left (destArrayTree_Node t))) (TreeListFn (ArrayTree_Node_recd_right (destArrayTree_Node t))))))

(define-fun-rec SumTreeFn ((t ArrayTree)) Int
  (ite (is-ArrayTree_Leaf t) (SumElementsFn (destArrayTree_Leaf t)) (+ (SumTreeFn (ArrayTree_Node_recd_left (destArrayTree_Node t))) (SumTreeFn (ArrayTree_Node_recd_right (destArrayTree_Node t))))))

(define-fun-rec SumListFn ((L ArrayList)) Int
  (ite (is-ArrayList_nil L) 0 (+ (SumElementsFn (ArrayList_cons_recd_hd (destArrayList_cons L))) (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons L))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Goals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(push)
(assert (and (is-ArrayList_nil L) (and (is-ArrayList_nil v0) (and (not (is-ArrayList_nil (concatFn L v0))) (not (= (SumElementsFn (ArrayList_cons_recd_hd (destArrayList_cons (concatFn L v0)))) 0))))))
(check-sat)
(pop)

(push)
(assert (and (is-ArrayList_nil L) (and (is-ArrayList_nil v0) (and (not (is-ArrayList_nil (concatFn L v0))) (not (= (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons (concatFn L v0)))) 0))))))
(check-sat)
(pop)

(push)
(assert (and (is-ArrayList_nil L) (and (not (is-ArrayList_nil v0)) (and (is-ArrayList_nil (concatFn L v0)) (not (= (SumElementsFn (ArrayList_cons_recd_hd (destArrayList_cons v0))) 0))))))
(check-sat)
(pop)

(push)
(assert (and (is-ArrayList_nil L) (and (not (is-ArrayList_nil v0)) (and (is-ArrayList_nil (concatFn L v0)) (not (= (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons v0))) 0))))))
(check-sat)
(pop)

(push)
(assert (and (is-ArrayList_nil L) (and (not (is-ArrayList_nil v0)) (and (not (is-ArrayList_nil (concatFn L v0))) (not (= (+ (SumElementsFn (ArrayList_cons_recd_hd (destArrayList_cons v0))) (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons v0)))) (+ (SumElementsFn (ArrayList_cons_recd_hd (destArrayList_cons (concatFn L v0)))) (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons (concatFn L v0)))))))))))
(check-sat)
(pop)

(push)
(assert (and (not (is-ArrayList_nil L)) (and (is-ArrayList_nil v0) (and (is-ArrayList_nil (concatFn L v0)) (and (= (+ (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons L))) (SumListFn (concatFn L v0))) (SumListFn (concatFn (ArrayList_cons_recd_tl (destArrayList_cons L)) (concatFn L v0)))) (and (= (+ (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons L))) (SumListFn v0)) (SumListFn (concatFn (ArrayList_cons_recd_tl (destArrayList_cons L)) v0))) (and (= (+ (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons L))) (SumListFn L)) (SumListFn (concatFn (ArrayList_cons_recd_tl (destArrayList_cons L)) L))) (and (= (+ (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons L))) (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons L)))) (SumListFn (concatFn (ArrayList_cons_recd_tl (destArrayList_cons L)) (ArrayList_cons_recd_tl (destArrayList_cons L))))) (not (= (SumElementsFn (ArrayList_cons_recd_hd (destArrayList_cons L))) 0))))))))))
(check-sat)
(pop)

(push)
(assert (and (not (is-ArrayList_nil L)) (and (is-ArrayList_nil v0) (and (is-ArrayList_nil (concatFn L v0)) (and (= (+ (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons L))) (SumListFn (concatFn L v0))) (SumListFn (concatFn (ArrayList_cons_recd_tl (destArrayList_cons L)) (concatFn L v0)))) (and (= (+ (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons L))) (SumListFn v0)) (SumListFn (concatFn (ArrayList_cons_recd_tl (destArrayList_cons L)) v0))) (and (= (+ (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons L))) (SumListFn L)) (SumListFn (concatFn (ArrayList_cons_recd_tl (destArrayList_cons L)) L))) (and (= (+ (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons L))) (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons L)))) (SumListFn (concatFn (ArrayList_cons_recd_tl (destArrayList_cons L)) (ArrayList_cons_recd_tl (destArrayList_cons L))))) (not (= (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons L))) 0))))))))))
(check-sat)
(pop)

(push)
(assert (and (not (is-ArrayList_nil L)) (and (is-ArrayList_nil v0) (and (not (is-ArrayList_nil (concatFn L v0))) (and (= (+ (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons L))) (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons (concatFn L v0))))) (SumListFn (concatFn (ArrayList_cons_recd_tl (destArrayList_cons L)) (ArrayList_cons_recd_tl (destArrayList_cons (concatFn L v0)))))) (and (= (+ (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons L))) (SumListFn (concatFn L v0))) (SumListFn (concatFn (ArrayList_cons_recd_tl (destArrayList_cons L)) (concatFn L v0)))) (and (= (+ (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons L))) (SumListFn v0)) (SumListFn (concatFn (ArrayList_cons_recd_tl (destArrayList_cons L)) v0))) (and (= (+ (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons L))) (SumListFn L)) (SumListFn (concatFn (ArrayList_cons_recd_tl (destArrayList_cons L)) L))) (and (= (+ (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons L))) (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons L)))) (SumListFn (concatFn (ArrayList_cons_recd_tl (destArrayList_cons L)) (ArrayList_cons_recd_tl (destArrayList_cons L))))) (not (= (+ (SumElementsFn (ArrayList_cons_recd_hd (destArrayList_cons L))) (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons L)))) (+ (SumElementsFn (ArrayList_cons_recd_hd (destArrayList_cons (concatFn L v0)))) (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons (concatFn L v0))))))))))))))))
(check-sat)
(pop)

(push)
(assert (and (not (is-ArrayList_nil L)) (and (not (is-ArrayList_nil v0)) (and (is-ArrayList_nil (concatFn L v0)) (and (= (+ (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons L))) (SumListFn (concatFn L v0))) (SumListFn (concatFn (ArrayList_cons_recd_tl (destArrayList_cons L)) (concatFn L v0)))) (and (= (+ (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons L))) (SumListFn v0)) (SumListFn (concatFn (ArrayList_cons_recd_tl (destArrayList_cons L)) v0))) (and (= (+ (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons L))) (SumListFn L)) (SumListFn (concatFn (ArrayList_cons_recd_tl (destArrayList_cons L)) L))) (and (= (+ (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons L))) (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons L)))) (SumListFn (concatFn (ArrayList_cons_recd_tl (destArrayList_cons L)) (ArrayList_cons_recd_tl (destArrayList_cons L))))) (not (= (SumElementsFn (ArrayList_cons_recd_hd (destArrayList_cons L))) 0))))))))))
(check-sat)
(pop)

(push)
(assert (and (not (is-ArrayList_nil L)) (and (not (is-ArrayList_nil v0)) (and (is-ArrayList_nil (concatFn L v0)) (and (= (+ (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons L))) (SumListFn (concatFn L v0))) (SumListFn (concatFn (ArrayList_cons_recd_tl (destArrayList_cons L)) (concatFn L v0)))) (and (= (+ (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons L))) (SumListFn v0)) (SumListFn (concatFn (ArrayList_cons_recd_tl (destArrayList_cons L)) v0))) (and (= (+ (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons L))) (SumListFn L)) (SumListFn (concatFn (ArrayList_cons_recd_tl (destArrayList_cons L)) L))) (and (= (+ (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons L))) (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons L)))) (SumListFn (concatFn (ArrayList_cons_recd_tl (destArrayList_cons L)) (ArrayList_cons_recd_tl (destArrayList_cons L))))) (not (= (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons L))) 0))))))))))
(check-sat)
(pop)

(push)
(assert (and (not (is-ArrayList_nil L)) (and (not (is-ArrayList_nil v0)) (and (is-ArrayList_nil (concatFn L v0)) (and (= (+ (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons L))) (SumListFn (concatFn L v0))) (SumListFn (concatFn (ArrayList_cons_recd_tl (destArrayList_cons L)) (concatFn L v0)))) (and (= (+ (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons L))) (SumListFn v0)) (SumListFn (concatFn (ArrayList_cons_recd_tl (destArrayList_cons L)) v0))) (and (= (+ (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons L))) (SumListFn L)) (SumListFn (concatFn (ArrayList_cons_recd_tl (destArrayList_cons L)) L))) (and (= (+ (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons L))) (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons L)))) (SumListFn (concatFn (ArrayList_cons_recd_tl (destArrayList_cons L)) (ArrayList_cons_recd_tl (destArrayList_cons L))))) (not (= (SumElementsFn (ArrayList_cons_recd_hd (destArrayList_cons v0))) 0))))))))))
(check-sat)
(pop)

(push)
(assert (and (not (is-ArrayList_nil L)) (and (not (is-ArrayList_nil v0)) (and (is-ArrayList_nil (concatFn L v0)) (and (= (+ (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons L))) (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons v0)))) (SumListFn (concatFn (ArrayList_cons_recd_tl (destArrayList_cons L)) (ArrayList_cons_recd_tl (destArrayList_cons v0))))) (and (= (+ (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons L))) (SumListFn (concatFn L v0))) (SumListFn (concatFn (ArrayList_cons_recd_tl (destArrayList_cons L)) (concatFn L v0)))) (and (= (+ (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons L))) (SumListFn v0)) (SumListFn (concatFn (ArrayList_cons_recd_tl (destArrayList_cons L)) v0))) (and (= (+ (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons L))) (SumListFn L)) (SumListFn (concatFn (ArrayList_cons_recd_tl (destArrayList_cons L)) L))) (and (= (+ (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons L))) (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons L)))) (SumListFn (concatFn (ArrayList_cons_recd_tl (destArrayList_cons L)) (ArrayList_cons_recd_tl (destArrayList_cons L))))) (not (= (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons v0))) 0)))))))))))
(check-sat)
(pop)

(push)
(assert (and (not (is-ArrayList_nil L)) (and (not (is-ArrayList_nil v0)) (and (not (is-ArrayList_nil (concatFn L v0))) (and (= (+ (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons L))) (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons v0)))) (SumListFn (concatFn (ArrayList_cons_recd_tl (destArrayList_cons L)) (ArrayList_cons_recd_tl (destArrayList_cons v0))))) (and (= (+ (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons L))) (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons (concatFn L v0))))) (SumListFn (concatFn (ArrayList_cons_recd_tl (destArrayList_cons L)) (ArrayList_cons_recd_tl (destArrayList_cons (concatFn L v0)))))) (and (= (+ (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons L))) (SumListFn (concatFn L v0))) (SumListFn (concatFn (ArrayList_cons_recd_tl (destArrayList_cons L)) (concatFn L v0)))) (and (= (+ (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons L))) (SumListFn v0)) (SumListFn (concatFn (ArrayList_cons_recd_tl (destArrayList_cons L)) v0))) (and (= (+ (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons L))) (SumListFn L)) (SumListFn (concatFn (ArrayList_cons_recd_tl (destArrayList_cons L)) L))) (and (= (+ (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons L))) (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons L)))) (SumListFn (concatFn (ArrayList_cons_recd_tl (destArrayList_cons L)) (ArrayList_cons_recd_tl (destArrayList_cons L))))) (not (= (+ (+ (SumElementsFn (ArrayList_cons_recd_hd (destArrayList_cons L))) (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons L)))) (+ (SumElementsFn (ArrayList_cons_recd_hd (destArrayList_cons v0))) (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons v0))))) (+ (SumElementsFn (ArrayList_cons_recd_hd (destArrayList_cons (concatFn L v0)))) (SumListFn (ArrayList_cons_recd_tl (destArrayList_cons (concatFn L v0)))))))))))))))))
(check-sat)
(pop)
