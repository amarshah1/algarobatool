;; universe for nat:
;;   nat!val!1 nat!val!0 
;; -----------
;; definitions for universe elements:
(set-logic ALL)

(declare-sort nat)
(declare-sort list)
(declare-sort tree)

(declare-fun nat!val!1 () nat)
(declare-fun nat!val!0 () nat)
;; cardinality constraint:
;; -----------
;; universe for list:
;;   list!val!2 list!val!8 list!val!9 list!val!10 list!val!0 list!val!1 list!val!6 list!val!7 list!val!3 list!val!5 list!val!4 
;; -----------
;; definitions for universe elements:
(declare-fun list!val!2 () list)
(declare-fun list!val!8 () list)
(declare-fun list!val!9 () list)
(declare-fun list!val!10 () list)
(declare-fun list!val!0 () list)
(declare-fun list!val!1 () list)
(declare-fun list!val!6 () list)
(declare-fun list!val!7 () list)
(declare-fun list!val!3 () list)
(declare-fun list!val!5 () list)
(declare-fun list!val!4 () list)
;; cardinality constraint:

;; -----------
;; universe for tree:
;;   tree!val!4 tree!val!8 tree!val!9 tree!val!2 tree!val!3 tree!val!6 tree!val!5 tree!val!7 tree!val!0 tree!val!1 
;; -----------
;; definitions for universe elements:
(declare-fun tree!val!4 () tree)
(declare-fun tree!val!8 () tree)
(declare-fun tree!val!9 () tree)
(declare-fun tree!val!2 () tree)
(declare-fun tree!val!3 () tree)
(declare-fun tree!val!6 () tree)
(declare-fun tree!val!5 () tree)
(declare-fun tree!val!7 () tree)
(declare-fun tree!val!0 () tree)
(declare-fun tree!val!1 () tree)
;; cardinality constraint:

;; -----------
(define-fun contrived_variable3 () tree
  tree!val!3)
(define-fun contrived_variable10 () tree
  tree!val!4)
(define-fun contrived_variable12 () tree
  tree!val!5)
(define-fun contrived_variable5 () list
  list!val!4)
(define-fun contrived_variable15 () list
  list!val!0)
(define-fun contrived_variable9 () list
  list!val!2)
(define-fun contrived_variable8 () list
  list!val!0)
(define-fun contrived_variable24 () tree
  tree!val!9)
(define-fun contrived_variable19 () list
  list!val!5)
(define-fun contrived_variable14 () tree
  tree!val!6)
(define-fun contrived_variable13 () list
  list!val!3)
(define-fun x3 () tree
  tree!val!2)
(define-fun contrived_variable20 () tree
  tree!val!3)
(define-fun contrived_variable1 () tree
  tree!val!0)
(define-fun contrived_variable17 () list
  list!val!0)
(define-fun contrived_variable21 () list
  list!val!6)
(define-fun contrived_variable16 () tree
  tree!val!4)
(define-fun x2 () list
  list!val!0)
(define-fun null () list
  list!val!0)
(define-fun contrived_variable25 () list
  list!val!9)
(define-fun contrived_variable23 () list
  list!val!8)
(define-fun contrived_variable2 () list
  list!val!1)
(define-fun contrived_variable18 () tree
  tree!val!7)
(define-fun contrived_variable7 () list
  list!val!1)
(define-fun zero () nat
  nat!val!0)
(define-fun contrived_variable4 () list
  list!val!5)
(define-fun contrived_variable11 () list
  list!val!0)
(define-fun contrived_variable0 () tree
  tree!val!1)
(define-fun contrived_variable6 () tree
  tree!val!4)
(define-fun contrived_variable22 () list
  list!val!7)
(define-fun is-leaf ((x!0 tree)) Bool
  (ite (= x!0 tree!val!5) false
  (ite (= x!0 tree!val!9) false
  (ite (= x!0 tree!val!0) false
    true))))
(define-fun is-cons ((x!0 list)) Bool
  (ite (= x!0 list!val!0) false
    true))
(define-fun is-succ ((x!0 nat)) Bool
  false)
(define-fun children ((x!0 tree)) list
  (ite (= x!0 tree!val!8) list!val!2
  (ite (= x!0 tree!val!4) list!val!4
  (ite (= x!0 tree!val!1) list!val!10
    list!val!0))))
(define-fun cons ((x!0 tree) (x!1 list)) list
  (ite (and (= x!0 tree!val!4) (= x!1 list!val!0)) list!val!1
  (ite (and (= x!0 tree!val!7) (= x!1 list!val!5)) list!val!4
  (ite (and (= x!0 tree!val!3) (= x!1 list!val!6)) list!val!5
    list!val!0))))
(define-fun is-zero ((x!0 nat)) Bool
  true)
(define-fun car ((x!0 list)) tree
  (ite (= x!0 list!val!1) tree!val!4
  (ite (= x!0 list!val!4) tree!val!7
  (ite (= x!0 list!val!5) tree!val!3
    tree!val!1))))
(define-fun cdr ((x!0 list)) list
  (ite (= x!0 list!val!4) list!val!5
  (ite (= x!0 list!val!5) list!val!6
  (ite (= x!0 list!val!0) list!val!1
    list!val!0))))
(define-fun data ((x!0 tree)) nat
  nat!val!1)
(define-fun leaf ((x!0 nat)) tree
  tree!val!8)
(define-fun is-null ((x!0 list)) Bool
  (ite (= x!0 list!val!0) true
    false))
(define-fun is-node ((x!0 tree)) Bool
  (ite (= x!0 tree!val!0) true
  (ite (= x!0 tree!val!5) true
  (ite (= x!0 tree!val!9) true
    false))))
(define-fun node ((x!0 list)) tree
  (ite (= x!0 list!val!7) tree!val!8
  (ite (= x!0 list!val!8) tree!val!4
    tree!val!0)))
    

;the guards we used where(is-cons contrived_variable7)
    
    
;    adding the axioms: (assert (and (not (is-succ zero)) (is-zero zero)))
(assert (and (not (is-cons null)) (is-null null)))
(assert (and true
     (not (is-cons x2))
     (not (= contrived_variable1 contrived_variable0))
     (is-leaf x3)
     (is-cons contrived_variable2)
     (is-leaf contrived_variable3)
     (not (= x2 contrived_variable9))))
(assert (let ((a!1 (=> (is-cons contrived_variable7)
               (and (= (car contrived_variable7) contrived_variable10)
                    (= (cdr contrived_variable7) contrived_variable11)))))
  (and (= (car contrived_variable7) contrived_variable6)
       (= (cons contrived_variable10 contrived_variable11) contrived_variable7)
       a!1)))
(assert (and (= (node null) contrived_variable1)
     (is-node contrived_variable1)
     (= (children contrived_variable1) null)))
(assert (let ((a!1 (=> (is-cons contrived_variable8)
               (and (= (car contrived_variable8) contrived_variable12)
                    (= (cdr contrived_variable8) contrived_variable13)))))
  (and (= (cdr contrived_variable8) contrived_variable7)
       (= (cons contrived_variable12 contrived_variable13) contrived_variable8)
       a!1)))
(assert (let ((a!1 (=> (is-cons null)
               (and (= (car null) contrived_variable14)
                    (= (cdr null) contrived_variable15)))))
  (and (= (car null) contrived_variable0)
       (= (cons contrived_variable14 contrived_variable15) null)
       a!1)))
(assert (let ((a!1 (=> (is-cons contrived_variable2)
               (and (= (car contrived_variable2) contrived_variable16)
                    (= (cdr contrived_variable2) contrived_variable17)))))
  (and (= (cdr contrived_variable2) contrived_variable8)
       (= (cons contrived_variable16 contrived_variable17) contrived_variable2)
       a!1)))
(assert (let ((a!1 (=> (is-cons contrived_variable5)
               (and (= (car contrived_variable5) contrived_variable18)
                    (= (cdr contrived_variable5) contrived_variable19)))))
  (and (= (cdr contrived_variable5) contrived_variable4)
       (= (cons contrived_variable18 contrived_variable19) contrived_variable5)
       a!1)))
(assert (let ((a!1 (=> (is-cons contrived_variable4)
               (and (= (car contrived_variable4) contrived_variable20)
                    (= (cdr contrived_variable4) contrived_variable21)))))
  (and (= (car contrived_variable4) contrived_variable3)
       (= (cons contrived_variable20 contrived_variable21) contrived_variable4)
       a!1)))
(assert (let ((a!1 (= (children (leaf (data x3))) contrived_variable9))
      (a!2 (= (children (leaf (data x3))) contrived_variable22)))
(let ((a!3 (=> (is-node (leaf (data x3))) (and a!2))))
  (and a!1 (= (node contrived_variable22) (leaf (data x3))) a!3))))
(assert (let ((a!1 (=> (is-node contrived_variable6)
               (and (= (children contrived_variable6) contrived_variable23)))))
  (and (= (children contrived_variable6) contrived_variable5)
       (= (node contrived_variable23) contrived_variable6)
       a!1)))
(assert (let ((a!1 (=> (is-cons null)
               (and (= (car null) contrived_variable24)
                    (= (cdr null) contrived_variable25)))))
  (and (= (cdr null) contrived_variable2)
       (= (cons contrived_variable24 contrived_variable25) null)
       a!1)))
(assert (or (and (not (is-cons contrived_variable22)) (is-null contrived_variable22))
    (and (is-cons contrived_variable22) (not (is-null contrived_variable22)))))
(assert (=> (is-null contrived_variable22) (= contrived_variable22 null)))
(assert (or (and (not (is-cons contrived_variable17)) (is-null contrived_variable17))
    (and (is-cons contrived_variable17) (not (is-null contrived_variable17)))))
(assert (=> (is-null contrived_variable17) (= contrived_variable17 null)))
(assert (or (and (not (is-cons contrived_variable21)) (is-null contrived_variable21))
    (and (is-cons contrived_variable21) (not (is-null contrived_variable21)))))
(assert (=> (is-null contrived_variable21) (= contrived_variable21 null)))
(assert (or (and (not (is-cons contrived_variable23)) (is-null contrived_variable23))
    (and (is-cons contrived_variable23) (not (is-null contrived_variable23)))))
(assert (=> (is-null contrived_variable23) (= contrived_variable23 null)))
(assert (or (and (not (is-cons contrived_variable19)) (is-null contrived_variable19))
    (and (is-cons contrived_variable19) (not (is-null contrived_variable19)))))
(assert (=> (is-null contrived_variable19) (= contrived_variable19 null)))
(assert (or (and (not (is-cons contrived_variable25)) (is-null contrived_variable25))
    (and (is-cons contrived_variable25) (not (is-null contrived_variable25)))))
(assert (=> (is-null contrived_variable25) (= contrived_variable25 null)))
(assert (or (and (not (is-cons contrived_variable11)) (is-null contrived_variable11))
    (and (is-cons contrived_variable11) (not (is-null contrived_variable11)))))
(assert (=> (is-null contrived_variable11) (= contrived_variable11 null)))
(assert (or (and (not (is-cons contrived_variable15)) (is-null contrived_variable15))
    (and (is-cons contrived_variable15) (not (is-null contrived_variable15)))))
(assert (=> (is-null contrived_variable15) (= contrived_variable15 null)))
(assert (or (and (not (is-cons contrived_variable13)) (is-null contrived_variable13))
    (and (is-cons contrived_variable13) (not (is-null contrived_variable13)))))
(assert (=> (is-null contrived_variable13) (= contrived_variable13 null)))
(assert (or (and (not (is-cons contrived_variable2)) (is-null contrived_variable2))
    (and (is-cons contrived_variable2) (not (is-null contrived_variable2)))))
(assert (=> (is-null contrived_variable2) (= contrived_variable2 null)))
(assert (or (and (not (is-cons contrived_variable5)) (is-null contrived_variable5))
    (and (is-cons contrived_variable5) (not (is-null contrived_variable5)))))
(assert (=> (is-null contrived_variable5) (= contrived_variable5 null)))
(assert (or (and (not (is-cons contrived_variable9)) (is-null contrived_variable9))
    (and (is-cons contrived_variable9) (not (is-null contrived_variable9)))))
(assert (=> (is-null contrived_variable9) (= contrived_variable9 null)))
(assert (or (and (not (is-cons contrived_variable4)) (is-null contrived_variable4))
    (and (is-cons contrived_variable4) (not (is-null contrived_variable4)))))
(assert (=> (is-null contrived_variable4) (= contrived_variable4 null)))
(assert (or (and (not (is-cons contrived_variable8)) (is-null contrived_variable8))
    (and (is-cons contrived_variable8) (not (is-null contrived_variable8)))))
(assert (=> (is-null contrived_variable8) (= contrived_variable8 null)))
(assert (or (and (not (is-cons contrived_variable7)) (is-null contrived_variable7))
    (and (is-cons contrived_variable7) (not (is-null contrived_variable7)))))
(assert (=> (is-null contrived_variable7) (= contrived_variable7 null)))
(assert (or (and (not (is-cons x2)) (is-null x2)) (and (is-cons x2) (not (is-null x2)))))
(assert (=> (is-null x2) (= x2 null)))
(assert (or (and (not (is-node contrived_variable12)) (is-leaf contrived_variable12))
    (and (is-node contrived_variable12) (not (is-leaf contrived_variable12)))))
(assert (or (and (not (is-node contrived_variable14)) (is-leaf contrived_variable14))
    (and (is-node contrived_variable14) (not (is-leaf contrived_variable14)))))
(assert (or (and (not (is-node contrived_variable16)) (is-leaf contrived_variable16))
    (and (is-node contrived_variable16) (not (is-leaf contrived_variable16)))))
(assert (or (and (not (is-node contrived_variable10)) (is-leaf contrived_variable10))
    (and (is-node contrived_variable10) (not (is-leaf contrived_variable10)))))
(assert (or (and (not (is-node contrived_variable20)) (is-leaf contrived_variable20))
    (and (is-node contrived_variable20) (not (is-leaf contrived_variable20)))))
(assert (or (and (not (is-node contrived_variable18)) (is-leaf contrived_variable18))
    (and (is-node contrived_variable18) (not (is-leaf contrived_variable18)))))
(assert (or (and (not (is-node contrived_variable24)) (is-leaf contrived_variable24))
    (and (is-node contrived_variable24) (not (is-leaf contrived_variable24)))))
(assert (or (and (not (is-node contrived_variable3)) (is-leaf contrived_variable3))
    (and (is-node contrived_variable3) (not (is-leaf contrived_variable3)))))
(assert (or (and (not (is-node contrived_variable0)) (is-leaf contrived_variable0))
    (and (is-node contrived_variable0) (not (is-leaf contrived_variable0)))))
(assert (or (and (not (is-node contrived_variable1)) (is-leaf contrived_variable1))
    (and (is-node contrived_variable1) (not (is-leaf contrived_variable1)))))
(assert (or (and (not (is-node contrived_variable6)) (is-leaf contrived_variable6))
    (and (is-node contrived_variable6) (not (is-leaf contrived_variable6)))))
(assert (or (and (not (is-node x3)) (is-leaf x3)) (and (is-node x3) (not (is-leaf x3)))))
(assert (=> (and (is-cons contrived_variable4))
    (not (= (cdr contrived_variable4) contrived_variable4))))
(assert (=> (and (is-cons contrived_variable2))
    (not (= (cdr contrived_variable2) contrived_variable2))))
(assert (let ((a!1 (not (= (cdr (cdr contrived_variable2)) contrived_variable2))))
  (=> (and (is-cons (cdr contrived_variable2)) (is-cons contrived_variable2))
      a!1)))
(assert (let ((a!1 (not (= (children (car contrived_variable2)) contrived_variable2))))
  (=> (and (is-node (car contrived_variable2)) (is-cons contrived_variable2))
      a!1)))
(assert (=> (and (is-cons contrived_variable2))
    (not (= (cdr contrived_variable2) contrived_variable2))))
(assert (let ((a!1 (not (= (cdr (cdr contrived_variable2)) contrived_variable2))))
  (=> (and (is-cons (cdr contrived_variable2)) (is-cons contrived_variable2))
      a!1)))
(assert (let ((a!1 (and (is-cons (cdr (cdr contrived_variable2)))
                (is-cons (cdr contrived_variable2))
                (is-cons contrived_variable2)))
      (a!2 (= (cdr (cdr (cdr contrived_variable2))) contrived_variable2)))
  (=> a!1 (not a!2))))
(assert (let ((a!1 (and (is-node (car (cdr contrived_variable2)))
                (is-cons (cdr contrived_variable2))
                (is-cons contrived_variable2)))
      (a!2 (= (children (car (cdr contrived_variable2))) contrived_variable2)))
  (=> a!1 (not a!2))))
(assert (let ((a!1 (not (= (children (car contrived_variable2)) contrived_variable2))))
  (=> (and (is-node (car contrived_variable2)) (is-cons contrived_variable2))
      a!1)))
(assert (let ((a!1 (and (is-cons (children (car contrived_variable2)))
                (is-node (car contrived_variable2))
                (is-cons contrived_variable2)))
      (a!2 (= (cdr (children (car contrived_variable2))) contrived_variable2)))
  (=> a!1 (not a!2))))
(assert (=> (and (is-cons contrived_variable7))
    (not (= (cdr contrived_variable7) contrived_variable7))))

(assert (and (is-cons contrived_variable7) (= (cdr contrived_variable7) contrived_variable7)))

(check-sat)