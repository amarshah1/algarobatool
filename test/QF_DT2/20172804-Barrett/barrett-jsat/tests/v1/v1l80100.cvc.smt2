(set-info :smt-lib-version 2.6)
(set-logic QF_DT)
(set-info :source |
Generated by: Andrew Reynolds
Generated on: 2017-04-28
Generator: Random, converted to v2.6 by CVC4
Application: Regressions for datatypes decision procedure.
Target solver: CVC3
Publications: "An Abstract Decision Procedure for Satisfiability in the Theory of Inductive Data Types" by Clark Barrett, Igor Shikanian, and Cesare Tinelli, Journal on Satisfiability, Boolean Modeling and Computation 2007.
|)
(set-info :license "https://creativecommons.org/licenses/by/4.0/")
(set-info :category "random")
(set-info :status sat)


(declare-datatypes ((nat 0)(list 0)(tree 0)) (((succ (pred nat)) (zero))
((cons (car tree) (cdr list)) (null))
((node (children list)) (leaf (data nat)))
))
(declare-fun x1 () nat)
(declare-fun x2 () list)
(declare-fun x3 () tree)

(assert (and (and (and (and (and (and (and (= (car null) (node null)) (= (cdr (cdr (children x3))) null)) (is-leaf (leaf x1))) (not (is-leaf x3))) (not ((_ is zero) (data x3)))) (= x2 (children (car (cdr x2))))) (not ((_ is node) (leaf (pred (succ x1)))))) (not (= (cons (leaf (succ x1)) null) (cons (car (cons (node (cons (node x2) x2)) x2)) (children (car x2)))))))
(check-sat)
(exit)


