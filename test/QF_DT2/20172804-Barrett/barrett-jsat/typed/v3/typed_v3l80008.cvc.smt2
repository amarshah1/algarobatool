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
(declare-fun x2 () nat)
(declare-fun x3 () nat)
(declare-fun x4 () list)
(declare-fun x5 () list)
(declare-fun x6 () list)
(declare-fun x7 () tree)
(declare-fun x8 () tree)
(declare-fun x9 () tree)

(assert (and (and (and (and (and (and (and (not (= (ite ((_ is succ) x2) (pred x2) zero) (ite ((_ is succ) (succ (ite (is-leaf (leaf (succ x3))) (data (leaf (succ x3))) zero))) (pred (succ (ite (is-leaf (leaf (succ x3))) (data (leaf (succ x3))) zero))) zero))) (= x1 zero)) (not (= x2 x1))) (not ((_ is succ) x3))) (= x3 (ite ((_ is succ) (ite ((_ is succ) (ite ((_ is succ) (succ zero)) (pred (succ zero)) zero)) (pred (ite ((_ is succ) (succ zero)) (pred (succ zero)) zero)) zero)) (pred (ite ((_ is succ) (ite ((_ is succ) (succ zero)) (pred (succ zero)) zero)) (pred (ite ((_ is succ) (succ zero)) (pred (succ zero)) zero)) zero)) zero))) ((_ is zero) x3)) (is-leaf x9)) (= x3 x1)))
(check-sat)
(exit)


