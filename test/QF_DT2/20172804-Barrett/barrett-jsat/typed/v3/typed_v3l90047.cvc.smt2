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
(set-info :status unsat)


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

(assert (and (and (and (and (and (and (and (and (= x6 (cons (node null) x5)) (not (= x7 (ite (is-cons (cons (leaf (ite (is-leaf x8) (data x8) zero)) (ite (is-cons (cons (node x6) x4)) (cdr (cons (node x6) x4)) null))) (car (cons (leaf (ite (is-leaf x8) (data x8) zero)) (ite (is-cons (cons (node x6) x4)) (cdr (cons (node x6) x4)) null))) (leaf zero))))) ((_ is zero) (ite ((_ is succ) zero) (pred zero) zero))) (not (is-leaf x7))) (is-leaf (leaf (ite (is-leaf x8) (data x8) zero)))) (= x4 x5)) (not ((_ is null) null))) (not (is-cons x6))) ((_ is zero) (succ (ite ((_ is succ) x1) (pred x1) zero)))))
(check-sat)
(exit)


