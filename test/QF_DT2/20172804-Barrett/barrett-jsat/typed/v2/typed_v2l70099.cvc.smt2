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
(declare-fun x3 () list)
(declare-fun x4 () list)
(declare-fun x5 () tree)
(declare-fun x6 () tree)

(assert (and (and (and (and (and (and (is-cons (ite ((_ is node) (node (cons x6 x3))) (children (node (cons x6 x3))) null)) ((_ is succ) (ite (is-leaf (ite (is-cons x3) (car x3) (leaf zero))) (data (ite (is-cons x3) (car x3) (leaf zero))) zero))) ((_ is zero) x2)) ((_ is succ) zero)) (not (= x4 x3))) (= (ite (is-cons (ite (is-cons x3) (cdr x3) null)) (car (ite (is-cons x3) (cdr x3) null)) (leaf zero)) x6)) (not (= (leaf x2) (leaf zero)))))
(check-sat)
(exit)


