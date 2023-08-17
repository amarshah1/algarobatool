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

(assert (and (and (and (not ((_ is succ) x1)) (not (= (ite (is-cons x5) (car x5) (leaf zero)) (ite (is-cons (ite ((_ is node) x7) (children x7) null)) (car (ite ((_ is node) x7) (children x7) null)) (leaf zero))))) (not ((_ is null) (ite (is-cons (ite (is-cons x4) (cdr x4) null)) (cdr (ite (is-cons x4) (cdr x4) null)) null)))) (not (= (leaf x2) x7))))
(check-sat)
(exit)


