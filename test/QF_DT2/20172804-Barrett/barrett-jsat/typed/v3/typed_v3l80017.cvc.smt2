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

(assert (and (and (and (and (and (and (and (not (= x7 x7)) (not (is-cons x4))) (is-leaf (node (ite (is-cons x4) (cdr x4) null)))) (= (succ (ite ((_ is succ) (ite (is-leaf (leaf x1)) (data (leaf x1)) zero)) (pred (ite (is-leaf (leaf x1)) (data (leaf x1)) zero)) zero)) x1)) (not (is-cons (cons x9 (cons (ite (is-cons (ite (is-cons (cons x7 x5)) (cdr (cons x7 x5)) null)) (car (ite (is-cons (cons x7 x5)) (cdr (cons x7 x5)) null)) (leaf zero)) x5))))) (= (succ zero) (succ (succ zero)))) (not ((_ is succ) (ite (is-leaf x9) (data x9) zero)))) (is-leaf (ite (is-cons null) (car null) (leaf zero)))))
(check-sat)
(exit)

