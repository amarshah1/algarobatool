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
(declare-fun x4 () nat)
(declare-fun x5 () nat)
(declare-fun x6 () list)
(declare-fun x7 () list)
(declare-fun x8 () list)
(declare-fun x9 () list)
(declare-fun x10 () list)
(declare-fun x11 () tree)
(declare-fun x12 () tree)
(declare-fun x13 () tree)
(declare-fun x14 () tree)
(declare-fun x15 () tree)

(assert (and (and (and (and (and (and (not (= (succ (succ (succ zero))) (succ zero))) ((_ is null) (cons (node (ite ((_ is node) (leaf x2)) (children (leaf x2)) null)) (ite (is-cons x9) (cdr x9) null)))) (is-leaf (node x10))) (not (= (ite (is-cons x6) (car x6) (leaf zero)) (leaf x3)))) (not (= x7 (ite ((_ is node) (ite (is-cons x6) (car x6) (leaf zero))) (children (ite (is-cons x6) (car x6) (leaf zero))) null)))) (= (ite (is-leaf (ite (is-cons x8) (car x8) (leaf zero))) (data (ite (is-cons x8) (car x8) (leaf zero))) zero) (ite ((_ is succ) x3) (pred x3) zero))) (not (is-cons (ite ((_ is node) (node (ite ((_ is node) x14) (children x14) null))) (children (node (ite ((_ is node) x14) (children x14) null))) null)))))
(check-sat)
(exit)

