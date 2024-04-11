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
(declare-fun x3 () list)
(declare-fun x4 () list)
(declare-fun x5 () tree)
(declare-fun x6 () tree)

(assert (and (and (and (and (and (and (and (not ((_ is node) x6)) (not (is-cons (ite (is-cons (ite ((_ is node) x5) (children x5) null)) (cdr (ite ((_ is node) x5) (children x5) null)) null)))) (not (= (node null) (leaf x1)))) (= (ite ((_ is leaf) (ite (is-cons (ite ((_ is node) x5) (children x5) null)) (car (ite ((_ is node) x5) (children x5) null)) (leaf zero))) (data (ite (is-cons (ite ((_ is node) x5) (children x5) null)) (car (ite ((_ is node) x5) (children x5) null)) (leaf zero))) zero) x2)) ((_ is zero) (ite ((_ is leaf) (node x3)) (data (node x3)) zero))) (= x2 x2)) (not (= (leaf (ite ((_ is leaf) (node x4)) (data (node x4)) zero)) (ite (is-cons x3) (car x3) (leaf zero))))) ((_ is leaf) x5)))
(check-sat)
(exit)

