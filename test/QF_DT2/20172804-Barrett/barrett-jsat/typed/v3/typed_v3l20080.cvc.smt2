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

(assert (and (not (= (node x5) (ite (is-cons x6) (car x6) (leaf zero)))) (is-cons (ite ((_ is node) (ite (is-cons (cons x7 (ite ((_ is node) (node null)) (children (node null)) null))) (car (cons x7 (ite ((_ is node) (node null)) (children (node null)) null))) (leaf zero))) (children (ite (is-cons (cons x7 (ite ((_ is node) (node null)) (children (node null)) null))) (car (cons x7 (ite ((_ is node) (node null)) (children (node null)) null))) (leaf zero))) null))))
(check-sat)
(exit)


