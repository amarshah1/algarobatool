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

(assert (and (and (and (not (= (leaf x1) (node (ite ((_ is node) (node null)) (children (node null)) null)))) (not ((_ is succ) (ite (is-leaf (leaf (ite (is-leaf (leaf (ite (is-leaf (leaf (ite (is-leaf (ite (is-cons null) (car null) (leaf zero))) (data (ite (is-cons null) (car null) (leaf zero))) zero))) (data (leaf (ite (is-leaf (ite (is-cons null) (car null) (leaf zero))) (data (ite (is-cons null) (car null) (leaf zero))) zero))) zero))) (data (leaf (ite (is-leaf (leaf (ite (is-leaf (ite (is-cons null) (car null) (leaf zero))) (data (ite (is-cons null) (car null) (leaf zero))) zero))) (data (leaf (ite (is-leaf (ite (is-cons null) (car null) (leaf zero))) (data (ite (is-cons null) (car null) (leaf zero))) zero))) zero))) zero))) (data (leaf (ite (is-leaf (leaf (ite (is-leaf (leaf (ite (is-leaf (ite (is-cons null) (car null) (leaf zero))) (data (ite (is-cons null) (car null) (leaf zero))) zero))) (data (leaf (ite (is-leaf (ite (is-cons null) (car null) (leaf zero))) (data (ite (is-cons null) (car null) (leaf zero))) zero))) zero))) (data (leaf (ite (is-leaf (leaf (ite (is-leaf (ite (is-cons null) (car null) (leaf zero))) (data (ite (is-cons null) (car null) (leaf zero))) zero))) (data (leaf (ite (is-leaf (ite (is-cons null) (car null) (leaf zero))) (data (ite (is-cons null) (car null) (leaf zero))) zero))) zero))) zero))) zero)))) (= (leaf zero) (leaf zero))) (= (cons x3 null) (ite ((_ is node) (ite (is-cons (ite (is-cons x2) (cdr x2) null)) (car (ite (is-cons x2) (cdr x2) null)) (leaf zero))) (children (ite (is-cons (ite (is-cons x2) (cdr x2) null)) (car (ite (is-cons x2) (cdr x2) null)) (leaf zero))) null))))
(check-sat)
(exit)


