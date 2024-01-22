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

(assert (and (and (and (and (and (and (and (= zero x2) ((_ is succ) zero)) (not ((_ is zero) (ite (is-leaf (ite (is-cons (ite ((_ is node) (node (ite ((_ is node) (ite (is-cons (ite ((_ is node) (ite (is-cons x4) (car x4) (leaf zero))) (children (ite (is-cons x4) (car x4) (leaf zero))) null)) (car (ite ((_ is node) (ite (is-cons x4) (car x4) (leaf zero))) (children (ite (is-cons x4) (car x4) (leaf zero))) null)) (leaf zero))) (children (ite (is-cons (ite ((_ is node) (ite (is-cons x4) (car x4) (leaf zero))) (children (ite (is-cons x4) (car x4) (leaf zero))) null)) (car (ite ((_ is node) (ite (is-cons x4) (car x4) (leaf zero))) (children (ite (is-cons x4) (car x4) (leaf zero))) null)) (leaf zero))) null))) (children (node (ite ((_ is node) (ite (is-cons (ite ((_ is node) (ite (is-cons x4) (car x4) (leaf zero))) (children (ite (is-cons x4) (car x4) (leaf zero))) null)) (car (ite ((_ is node) (ite (is-cons x4) (car x4) (leaf zero))) (children (ite (is-cons x4) (car x4) (leaf zero))) null)) (leaf zero))) (children (ite (is-cons (ite ((_ is node) (ite (is-cons x4) (car x4) (leaf zero))) (children (ite (is-cons x4) (car x4) (leaf zero))) null)) (car (ite ((_ is node) (ite (is-cons x4) (car x4) (leaf zero))) (children (ite (is-cons x4) (car x4) (leaf zero))) null)) (leaf zero))) null))) null)) (car (ite ((_ is node) (node (ite ((_ is node) (ite (is-cons (ite ((_ is node) (ite (is-cons x4) (car x4) (leaf zero))) (children (ite (is-cons x4) (car x4) (leaf zero))) null)) (car (ite ((_ is node) (ite (is-cons x4) (car x4) (leaf zero))) (children (ite (is-cons x4) (car x4) (leaf zero))) null)) (leaf zero))) (children (ite (is-cons (ite ((_ is node) (ite (is-cons x4) (car x4) (leaf zero))) (children (ite (is-cons x4) (car x4) (leaf zero))) null)) (car (ite ((_ is node) (ite (is-cons x4) (car x4) (leaf zero))) (children (ite (is-cons x4) (car x4) (leaf zero))) null)) (leaf zero))) null))) (children (node (ite ((_ is node) (ite (is-cons (ite ((_ is node) (ite (is-cons x4) (car x4) (leaf zero))) (children (ite (is-cons x4) (car x4) (leaf zero))) null)) (car (ite ((_ is node) (ite (is-cons x4) (car x4) (leaf zero))) (children (ite (is-cons x4) (car x4) (leaf zero))) null)) (leaf zero))) (children (ite (is-cons (ite ((_ is node) (ite (is-cons x4) (car x4) (leaf zero))) (children (ite (is-cons x4) (car x4) (leaf zero))) null)) (car (ite ((_ is node) (ite (is-cons x4) (car x4) (leaf zero))) (children (ite (is-cons x4) (car x4) (leaf zero))) null)) (leaf zero))) null))) null)) (leaf zero))) (data (ite (is-cons (ite ((_ is node) (node (ite ((_ is node) (ite (is-cons (ite ((_ is node) (ite (is-cons x4) (car x4) (leaf zero))) (children (ite (is-cons x4) (car x4) (leaf zero))) null)) (car (ite ((_ is node) (ite (is-cons x4) (car x4) (leaf zero))) (children (ite (is-cons x4) (car x4) (leaf zero))) null)) (leaf zero))) (children (ite (is-cons (ite ((_ is node) (ite (is-cons x4) (car x4) (leaf zero))) (children (ite (is-cons x4) (car x4) (leaf zero))) null)) (car (ite ((_ is node) (ite (is-cons x4) (car x4) (leaf zero))) (children (ite (is-cons x4) (car x4) (leaf zero))) null)) (leaf zero))) null))) (children (node (ite ((_ is node) (ite (is-cons (ite ((_ is node) (ite (is-cons x4) (car x4) (leaf zero))) (children (ite (is-cons x4) (car x4) (leaf zero))) null)) (car (ite ((_ is node) (ite (is-cons x4) (car x4) (leaf zero))) (children (ite (is-cons x4) (car x4) (leaf zero))) null)) (leaf zero))) (children (ite (is-cons (ite ((_ is node) (ite (is-cons x4) (car x4) (leaf zero))) (children (ite (is-cons x4) (car x4) (leaf zero))) null)) (car (ite ((_ is node) (ite (is-cons x4) (car x4) (leaf zero))) (children (ite (is-cons x4) (car x4) (leaf zero))) null)) (leaf zero))) null))) null)) (car (ite ((_ is node) (node (ite ((_ is node) (ite (is-cons (ite ((_ is node) (ite (is-cons x4) (car x4) (leaf zero))) (children (ite (is-cons x4) (car x4) (leaf zero))) null)) (car (ite ((_ is node) (ite (is-cons x4) (car x4) (leaf zero))) (children (ite (is-cons x4) (car x4) (leaf zero))) null)) (leaf zero))) (children (ite (is-cons (ite ((_ is node) (ite (is-cons x4) (car x4) (leaf zero))) (children (ite (is-cons x4) (car x4) (leaf zero))) null)) (car (ite ((_ is node) (ite (is-cons x4) (car x4) (leaf zero))) (children (ite (is-cons x4) (car x4) (leaf zero))) null)) (leaf zero))) null))) (children (node (ite ((_ is node) (ite (is-cons (ite ((_ is node) (ite (is-cons x4) (car x4) (leaf zero))) (children (ite (is-cons x4) (car x4) (leaf zero))) null)) (car (ite ((_ is node) (ite (is-cons x4) (car x4) (leaf zero))) (children (ite (is-cons x4) (car x4) (leaf zero))) null)) (leaf zero))) (children (ite (is-cons (ite ((_ is node) (ite (is-cons x4) (car x4) (leaf zero))) (children (ite (is-cons x4) (car x4) (leaf zero))) null)) (car (ite ((_ is node) (ite (is-cons x4) (car x4) (leaf zero))) (children (ite (is-cons x4) (car x4) (leaf zero))) null)) (leaf zero))) null))) null)) (leaf zero))) zero)))) (not (= x6 (ite (is-cons x4) (car x4) (leaf zero))))) (is-leaf (ite (is-cons null) (car null) (leaf zero)))) (not (is-cons x3))) (= (ite (is-cons (ite (is-cons null) (cdr null) null)) (car (ite (is-cons null) (cdr null) null)) (leaf zero)) x6)) (not (is-cons null))))
(check-sat)
(exit)

