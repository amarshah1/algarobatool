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

(assert (and (and (and (and (and (and (and ((_ is node) x9) (not (is-cons (ite (is-cons x6) (cdr x6) null)))) (not (= (node (cons x7 null)) (node x4)))) (not (is-leaf x8))) (= (succ (ite (is-leaf (node (ite (is-cons (cons (ite (is-cons (ite ((_ is node) x8) (children x8) null)) (car (ite ((_ is node) x8) (children x8) null)) (leaf zero)) (ite ((_ is node) (ite (is-cons (ite ((_ is node) (leaf x3)) (children (leaf x3)) null)) (car (ite ((_ is node) (leaf x3)) (children (leaf x3)) null)) (leaf zero))) (children (ite (is-cons (ite ((_ is node) (leaf x3)) (children (leaf x3)) null)) (car (ite ((_ is node) (leaf x3)) (children (leaf x3)) null)) (leaf zero))) null))) (cdr (cons (ite (is-cons (ite ((_ is node) x8) (children x8) null)) (car (ite ((_ is node) x8) (children x8) null)) (leaf zero)) (ite ((_ is node) (ite (is-cons (ite ((_ is node) (leaf x3)) (children (leaf x3)) null)) (car (ite ((_ is node) (leaf x3)) (children (leaf x3)) null)) (leaf zero))) (children (ite (is-cons (ite ((_ is node) (leaf x3)) (children (leaf x3)) null)) (car (ite ((_ is node) (leaf x3)) (children (leaf x3)) null)) (leaf zero))) null))) null))) (data (node (ite (is-cons (cons (ite (is-cons (ite ((_ is node) x8) (children x8) null)) (car (ite ((_ is node) x8) (children x8) null)) (leaf zero)) (ite ((_ is node) (ite (is-cons (ite ((_ is node) (leaf x3)) (children (leaf x3)) null)) (car (ite ((_ is node) (leaf x3)) (children (leaf x3)) null)) (leaf zero))) (children (ite (is-cons (ite ((_ is node) (leaf x3)) (children (leaf x3)) null)) (car (ite ((_ is node) (leaf x3)) (children (leaf x3)) null)) (leaf zero))) null))) (cdr (cons (ite (is-cons (ite ((_ is node) x8) (children x8) null)) (car (ite ((_ is node) x8) (children x8) null)) (leaf zero)) (ite ((_ is node) (ite (is-cons (ite ((_ is node) (leaf x3)) (children (leaf x3)) null)) (car (ite ((_ is node) (leaf x3)) (children (leaf x3)) null)) (leaf zero))) (children (ite (is-cons (ite ((_ is node) (leaf x3)) (children (leaf x3)) null)) (car (ite ((_ is node) (leaf x3)) (children (leaf x3)) null)) (leaf zero))) null))) null))) zero)) zero)) (not (= x5 x4))) (not (= (ite (is-cons x6) (car x6) (leaf zero)) (leaf x1)))) (= (cons (leaf x1) (cons (ite (is-cons x6) (car x6) (leaf zero)) null)) x6)))
(check-sat)
(exit)

