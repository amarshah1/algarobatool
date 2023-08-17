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

(assert (and (and (and (and (and (and (and (not (= (ite (is-leaf x7) (data x7) zero) (ite ((_ is succ) (succ (ite ((_ is succ) x1) (pred x1) zero))) (pred (succ (ite ((_ is succ) x1) (pred x1) zero))) zero))) (not (= (succ (succ x3)) x3))) (= (succ x2) x3)) (not ((_ is null) (cons x9 x4)))) ((_ is succ) (succ x3))) (= (cons (leaf x3) null) (ite (is-cons x4) (cdr x4) null))) (not (= x7 (ite (is-cons (ite (is-cons (cons x8 (ite ((_ is node) (ite (is-cons x4) (car x4) (leaf zero))) (children (ite (is-cons x4) (car x4) (leaf zero))) null))) (cdr (cons x8 (ite ((_ is node) (ite (is-cons x4) (car x4) (leaf zero))) (children (ite (is-cons x4) (car x4) (leaf zero))) null))) null)) (car (ite (is-cons (cons x8 (ite ((_ is node) (ite (is-cons x4) (car x4) (leaf zero))) (children (ite (is-cons x4) (car x4) (leaf zero))) null))) (cdr (cons x8 (ite ((_ is node) (ite (is-cons x4) (car x4) (leaf zero))) (children (ite (is-cons x4) (car x4) (leaf zero))) null))) null)) (leaf zero))))) (not (is-cons x5))))
(check-sat)
(exit)


