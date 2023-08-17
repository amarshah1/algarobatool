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

(assert (and (and (and (and (and (and (and (= (node (ite (is-cons null) (cdr null) null)) (leaf x3)) (= x7 (ite (is-cons (cons x7 x6)) (car (cons x7 x6)) (leaf zero)))) (not (= x9 (leaf x3)))) (not (= x7 (ite (is-cons x5) (car x5) (leaf zero))))) (not ((_ is succ) zero))) (not ((_ is zero) (succ x2)))) (= (ite (is-leaf (node (ite (is-cons (ite ((_ is node) (ite (is-cons (ite ((_ is node) x7) (children x7) null)) (car (ite ((_ is node) x7) (children x7) null)) (leaf zero))) (children (ite (is-cons (ite ((_ is node) x7) (children x7) null)) (car (ite ((_ is node) x7) (children x7) null)) (leaf zero))) null)) (cdr (ite ((_ is node) (ite (is-cons (ite ((_ is node) x7) (children x7) null)) (car (ite ((_ is node) x7) (children x7) null)) (leaf zero))) (children (ite (is-cons (ite ((_ is node) x7) (children x7) null)) (car (ite ((_ is node) x7) (children x7) null)) (leaf zero))) null)) null))) (data (node (ite (is-cons (ite ((_ is node) (ite (is-cons (ite ((_ is node) x7) (children x7) null)) (car (ite ((_ is node) x7) (children x7) null)) (leaf zero))) (children (ite (is-cons (ite ((_ is node) x7) (children x7) null)) (car (ite ((_ is node) x7) (children x7) null)) (leaf zero))) null)) (cdr (ite ((_ is node) (ite (is-cons (ite ((_ is node) x7) (children x7) null)) (car (ite ((_ is node) x7) (children x7) null)) (leaf zero))) (children (ite (is-cons (ite ((_ is node) x7) (children x7) null)) (car (ite ((_ is node) x7) (children x7) null)) (leaf zero))) null)) null))) zero) (ite (is-leaf x7) (data x7) zero))) (not (= x2 (succ x1)))))
(check-sat)
(exit)


