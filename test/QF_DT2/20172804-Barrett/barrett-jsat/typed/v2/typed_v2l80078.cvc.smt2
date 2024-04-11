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

(assert (and (and (and (and (and (and (and (not ((_ is null) (cons (node (cons (node (ite (is-cons x3) (cdr x3) null)) (ite ((_ is node) (node (ite (is-cons (cons (ite (is-cons x3) (car x3) (leaf zero)) (ite ((_ is node) (ite (is-cons (ite ((_ is node) (ite (is-cons null) (car null) (leaf zero))) (children (ite (is-cons null) (car null) (leaf zero))) null)) (car (ite ((_ is node) (ite (is-cons null) (car null) (leaf zero))) (children (ite (is-cons null) (car null) (leaf zero))) null)) (leaf zero))) (children (ite (is-cons (ite ((_ is node) (ite (is-cons null) (car null) (leaf zero))) (children (ite (is-cons null) (car null) (leaf zero))) null)) (car (ite ((_ is node) (ite (is-cons null) (car null) (leaf zero))) (children (ite (is-cons null) (car null) (leaf zero))) null)) (leaf zero))) null))) (cdr (cons (ite (is-cons x3) (car x3) (leaf zero)) (ite ((_ is node) (ite (is-cons (ite ((_ is node) (ite (is-cons null) (car null) (leaf zero))) (children (ite (is-cons null) (car null) (leaf zero))) null)) (car (ite ((_ is node) (ite (is-cons null) (car null) (leaf zero))) (children (ite (is-cons null) (car null) (leaf zero))) null)) (leaf zero))) (children (ite (is-cons (ite ((_ is node) (ite (is-cons null) (car null) (leaf zero))) (children (ite (is-cons null) (car null) (leaf zero))) null)) (car (ite ((_ is node) (ite (is-cons null) (car null) (leaf zero))) (children (ite (is-cons null) (car null) (leaf zero))) null)) (leaf zero))) null))) null))) (children (node (ite (is-cons (cons (ite (is-cons x3) (car x3) (leaf zero)) (ite ((_ is node) (ite (is-cons (ite ((_ is node) (ite (is-cons null) (car null) (leaf zero))) (children (ite (is-cons null) (car null) (leaf zero))) null)) (car (ite ((_ is node) (ite (is-cons null) (car null) (leaf zero))) (children (ite (is-cons null) (car null) (leaf zero))) null)) (leaf zero))) (children (ite (is-cons (ite ((_ is node) (ite (is-cons null) (car null) (leaf zero))) (children (ite (is-cons null) (car null) (leaf zero))) null)) (car (ite ((_ is node) (ite (is-cons null) (car null) (leaf zero))) (children (ite (is-cons null) (car null) (leaf zero))) null)) (leaf zero))) null))) (cdr (cons (ite (is-cons x3) (car x3) (leaf zero)) (ite ((_ is node) (ite (is-cons (ite ((_ is node) (ite (is-cons null) (car null) (leaf zero))) (children (ite (is-cons null) (car null) (leaf zero))) null)) (car (ite ((_ is node) (ite (is-cons null) (car null) (leaf zero))) (children (ite (is-cons null) (car null) (leaf zero))) null)) (leaf zero))) (children (ite (is-cons (ite ((_ is node) (ite (is-cons null) (car null) (leaf zero))) (children (ite (is-cons null) (car null) (leaf zero))) null)) (car (ite ((_ is node) (ite (is-cons null) (car null) (leaf zero))) (children (ite (is-cons null) (car null) (leaf zero))) null)) (leaf zero))) null))) null))) null))) (ite ((_ is node) x6) (children x6) null)))) (= (node (ite ((_ is node) (leaf (ite ((_ is leaf) x6) (data x6) zero))) (children (leaf (ite ((_ is leaf) x6) (data x6) zero))) null)) (leaf (succ (succ x1))))) (= (node x3) (ite (is-cons (ite ((_ is node) x5) (children x5) null)) (car (ite ((_ is node) x5) (children x5) null)) (leaf zero)))) (not ((_ is null) x3))) (= x6 x6)) (not ((_ is succ) zero))) (= x1 zero)) (not (= (ite (is-cons null) (car null) (leaf zero)) (ite (is-cons x4) (car x4) (leaf zero))))))
(check-sat)
(exit)

