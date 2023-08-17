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

(assert (and (and (and (and (and (not (= (node (cons x3 null)) (leaf x1))) ((_ is zero) zero)) (not (= (succ (ite ((_ is succ) (ite ((_ is succ) (succ x1)) (pred (succ x1)) zero)) (pred (ite ((_ is succ) (succ x1)) (pred (succ x1)) zero)) zero)) zero))) ((_ is null) (ite ((_ is node) (ite (is-cons (ite ((_ is node) (node (ite (is-cons (ite (is-cons x2) (cdr x2) null)) (cdr (ite (is-cons x2) (cdr x2) null)) null))) (children (node (ite (is-cons (ite (is-cons x2) (cdr x2) null)) (cdr (ite (is-cons x2) (cdr x2) null)) null))) null)) (car (ite ((_ is node) (node (ite (is-cons (ite (is-cons x2) (cdr x2) null)) (cdr (ite (is-cons x2) (cdr x2) null)) null))) (children (node (ite (is-cons (ite (is-cons x2) (cdr x2) null)) (cdr (ite (is-cons x2) (cdr x2) null)) null))) null)) (leaf zero))) (children (ite (is-cons (ite ((_ is node) (node (ite (is-cons (ite (is-cons x2) (cdr x2) null)) (cdr (ite (is-cons x2) (cdr x2) null)) null))) (children (node (ite (is-cons (ite (is-cons x2) (cdr x2) null)) (cdr (ite (is-cons x2) (cdr x2) null)) null))) null)) (car (ite ((_ is node) (node (ite (is-cons (ite (is-cons x2) (cdr x2) null)) (cdr (ite (is-cons x2) (cdr x2) null)) null))) (children (node (ite (is-cons (ite (is-cons x2) (cdr x2) null)) (cdr (ite (is-cons x2) (cdr x2) null)) null))) null)) (leaf zero))) null))) (not (= (succ x1) x1))) (= (ite ((_ is succ) (ite ((_ is succ) (ite (is-leaf (ite (is-cons (ite ((_ is node) (leaf zero)) (children (leaf zero)) null)) (car (ite ((_ is node) (leaf zero)) (children (leaf zero)) null)) (leaf zero))) (data (ite (is-cons (ite ((_ is node) (leaf zero)) (children (leaf zero)) null)) (car (ite ((_ is node) (leaf zero)) (children (leaf zero)) null)) (leaf zero))) zero)) (pred (ite (is-leaf (ite (is-cons (ite ((_ is node) (leaf zero)) (children (leaf zero)) null)) (car (ite ((_ is node) (leaf zero)) (children (leaf zero)) null)) (leaf zero))) (data (ite (is-cons (ite ((_ is node) (leaf zero)) (children (leaf zero)) null)) (car (ite ((_ is node) (leaf zero)) (children (leaf zero)) null)) (leaf zero))) zero)) zero)) (pred (ite ((_ is succ) (ite (is-leaf (ite (is-cons (ite ((_ is node) (leaf zero)) (children (leaf zero)) null)) (car (ite ((_ is node) (leaf zero)) (children (leaf zero)) null)) (leaf zero))) (data (ite (is-cons (ite ((_ is node) (leaf zero)) (children (leaf zero)) null)) (car (ite ((_ is node) (leaf zero)) (children (leaf zero)) null)) (leaf zero))) zero)) (pred (ite (is-leaf (ite (is-cons (ite ((_ is node) (leaf zero)) (children (leaf zero)) null)) (car (ite ((_ is node) (leaf zero)) (children (leaf zero)) null)) (leaf zero))) (data (ite (is-cons (ite ((_ is node) (leaf zero)) (children (leaf zero)) null)) (car (ite ((_ is node) (leaf zero)) (children (leaf zero)) null)) (leaf zero))) zero)) zero)) zero) (ite ((_ is succ) (ite ((_ is succ) (ite (is-leaf x3) (data x3) zero)) (pred (ite (is-leaf x3) (data x3) zero)) zero)) (pred (ite ((_ is succ) (ite (is-leaf x3) (data x3) zero)) (pred (ite (is-leaf x3) (data x3) zero)) zero)) zero))))
(check-sat)
(exit)


