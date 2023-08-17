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

(assert (and (and (and (and (and (and ((_ is node) (leaf zero)) (= (ite (is-cons x3) (car x3) (leaf zero)) (leaf (succ (succ (ite ((_ is succ) x1) (pred x1) zero)))))) (not (= (cons (leaf (ite ((_ is succ) (succ zero)) (pred (succ zero)) zero)) (cons (node x3) (ite ((_ is node) x6) (children x6) null))) (ite (is-cons (ite (is-cons (cons (ite (is-cons (ite (is-cons (ite (is-cons x4) (cdr x4) null)) (cdr (ite (is-cons x4) (cdr x4) null)) null)) (car (ite (is-cons (ite (is-cons x4) (cdr x4) null)) (cdr (ite (is-cons x4) (cdr x4) null)) null)) (leaf zero)) (cons x5 x3))) (cdr (cons (ite (is-cons (ite (is-cons (ite (is-cons x4) (cdr x4) null)) (cdr (ite (is-cons x4) (cdr x4) null)) null)) (car (ite (is-cons (ite (is-cons x4) (cdr x4) null)) (cdr (ite (is-cons x4) (cdr x4) null)) null)) (leaf zero)) (cons x5 x3))) null)) (cdr (ite (is-cons (cons (ite (is-cons (ite (is-cons (ite (is-cons x4) (cdr x4) null)) (cdr (ite (is-cons x4) (cdr x4) null)) null)) (car (ite (is-cons (ite (is-cons x4) (cdr x4) null)) (cdr (ite (is-cons x4) (cdr x4) null)) null)) (leaf zero)) (cons x5 x3))) (cdr (cons (ite (is-cons (ite (is-cons (ite (is-cons x4) (cdr x4) null)) (cdr (ite (is-cons x4) (cdr x4) null)) null)) (car (ite (is-cons (ite (is-cons x4) (cdr x4) null)) (cdr (ite (is-cons x4) (cdr x4) null)) null)) (leaf zero)) (cons x5 x3))) null)) null)))) ((_ is null) x4)) (not (= (leaf (succ (ite ((_ is succ) (succ (ite (is-leaf (ite (is-cons null) (car null) (leaf zero))) (data (ite (is-cons null) (car null) (leaf zero))) zero))) (pred (succ (ite (is-leaf (ite (is-cons null) (car null) (leaf zero))) (data (ite (is-cons null) (car null) (leaf zero))) zero))) zero))) x5))) (= (ite ((_ is node) (leaf x1)) (children (leaf x1)) null) x4)) (= null x4)))
(check-sat)
(exit)


