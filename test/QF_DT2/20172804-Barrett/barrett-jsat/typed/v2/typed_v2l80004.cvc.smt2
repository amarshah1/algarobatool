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

(assert (and (and (and (and (and (and (and (not ((_ is node) x5)) (not ((_ is node) (node (ite (is-cons null) (cdr null) null))))) (= null (ite (is-cons (ite (is-cons (ite (is-cons (ite (is-cons (ite (is-cons (cons (node x3) (ite ((_ is node) (leaf x1)) (children (leaf x1)) null))) (cdr (cons (node x3) (ite ((_ is node) (leaf x1)) (children (leaf x1)) null))) null)) (cdr (ite (is-cons (cons (node x3) (ite ((_ is node) (leaf x1)) (children (leaf x1)) null))) (cdr (cons (node x3) (ite ((_ is node) (leaf x1)) (children (leaf x1)) null))) null)) null)) (cdr (ite (is-cons (ite (is-cons (cons (node x3) (ite ((_ is node) (leaf x1)) (children (leaf x1)) null))) (cdr (cons (node x3) (ite ((_ is node) (leaf x1)) (children (leaf x1)) null))) null)) (cdr (ite (is-cons (cons (node x3) (ite ((_ is node) (leaf x1)) (children (leaf x1)) null))) (cdr (cons (node x3) (ite ((_ is node) (leaf x1)) (children (leaf x1)) null))) null)) null)) null)) (cdr (ite (is-cons (ite (is-cons (ite (is-cons (cons (node x3) (ite ((_ is node) (leaf x1)) (children (leaf x1)) null))) (cdr (cons (node x3) (ite ((_ is node) (leaf x1)) (children (leaf x1)) null))) null)) (cdr (ite (is-cons (cons (node x3) (ite ((_ is node) (leaf x1)) (children (leaf x1)) null))) (cdr (cons (node x3) (ite ((_ is node) (leaf x1)) (children (leaf x1)) null))) null)) null)) (cdr (ite (is-cons (ite (is-cons (cons (node x3) (ite ((_ is node) (leaf x1)) (children (leaf x1)) null))) (cdr (cons (node x3) (ite ((_ is node) (leaf x1)) (children (leaf x1)) null))) null)) (cdr (ite (is-cons (cons (node x3) (ite ((_ is node) (leaf x1)) (children (leaf x1)) null))) (cdr (cons (node x3) (ite ((_ is node) (leaf x1)) (children (leaf x1)) null))) null)) null)) null)) null)) (cdr (ite (is-cons (ite (is-cons (ite (is-cons (ite (is-cons (cons (node x3) (ite ((_ is node) (leaf x1)) (children (leaf x1)) null))) (cdr (cons (node x3) (ite ((_ is node) (leaf x1)) (children (leaf x1)) null))) null)) (cdr (ite (is-cons (cons (node x3) (ite ((_ is node) (leaf x1)) (children (leaf x1)) null))) (cdr (cons (node x3) (ite ((_ is node) (leaf x1)) (children (leaf x1)) null))) null)) null)) (cdr (ite (is-cons (ite (is-cons (cons (node x3) (ite ((_ is node) (leaf x1)) (children (leaf x1)) null))) (cdr (cons (node x3) (ite ((_ is node) (leaf x1)) (children (leaf x1)) null))) null)) (cdr (ite (is-cons (cons (node x3) (ite ((_ is node) (leaf x1)) (children (leaf x1)) null))) (cdr (cons (node x3) (ite ((_ is node) (leaf x1)) (children (leaf x1)) null))) null)) null)) null)) (cdr (ite (is-cons (ite (is-cons (ite (is-cons (cons (node x3) (ite ((_ is node) (leaf x1)) (children (leaf x1)) null))) (cdr (cons (node x3) (ite ((_ is node) (leaf x1)) (children (leaf x1)) null))) null)) (cdr (ite (is-cons (cons (node x3) (ite ((_ is node) (leaf x1)) (children (leaf x1)) null))) (cdr (cons (node x3) (ite ((_ is node) (leaf x1)) (children (leaf x1)) null))) null)) null)) (cdr (ite (is-cons (ite (is-cons (cons (node x3) (ite ((_ is node) (leaf x1)) (children (leaf x1)) null))) (cdr (cons (node x3) (ite ((_ is node) (leaf x1)) (children (leaf x1)) null))) null)) (cdr (ite (is-cons (cons (node x3) (ite ((_ is node) (leaf x1)) (children (leaf x1)) null))) (cdr (cons (node x3) (ite ((_ is node) (leaf x1)) (children (leaf x1)) null))) null)) null)) null)) null)) null))) (= x5 x6)) (not ((_ is zero) (ite ((_ is leaf) (leaf zero)) (data (leaf zero)) zero)))) (= (ite ((_ is succ) (ite ((_ is leaf) x5) (data x5) zero)) (pred (ite ((_ is leaf) x5) (data x5) zero)) zero) x2)) (is-cons x3)) (= (succ x1) (ite ((_ is succ) (ite ((_ is succ) x1) (pred x1) zero)) (pred (ite ((_ is succ) x1) (pred x1) zero)) zero))))
(check-sat)
(exit)

