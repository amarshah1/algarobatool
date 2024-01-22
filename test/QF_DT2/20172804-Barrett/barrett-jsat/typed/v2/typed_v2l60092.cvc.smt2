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

(assert (and (and (and (and (and (= (ite ((_ is leaf) (leaf x1)) (data (leaf x1)) zero) x1) ((_ is node) (node null))) (not (= (ite (is-cons x4) (car x4) (leaf zero)) (node x4)))) (not ((_ is succ) zero))) (= zero (ite ((_ is succ) (ite ((_ is succ) (ite ((_ is leaf) (ite (is-cons (ite ((_ is node) (node (ite (is-cons (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) (cdr (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) null))) (children (node (ite (is-cons (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) (cdr (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) null))) null)) (car (ite ((_ is node) (node (ite (is-cons (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) (cdr (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) null))) (children (node (ite (is-cons (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) (cdr (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) null))) null)) (leaf zero))) (data (ite (is-cons (ite ((_ is node) (node (ite (is-cons (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) (cdr (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) null))) (children (node (ite (is-cons (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) (cdr (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) null))) null)) (car (ite ((_ is node) (node (ite (is-cons (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) (cdr (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) null))) (children (node (ite (is-cons (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) (cdr (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) null))) null)) (leaf zero))) zero)) (pred (ite ((_ is leaf) (ite (is-cons (ite ((_ is node) (node (ite (is-cons (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) (cdr (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) null))) (children (node (ite (is-cons (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) (cdr (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) null))) null)) (car (ite ((_ is node) (node (ite (is-cons (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) (cdr (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) null))) (children (node (ite (is-cons (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) (cdr (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) null))) null)) (leaf zero))) (data (ite (is-cons (ite ((_ is node) (node (ite (is-cons (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) (cdr (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) null))) (children (node (ite (is-cons (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) (cdr (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) null))) null)) (car (ite ((_ is node) (node (ite (is-cons (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) (cdr (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) null))) (children (node (ite (is-cons (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) (cdr (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) null))) null)) (leaf zero))) zero)) zero)) (pred (ite ((_ is succ) (ite ((_ is leaf) (ite (is-cons (ite ((_ is node) (node (ite (is-cons (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) (cdr (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) null))) (children (node (ite (is-cons (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) (cdr (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) null))) null)) (car (ite ((_ is node) (node (ite (is-cons (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) (cdr (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) null))) (children (node (ite (is-cons (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) (cdr (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) null))) null)) (leaf zero))) (data (ite (is-cons (ite ((_ is node) (node (ite (is-cons (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) (cdr (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) null))) (children (node (ite (is-cons (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) (cdr (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) null))) null)) (car (ite ((_ is node) (node (ite (is-cons (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) (cdr (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) null))) (children (node (ite (is-cons (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) (cdr (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) null))) null)) (leaf zero))) zero)) (pred (ite ((_ is leaf) (ite (is-cons (ite ((_ is node) (node (ite (is-cons (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) (cdr (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) null))) (children (node (ite (is-cons (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) (cdr (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) null))) null)) (car (ite ((_ is node) (node (ite (is-cons (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) (cdr (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) null))) (children (node (ite (is-cons (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) (cdr (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) null))) null)) (leaf zero))) (data (ite (is-cons (ite ((_ is node) (node (ite (is-cons (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) (cdr (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) null))) (children (node (ite (is-cons (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) (cdr (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) null))) null)) (car (ite ((_ is node) (node (ite (is-cons (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) (cdr (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) null))) (children (node (ite (is-cons (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) (cdr (ite ((_ is node) (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) (children (ite (is-cons (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (car (cons (leaf (ite ((_ is succ) x1) (pred x1) zero)) x3)) (leaf zero))) null)) null))) null)) (leaf zero))) zero)) zero)) zero))) ((_ is zero) (succ x2))))
(check-sat)
(exit)

