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

(assert (and (and (and (and (and (and (and (= (ite (is-cons x3) (cdr x3) null) x4) (is-cons x4)) ((_ is zero) (ite ((_ is succ) (ite ((_ is succ) (ite ((_ is leaf) (ite (is-cons (ite (is-cons (ite ((_ is node) x6) (children x6) null)) (cdr (ite ((_ is node) x6) (children x6) null)) null)) (car (ite (is-cons (ite ((_ is node) x6) (children x6) null)) (cdr (ite ((_ is node) x6) (children x6) null)) null)) (leaf zero))) (data (ite (is-cons (ite (is-cons (ite ((_ is node) x6) (children x6) null)) (cdr (ite ((_ is node) x6) (children x6) null)) null)) (car (ite (is-cons (ite ((_ is node) x6) (children x6) null)) (cdr (ite ((_ is node) x6) (children x6) null)) null)) (leaf zero))) zero)) (pred (ite ((_ is leaf) (ite (is-cons (ite (is-cons (ite ((_ is node) x6) (children x6) null)) (cdr (ite ((_ is node) x6) (children x6) null)) null)) (car (ite (is-cons (ite ((_ is node) x6) (children x6) null)) (cdr (ite ((_ is node) x6) (children x6) null)) null)) (leaf zero))) (data (ite (is-cons (ite (is-cons (ite ((_ is node) x6) (children x6) null)) (cdr (ite ((_ is node) x6) (children x6) null)) null)) (car (ite (is-cons (ite ((_ is node) x6) (children x6) null)) (cdr (ite ((_ is node) x6) (children x6) null)) null)) (leaf zero))) zero)) zero)) (pred (ite ((_ is succ) (ite ((_ is leaf) (ite (is-cons (ite (is-cons (ite ((_ is node) x6) (children x6) null)) (cdr (ite ((_ is node) x6) (children x6) null)) null)) (car (ite (is-cons (ite ((_ is node) x6) (children x6) null)) (cdr (ite ((_ is node) x6) (children x6) null)) null)) (leaf zero))) (data (ite (is-cons (ite (is-cons (ite ((_ is node) x6) (children x6) null)) (cdr (ite ((_ is node) x6) (children x6) null)) null)) (car (ite (is-cons (ite ((_ is node) x6) (children x6) null)) (cdr (ite ((_ is node) x6) (children x6) null)) null)) (leaf zero))) zero)) (pred (ite ((_ is leaf) (ite (is-cons (ite (is-cons (ite ((_ is node) x6) (children x6) null)) (cdr (ite ((_ is node) x6) (children x6) null)) null)) (car (ite (is-cons (ite ((_ is node) x6) (children x6) null)) (cdr (ite ((_ is node) x6) (children x6) null)) null)) (leaf zero))) (data (ite (is-cons (ite (is-cons (ite ((_ is node) x6) (children x6) null)) (cdr (ite ((_ is node) x6) (children x6) null)) null)) (car (ite (is-cons (ite ((_ is node) x6) (children x6) null)) (cdr (ite ((_ is node) x6) (children x6) null)) null)) (leaf zero))) zero)) zero)) zero))) (not (is-cons x3))) (not (= (ite ((_ is succ) zero) (pred zero) zero) zero))) (not ((_ is null) (ite ((_ is node) (leaf (ite ((_ is leaf) (leaf (ite ((_ is leaf) (node (ite (is-cons null) (cdr null) null))) (data (node (ite (is-cons null) (cdr null) null))) zero))) (data (leaf (ite ((_ is leaf) (node (ite (is-cons null) (cdr null) null))) (data (node (ite (is-cons null) (cdr null) null))) zero))) zero))) (children (leaf (ite ((_ is leaf) (leaf (ite ((_ is leaf) (node (ite (is-cons null) (cdr null) null))) (data (node (ite (is-cons null) (cdr null) null))) zero))) (data (leaf (ite ((_ is leaf) (node (ite (is-cons null) (cdr null) null))) (data (node (ite (is-cons null) (cdr null) null))) zero))) zero))) null)))) (not (= x2 (ite ((_ is succ) (ite ((_ is succ) (succ (succ (succ (succ x1))))) (pred (succ (succ (succ (succ x1))))) zero)) (pred (ite ((_ is succ) (succ (succ (succ (succ x1))))) (pred (succ (succ (succ (succ x1))))) zero)) zero)))) (not ((_ is null) (ite (is-cons null) (cdr null) null)))))
(check-sat)
(exit)


