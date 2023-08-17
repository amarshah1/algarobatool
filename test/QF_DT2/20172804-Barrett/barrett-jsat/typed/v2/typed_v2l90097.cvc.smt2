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

(assert (and (and (and (and (and (and (and (and (not (= (succ x2) (ite ((_ is leaf) (node null)) (data (node null)) zero))) (not ((_ is succ) (succ x1)))) (= (leaf (ite ((_ is succ) x1) (pred x1) zero)) (leaf x1))) (not (= x3 (cons (leaf x2) null)))) (= (leaf (ite ((_ is leaf) (ite (is-cons (cons (leaf (ite ((_ is succ) (succ (ite ((_ is succ) x2) (pred x2) zero))) (pred (succ (ite ((_ is succ) x2) (pred x2) zero))) zero)) x4)) (car (cons (leaf (ite ((_ is succ) (succ (ite ((_ is succ) x2) (pred x2) zero))) (pred (succ (ite ((_ is succ) x2) (pred x2) zero))) zero)) x4)) (leaf zero))) (data (ite (is-cons (cons (leaf (ite ((_ is succ) (succ (ite ((_ is succ) x2) (pred x2) zero))) (pred (succ (ite ((_ is succ) x2) (pred x2) zero))) zero)) x4)) (car (cons (leaf (ite ((_ is succ) (succ (ite ((_ is succ) x2) (pred x2) zero))) (pred (succ (ite ((_ is succ) x2) (pred x2) zero))) zero)) x4)) (leaf zero))) zero)) (ite (is-cons (cons (leaf (succ (ite ((_ is succ) (succ (succ x2))) (pred (succ (succ x2))) zero))) null)) (car (cons (leaf (succ (ite ((_ is succ) (succ (succ x2))) (pred (succ (succ x2))) zero))) null)) (leaf zero)))) (not (= zero (succ x1)))) (= (ite ((_ is leaf) (node (ite (is-cons (cons x6 x3)) (cdr (cons x6 x3)) null))) (data (node (ite (is-cons (cons x6 x3)) (cdr (cons x6 x3)) null))) zero) (ite ((_ is leaf) (leaf (succ (ite ((_ is leaf) x5) (data x5) zero)))) (data (leaf (succ (ite ((_ is leaf) x5) (data x5) zero)))) zero))) (= x5 (leaf x1))) ((_ is null) x4)))
(check-sat)
(exit)


