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

(assert (and (and (and (and (and (and (and (and (not (is-cons (ite ((_ is node) (leaf (ite ((_ is succ) (ite ((_ is leaf) x6) (data x6) zero)) (pred (ite ((_ is leaf) x6) (data x6) zero)) zero))) (children (leaf (ite ((_ is succ) (ite ((_ is leaf) x6) (data x6) zero)) (pred (ite ((_ is leaf) x6) (data x6) zero)) zero))) null))) (not (= zero zero))) (not ((_ is succ) (succ (ite ((_ is leaf) x6) (data x6) zero))))) (= (succ (succ (ite ((_ is leaf) (node x4)) (data (node x4)) zero))) x1)) (= (node (ite (is-cons x4) (cdr x4) null)) x6)) (not (is-cons (ite (is-cons (ite (is-cons x3) (cdr x3) null)) (cdr (ite (is-cons x3) (cdr x3) null)) null)))) (not (= (ite (is-cons (cons (node (cons (node (ite ((_ is node) (ite (is-cons (cons x5 null)) (car (cons x5 null)) (leaf zero))) (children (ite (is-cons (cons x5 null)) (car (cons x5 null)) (leaf zero))) null)) x3)) x4)) (car (cons (node (cons (node (ite ((_ is node) (ite (is-cons (cons x5 null)) (car (cons x5 null)) (leaf zero))) (children (ite (is-cons (cons x5 null)) (car (cons x5 null)) (leaf zero))) null)) x3)) x4)) (leaf zero)) (ite (is-cons x3) (car x3) (leaf zero))))) (not (= x6 (node null)))) (not (= (ite (is-cons x4) (car x4) (leaf zero)) (node x4)))))
(check-sat)
(exit)


