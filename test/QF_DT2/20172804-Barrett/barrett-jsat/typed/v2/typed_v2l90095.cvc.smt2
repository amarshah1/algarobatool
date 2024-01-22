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

(assert (and (and (and (and (and (and (and (and (not ((_ is null) x4)) (not ((_ is node) (leaf zero)))) (not (= zero (ite ((_ is succ) (ite (is-leaf (ite (is-cons (cons x6 (cons (ite (is-cons (ite (is-cons (ite (is-cons x4) (cdr x4) null)) (cdr (ite (is-cons x4) (cdr x4) null)) null)) (car (ite (is-cons (ite (is-cons x4) (cdr x4) null)) (cdr (ite (is-cons x4) (cdr x4) null)) null)) (leaf zero)) (ite ((_ is node) x5) (children x5) null)))) (car (cons x6 (cons (ite (is-cons (ite (is-cons (ite (is-cons x4) (cdr x4) null)) (cdr (ite (is-cons x4) (cdr x4) null)) null)) (car (ite (is-cons (ite (is-cons x4) (cdr x4) null)) (cdr (ite (is-cons x4) (cdr x4) null)) null)) (leaf zero)) (ite ((_ is node) x5) (children x5) null)))) (leaf zero))) (data (ite (is-cons (cons x6 (cons (ite (is-cons (ite (is-cons (ite (is-cons x4) (cdr x4) null)) (cdr (ite (is-cons x4) (cdr x4) null)) null)) (car (ite (is-cons (ite (is-cons x4) (cdr x4) null)) (cdr (ite (is-cons x4) (cdr x4) null)) null)) (leaf zero)) (ite ((_ is node) x5) (children x5) null)))) (car (cons x6 (cons (ite (is-cons (ite (is-cons (ite (is-cons x4) (cdr x4) null)) (cdr (ite (is-cons x4) (cdr x4) null)) null)) (car (ite (is-cons (ite (is-cons x4) (cdr x4) null)) (cdr (ite (is-cons x4) (cdr x4) null)) null)) (leaf zero)) (ite ((_ is node) x5) (children x5) null)))) (leaf zero))) zero)) (pred (ite (is-leaf (ite (is-cons (cons x6 (cons (ite (is-cons (ite (is-cons (ite (is-cons x4) (cdr x4) null)) (cdr (ite (is-cons x4) (cdr x4) null)) null)) (car (ite (is-cons (ite (is-cons x4) (cdr x4) null)) (cdr (ite (is-cons x4) (cdr x4) null)) null)) (leaf zero)) (ite ((_ is node) x5) (children x5) null)))) (car (cons x6 (cons (ite (is-cons (ite (is-cons (ite (is-cons x4) (cdr x4) null)) (cdr (ite (is-cons x4) (cdr x4) null)) null)) (car (ite (is-cons (ite (is-cons x4) (cdr x4) null)) (cdr (ite (is-cons x4) (cdr x4) null)) null)) (leaf zero)) (ite ((_ is node) x5) (children x5) null)))) (leaf zero))) (data (ite (is-cons (cons x6 (cons (ite (is-cons (ite (is-cons (ite (is-cons x4) (cdr x4) null)) (cdr (ite (is-cons x4) (cdr x4) null)) null)) (car (ite (is-cons (ite (is-cons x4) (cdr x4) null)) (cdr (ite (is-cons x4) (cdr x4) null)) null)) (leaf zero)) (ite ((_ is node) x5) (children x5) null)))) (car (cons x6 (cons (ite (is-cons (ite (is-cons (ite (is-cons x4) (cdr x4) null)) (cdr (ite (is-cons x4) (cdr x4) null)) null)) (car (ite (is-cons (ite (is-cons x4) (cdr x4) null)) (cdr (ite (is-cons x4) (cdr x4) null)) null)) (leaf zero)) (ite ((_ is node) x5) (children x5) null)))) (leaf zero))) zero)) zero)))) ((_ is node) (leaf x1))) (= (ite ((_ is succ) x1) (pred x1) zero) (ite ((_ is succ) (ite (is-leaf x6) (data x6) zero)) (pred (ite (is-leaf x6) (data x6) zero)) zero))) (not (= (ite (is-leaf x5) (data x5) zero) x1))) (is-cons x3)) ((_ is null) (cons (leaf x1) (cons (node (ite ((_ is node) (leaf x2)) (children (leaf x2)) null)) (ite (is-cons x4) (cdr x4) null))))) (= (cons (node (ite (is-cons x4) (cdr x4) null)) null) x3)))
(check-sat)
(exit)

