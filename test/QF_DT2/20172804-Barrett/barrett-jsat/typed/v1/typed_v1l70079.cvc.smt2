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
(declare-fun x2 () list)
(declare-fun x3 () tree)

(assert (and (and (and (and (and (and (not ((_ is succ) zero)) (not (is-leaf x3))) (= (ite (is-cons (ite (is-cons (cons (node (ite (is-cons (cons x3 (ite (is-cons x2) (cdr x2) null))) (cdr (cons x3 (ite (is-cons x2) (cdr x2) null))) null)) null)) (cdr (cons (node (ite (is-cons (cons x3 (ite (is-cons x2) (cdr x2) null))) (cdr (cons x3 (ite (is-cons x2) (cdr x2) null))) null)) null)) null)) (cdr (ite (is-cons (cons (node (ite (is-cons (cons x3 (ite (is-cons x2) (cdr x2) null))) (cdr (cons x3 (ite (is-cons x2) (cdr x2) null))) null)) null)) (cdr (cons (node (ite (is-cons (cons x3 (ite (is-cons x2) (cdr x2) null))) (cdr (cons x3 (ite (is-cons x2) (cdr x2) null))) null)) null)) null)) null) (ite (is-cons null) (cdr null) null))) (not ((_ is succ) (succ (succ (ite (is-leaf (node (ite (is-cons x2) (cdr x2) null))) (data (node (ite (is-cons x2) (cdr x2) null))) zero)))))) (not (= (ite ((_ is node) (leaf (ite (is-leaf (node (cons (leaf (ite ((_ is succ) zero) (pred zero) zero)) x2))) (data (node (cons (leaf (ite ((_ is succ) zero) (pred zero) zero)) x2))) zero))) (children (leaf (ite (is-leaf (node (cons (leaf (ite ((_ is succ) zero) (pred zero) zero)) x2))) (data (node (cons (leaf (ite ((_ is succ) zero) (pred zero) zero)) x2))) zero))) null) (ite (is-cons null) (cdr null) null)))) (not (= (ite (is-leaf (leaf zero)) (data (leaf zero)) zero) (ite (is-leaf (ite (is-cons (ite (is-cons x2) (cdr x2) null)) (car (ite (is-cons x2) (cdr x2) null)) (leaf zero))) (data (ite (is-cons (ite (is-cons x2) (cdr x2) null)) (car (ite (is-cons x2) (cdr x2) null)) (leaf zero))) zero)))) (not ((_ is zero) (ite (is-leaf (node x2)) (data (node x2)) zero)))))
(check-sat)
(exit)

