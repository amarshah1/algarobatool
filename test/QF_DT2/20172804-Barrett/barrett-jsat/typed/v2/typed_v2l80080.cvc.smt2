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
(declare-fun x2 () nat)
(declare-fun x3 () list)
(declare-fun x4 () list)
(declare-fun x5 () tree)
(declare-fun x6 () tree)

(assert (and (and (and (and (and (and (and ((_ is null) (ite (is-cons x4) (cdr x4) null)) ((_ is succ) x1)) ((_ is leaf) x6)) (not ((_ is succ) (ite ((_ is leaf) (ite (is-cons x4) (car x4) (leaf zero))) (data (ite (is-cons x4) (car x4) (leaf zero))) zero)))) (not (= (cons x6 (cons (node (ite (is-cons (ite (is-cons x4) (cdr x4) null)) (cdr (ite (is-cons x4) (cdr x4) null)) null)) x3)) (cons (ite (is-cons (ite ((_ is node) x6) (children x6) null)) (car (ite ((_ is node) x6) (children x6) null)) (leaf zero)) (ite (is-cons x3) (cdr x3) null))))) (= (ite (is-cons (ite (is-cons (ite ((_ is node) (node (ite (is-cons (ite ((_ is node) x6) (children x6) null)) (cdr (ite ((_ is node) x6) (children x6) null)) null))) (children (node (ite (is-cons (ite ((_ is node) x6) (children x6) null)) (cdr (ite ((_ is node) x6) (children x6) null)) null))) null)) (cdr (ite ((_ is node) (node (ite (is-cons (ite ((_ is node) x6) (children x6) null)) (cdr (ite ((_ is node) x6) (children x6) null)) null))) (children (node (ite (is-cons (ite ((_ is node) x6) (children x6) null)) (cdr (ite ((_ is node) x6) (children x6) null)) null))) null)) null)) (car (ite (is-cons (ite ((_ is node) (node (ite (is-cons (ite ((_ is node) x6) (children x6) null)) (cdr (ite ((_ is node) x6) (children x6) null)) null))) (children (node (ite (is-cons (ite ((_ is node) x6) (children x6) null)) (cdr (ite ((_ is node) x6) (children x6) null)) null))) null)) (cdr (ite ((_ is node) (node (ite (is-cons (ite ((_ is node) x6) (children x6) null)) (cdr (ite ((_ is node) x6) (children x6) null)) null))) (children (node (ite (is-cons (ite ((_ is node) x6) (children x6) null)) (cdr (ite ((_ is node) x6) (children x6) null)) null))) null)) null)) (leaf zero)) x5)) (not ((_ is null) x3))) (= (ite (is-cons x4) (cdr x4) null) (ite ((_ is node) (ite (is-cons (ite ((_ is node) (node (cons (leaf x2) (ite ((_ is node) x5) (children x5) null)))) (children (node (cons (leaf x2) (ite ((_ is node) x5) (children x5) null)))) null)) (car (ite ((_ is node) (node (cons (leaf x2) (ite ((_ is node) x5) (children x5) null)))) (children (node (cons (leaf x2) (ite ((_ is node) x5) (children x5) null)))) null)) (leaf zero))) (children (ite (is-cons (ite ((_ is node) (node (cons (leaf x2) (ite ((_ is node) x5) (children x5) null)))) (children (node (cons (leaf x2) (ite ((_ is node) x5) (children x5) null)))) null)) (car (ite ((_ is node) (node (cons (leaf x2) (ite ((_ is node) x5) (children x5) null)))) (children (node (cons (leaf x2) (ite ((_ is node) x5) (children x5) null)))) null)) (leaf zero))) null))))
(check-sat)
(exit)

