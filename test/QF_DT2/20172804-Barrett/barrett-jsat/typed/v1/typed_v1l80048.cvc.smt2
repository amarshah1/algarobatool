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

(assert (and (and (and (and (and (and (and ((_ is zero) (succ x1)) (not ((_ is cons) null))) (= x2 (ite ((_ is cons) (ite ((_ is node) (node null)) (children (node null)) null)) (cdr (ite ((_ is node) (node null)) (children (node null)) null)) null))) (not ((_ is cons) (ite ((_ is node) (ite ((_ is cons) (ite ((_ is cons) (cons x3 (ite ((_ is node) (ite ((_ is cons) null) (car null) (leaf zero))) (children (ite ((_ is cons) null) (car null) (leaf zero))) null))) (cdr (cons x3 (ite ((_ is node) (ite ((_ is cons) null) (car null) (leaf zero))) (children (ite ((_ is cons) null) (car null) (leaf zero))) null))) null)) (car (ite ((_ is cons) (cons x3 (ite ((_ is node) (ite ((_ is cons) null) (car null) (leaf zero))) (children (ite ((_ is cons) null) (car null) (leaf zero))) null))) (cdr (cons x3 (ite ((_ is node) (ite ((_ is cons) null) (car null) (leaf zero))) (children (ite ((_ is cons) null) (car null) (leaf zero))) null))) null)) (leaf zero))) (children (ite ((_ is cons) (ite ((_ is cons) (cons x3 (ite ((_ is node) (ite ((_ is cons) null) (car null) (leaf zero))) (children (ite ((_ is cons) null) (car null) (leaf zero))) null))) (cdr (cons x3 (ite ((_ is node) (ite ((_ is cons) null) (car null) (leaf zero))) (children (ite ((_ is cons) null) (car null) (leaf zero))) null))) null)) (car (ite ((_ is cons) (cons x3 (ite ((_ is node) (ite ((_ is cons) null) (car null) (leaf zero))) (children (ite ((_ is cons) null) (car null) (leaf zero))) null))) (cdr (cons x3 (ite ((_ is node) (ite ((_ is cons) null) (car null) (leaf zero))) (children (ite ((_ is cons) null) (car null) (leaf zero))) null))) null)) (leaf zero))) null)))) (not ((_ is succ) (ite (is-leaf (leaf x1)) (data (leaf x1)) zero)))) (not ((_ is node) x3))) (= (ite (is-leaf x3) (data x3) zero) zero)) (not ((_ is succ) (succ zero)))))
(check-sat)
(exit)


