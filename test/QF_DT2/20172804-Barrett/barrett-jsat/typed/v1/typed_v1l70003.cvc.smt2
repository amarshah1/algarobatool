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

(assert (and (and (and (and (and (and (= (ite ((_ is node) (ite ((_ is cons) (cons (node (cons (ite ((_ is cons) (ite ((_ is node) (ite ((_ is cons) null) (car null) (leaf zero))) (children (ite ((_ is cons) null) (car null) (leaf zero))) null)) (car (ite ((_ is node) (ite ((_ is cons) null) (car null) (leaf zero))) (children (ite ((_ is cons) null) (car null) (leaf zero))) null)) (leaf zero)) x2)) null)) (car (cons (node (cons (ite ((_ is cons) (ite ((_ is node) (ite ((_ is cons) null) (car null) (leaf zero))) (children (ite ((_ is cons) null) (car null) (leaf zero))) null)) (car (ite ((_ is node) (ite ((_ is cons) null) (car null) (leaf zero))) (children (ite ((_ is cons) null) (car null) (leaf zero))) null)) (leaf zero)) x2)) null)) (leaf zero))) (children (ite ((_ is cons) (cons (node (cons (ite ((_ is cons) (ite ((_ is node) (ite ((_ is cons) null) (car null) (leaf zero))) (children (ite ((_ is cons) null) (car null) (leaf zero))) null)) (car (ite ((_ is node) (ite ((_ is cons) null) (car null) (leaf zero))) (children (ite ((_ is cons) null) (car null) (leaf zero))) null)) (leaf zero)) x2)) null)) (car (cons (node (cons (ite ((_ is cons) (ite ((_ is node) (ite ((_ is cons) null) (car null) (leaf zero))) (children (ite ((_ is cons) null) (car null) (leaf zero))) null)) (car (ite ((_ is node) (ite ((_ is cons) null) (car null) (leaf zero))) (children (ite ((_ is cons) null) (car null) (leaf zero))) null)) (leaf zero)) x2)) null)) (leaf zero))) null) (cons (leaf (succ (ite ((_ is succ) x1) (pred x1) zero))) x2)) (not (= (cons (leaf (succ x1)) null) (cons (leaf zero) (ite ((_ is cons) (ite ((_ is node) (leaf (succ (ite (is-leaf (node (ite ((_ is node) (ite ((_ is cons) (cons (leaf zero) (cons (ite ((_ is cons) null) (car null) (leaf zero)) (cons (leaf (ite (is-leaf x3) (data x3) zero)) null)))) (car (cons (leaf zero) (cons (ite ((_ is cons) null) (car null) (leaf zero)) (cons (leaf (ite (is-leaf x3) (data x3) zero)) null)))) (leaf zero))) (children (ite ((_ is cons) (cons (leaf zero) (cons (ite ((_ is cons) null) (car null) (leaf zero)) (cons (leaf (ite (is-leaf x3) (data x3) zero)) null)))) (car (cons (leaf zero) (cons (ite ((_ is cons) null) (car null) (leaf zero)) (cons (leaf (ite (is-leaf x3) (data x3) zero)) null)))) (leaf zero))) null))) (data (node (ite ((_ is node) (ite ((_ is cons) (cons (leaf zero) (cons (ite ((_ is cons) null) (car null) (leaf zero)) (cons (leaf (ite (is-leaf x3) (data x3) zero)) null)))) (car (cons (leaf zero) (cons (ite ((_ is cons) null) (car null) (leaf zero)) (cons (leaf (ite (is-leaf x3) (data x3) zero)) null)))) (leaf zero))) (children (ite ((_ is cons) (cons (leaf zero) (cons (ite ((_ is cons) null) (car null) (leaf zero)) (cons (leaf (ite (is-leaf x3) (data x3) zero)) null)))) (car (cons (leaf zero) (cons (ite ((_ is cons) null) (car null) (leaf zero)) (cons (leaf (ite (is-leaf x3) (data x3) zero)) null)))) (leaf zero))) null))) zero)))) (children (leaf (succ (ite (is-leaf (node (ite ((_ is node) (ite ((_ is cons) (cons (leaf zero) (cons (ite ((_ is cons) null) (car null) (leaf zero)) (cons (leaf (ite (is-leaf x3) (data x3) zero)) null)))) (car (cons (leaf zero) (cons (ite ((_ is cons) null) (car null) (leaf zero)) (cons (leaf (ite (is-leaf x3) (data x3) zero)) null)))) (leaf zero))) (children (ite ((_ is cons) (cons (leaf zero) (cons (ite ((_ is cons) null) (car null) (leaf zero)) (cons (leaf (ite (is-leaf x3) (data x3) zero)) null)))) (car (cons (leaf zero) (cons (ite ((_ is cons) null) (car null) (leaf zero)) (cons (leaf (ite (is-leaf x3) (data x3) zero)) null)))) (leaf zero))) null))) (data (node (ite ((_ is node) (ite ((_ is cons) (cons (leaf zero) (cons (ite ((_ is cons) null) (car null) (leaf zero)) (cons (leaf (ite (is-leaf x3) (data x3) zero)) null)))) (car (cons (leaf zero) (cons (ite ((_ is cons) null) (car null) (leaf zero)) (cons (leaf (ite (is-leaf x3) (data x3) zero)) null)))) (leaf zero))) (children (ite ((_ is cons) (cons (leaf zero) (cons (ite ((_ is cons) null) (car null) (leaf zero)) (cons (leaf (ite (is-leaf x3) (data x3) zero)) null)))) (car (cons (leaf zero) (cons (ite ((_ is cons) null) (car null) (leaf zero)) (cons (leaf (ite (is-leaf x3) (data x3) zero)) null)))) (leaf zero))) null))) zero)))) null)) (cdr (ite ((_ is node) (leaf (succ (ite (is-leaf (node (ite ((_ is node) (ite ((_ is cons) (cons (leaf zero) (cons (ite ((_ is cons) null) (car null) (leaf zero)) (cons (leaf (ite (is-leaf x3) (data x3) zero)) null)))) (car (cons (leaf zero) (cons (ite ((_ is cons) null) (car null) (leaf zero)) (cons (leaf (ite (is-leaf x3) (data x3) zero)) null)))) (leaf zero))) (children (ite ((_ is cons) (cons (leaf zero) (cons (ite ((_ is cons) null) (car null) (leaf zero)) (cons (leaf (ite (is-leaf x3) (data x3) zero)) null)))) (car (cons (leaf zero) (cons (ite ((_ is cons) null) (car null) (leaf zero)) (cons (leaf (ite (is-leaf x3) (data x3) zero)) null)))) (leaf zero))) null))) (data (node (ite ((_ is node) (ite ((_ is cons) (cons (leaf zero) (cons (ite ((_ is cons) null) (car null) (leaf zero)) (cons (leaf (ite (is-leaf x3) (data x3) zero)) null)))) (car (cons (leaf zero) (cons (ite ((_ is cons) null) (car null) (leaf zero)) (cons (leaf (ite (is-leaf x3) (data x3) zero)) null)))) (leaf zero))) (children (ite ((_ is cons) (cons (leaf zero) (cons (ite ((_ is cons) null) (car null) (leaf zero)) (cons (leaf (ite (is-leaf x3) (data x3) zero)) null)))) (car (cons (leaf zero) (cons (ite ((_ is cons) null) (car null) (leaf zero)) (cons (leaf (ite (is-leaf x3) (data x3) zero)) null)))) (leaf zero))) null))) zero)))) (children (leaf (succ (ite (is-leaf (node (ite ((_ is node) (ite ((_ is cons) (cons (leaf zero) (cons (ite ((_ is cons) null) (car null) (leaf zero)) (cons (leaf (ite (is-leaf x3) (data x3) zero)) null)))) (car (cons (leaf zero) (cons (ite ((_ is cons) null) (car null) (leaf zero)) (cons (leaf (ite (is-leaf x3) (data x3) zero)) null)))) (leaf zero))) (children (ite ((_ is cons) (cons (leaf zero) (cons (ite ((_ is cons) null) (car null) (leaf zero)) (cons (leaf (ite (is-leaf x3) (data x3) zero)) null)))) (car (cons (leaf zero) (cons (ite ((_ is cons) null) (car null) (leaf zero)) (cons (leaf (ite (is-leaf x3) (data x3) zero)) null)))) (leaf zero))) null))) (data (node (ite ((_ is node) (ite ((_ is cons) (cons (leaf zero) (cons (ite ((_ is cons) null) (car null) (leaf zero)) (cons (leaf (ite (is-leaf x3) (data x3) zero)) null)))) (car (cons (leaf zero) (cons (ite ((_ is cons) null) (car null) (leaf zero)) (cons (leaf (ite (is-leaf x3) (data x3) zero)) null)))) (leaf zero))) (children (ite ((_ is cons) (cons (leaf zero) (cons (ite ((_ is cons) null) (car null) (leaf zero)) (cons (leaf (ite (is-leaf x3) (data x3) zero)) null)))) (car (cons (leaf zero) (cons (ite ((_ is cons) null) (car null) (leaf zero)) (cons (leaf (ite (is-leaf x3) (data x3) zero)) null)))) (leaf zero))) null))) zero)))) null)) null))))) (= zero (ite ((_ is succ) (succ zero)) (pred (succ zero)) zero))) (= x1 (ite (is-leaf (node (ite ((_ is cons) null) (cdr null) null))) (data (node (ite ((_ is cons) null) (cdr null) null))) zero))) (not (is-leaf x3))) (= (ite ((_ is cons) null) (car null) (leaf zero)) (ite ((_ is cons) null) (car null) (leaf zero)))) (not (= (ite ((_ is cons) x2) (cdr x2) null) x2))))
(check-sat)
(exit)


