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

(assert (and (and (and (and (and (and ((_ is node) (ite ((_ is cons) x2) (car x2) (leaf zero))) (not (= (ite ((_ is cons) (ite ((_ is node) (node (cons x3 null))) (children (node (cons x3 null))) null)) (cdr (ite ((_ is node) (node (cons x3 null))) (children (node (cons x3 null))) null)) null) (cons (ite ((_ is cons) (cons (leaf (ite ((_ is succ) (ite (is-leaf x3) (data x3) zero)) (pred (ite (is-leaf x3) (data x3) zero)) zero)) null)) (car (cons (leaf (ite ((_ is succ) (ite (is-leaf x3) (data x3) zero)) (pred (ite (is-leaf x3) (data x3) zero)) zero)) null)) (leaf zero)) x2)))) (not (= x2 x2))) (not (= (node (cons (ite ((_ is cons) x2) (car x2) (leaf zero)) x2)) (node (ite ((_ is node) (node (ite ((_ is node) (ite ((_ is cons) null) (car null) (leaf zero))) (children (ite ((_ is cons) null) (car null) (leaf zero))) null))) (children (node (ite ((_ is node) (ite ((_ is cons) null) (car null) (leaf zero))) (children (ite ((_ is cons) null) (car null) (leaf zero))) null))) null))))) ((_ is zero) (succ zero))) (not ((_ is succ) zero))) (not (= x1 (ite ((_ is succ) zero) (pred zero) zero)))))
(check-sat)
(exit)

