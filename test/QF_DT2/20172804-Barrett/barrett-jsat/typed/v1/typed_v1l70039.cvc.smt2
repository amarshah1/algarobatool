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

(assert (and (and (and (and (and (and (not ((_ is null) null)) (not (= null (cons (leaf (ite ((_ is succ) zero) (pred zero) zero)) (cons (ite ((_ is cons) (cons (leaf zero) (ite ((_ is node) (node null)) (children (node null)) null))) (car (cons (leaf zero) (ite ((_ is node) (node null)) (children (node null)) null))) (leaf zero)) (ite ((_ is node) (leaf (ite (is-leaf (leaf x1)) (data (leaf x1)) zero))) (children (leaf (ite (is-leaf (leaf x1)) (data (leaf x1)) zero))) null)))))) (not (= zero zero))) ((_ is node) (node x2))) (not (= (ite ((_ is cons) (cons (node (ite ((_ is node) (ite ((_ is cons) null) (car null) (leaf zero))) (children (ite ((_ is cons) null) (car null) (leaf zero))) null)) (ite ((_ is cons) null) (cdr null) null))) (car (cons (node (ite ((_ is node) (ite ((_ is cons) null) (car null) (leaf zero))) (children (ite ((_ is cons) null) (car null) (leaf zero))) null)) (ite ((_ is cons) null) (cdr null) null))) (leaf zero)) (leaf x1)))) ((_ is null) null)) (not (= (ite ((_ is cons) x2) (car x2) (leaf zero)) (leaf zero)))))
(check-sat)
(exit)


