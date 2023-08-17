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

(assert (and (and (and (and ((_ is zero) (ite ((_ is succ) (ite (is-leaf (ite ((_ is cons) (ite ((_ is node) (ite ((_ is cons) x2) (car x2) (leaf zero))) (children (ite ((_ is cons) x2) (car x2) (leaf zero))) null)) (car (ite ((_ is node) (ite ((_ is cons) x2) (car x2) (leaf zero))) (children (ite ((_ is cons) x2) (car x2) (leaf zero))) null)) (leaf zero))) (data (ite ((_ is cons) (ite ((_ is node) (ite ((_ is cons) x2) (car x2) (leaf zero))) (children (ite ((_ is cons) x2) (car x2) (leaf zero))) null)) (car (ite ((_ is node) (ite ((_ is cons) x2) (car x2) (leaf zero))) (children (ite ((_ is cons) x2) (car x2) (leaf zero))) null)) (leaf zero))) zero)) (pred (ite (is-leaf (ite ((_ is cons) (ite ((_ is node) (ite ((_ is cons) x2) (car x2) (leaf zero))) (children (ite ((_ is cons) x2) (car x2) (leaf zero))) null)) (car (ite ((_ is node) (ite ((_ is cons) x2) (car x2) (leaf zero))) (children (ite ((_ is cons) x2) (car x2) (leaf zero))) null)) (leaf zero))) (data (ite ((_ is cons) (ite ((_ is node) (ite ((_ is cons) x2) (car x2) (leaf zero))) (children (ite ((_ is cons) x2) (car x2) (leaf zero))) null)) (car (ite ((_ is node) (ite ((_ is cons) x2) (car x2) (leaf zero))) (children (ite ((_ is cons) x2) (car x2) (leaf zero))) null)) (leaf zero))) zero)) zero)) (= zero (ite (is-leaf (ite ((_ is cons) (ite ((_ is node) x3) (children x3) null)) (car (ite ((_ is node) x3) (children x3) null)) (leaf zero))) (data (ite ((_ is cons) (ite ((_ is node) x3) (children x3) null)) (car (ite ((_ is node) x3) (children x3) null)) (leaf zero))) zero))) (= (node (ite ((_ is node) (ite ((_ is cons) (cons (node (cons (leaf zero) (cons x3 null))) x2)) (car (cons (node (cons (leaf zero) (cons x3 null))) x2)) (leaf zero))) (children (ite ((_ is cons) (cons (node (cons (leaf zero) (cons x3 null))) x2)) (car (cons (node (cons (leaf zero) (cons x3 null))) x2)) (leaf zero))) null)) (leaf (succ zero)))) (not ((_ is node) x3))) (not (= x1 (ite (is-leaf (ite ((_ is cons) (ite ((_ is cons) null) (cdr null) null)) (car (ite ((_ is cons) null) (cdr null) null)) (leaf zero))) (data (ite ((_ is cons) (ite ((_ is cons) null) (cdr null) null)) (car (ite ((_ is cons) null) (cdr null) null)) (leaf zero))) zero)))))
(check-sat)
(exit)


