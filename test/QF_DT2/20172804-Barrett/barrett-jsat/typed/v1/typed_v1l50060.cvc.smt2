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

(assert (and (and (and (and (not (= x2 (ite ((_ is node) (leaf (succ (ite (is-leaf (leaf (succ (succ (ite (is-leaf (ite (is-cons (cons (leaf x1) null)) (car (cons (leaf x1) null)) (leaf zero))) (data (ite (is-cons (cons (leaf x1) null)) (car (cons (leaf x1) null)) (leaf zero))) zero))))) (data (leaf (succ (succ (ite (is-leaf (ite (is-cons (cons (leaf x1) null)) (car (cons (leaf x1) null)) (leaf zero))) (data (ite (is-cons (cons (leaf x1) null)) (car (cons (leaf x1) null)) (leaf zero))) zero))))) zero)))) (children (leaf (succ (ite (is-leaf (leaf (succ (succ (ite (is-leaf (ite (is-cons (cons (leaf x1) null)) (car (cons (leaf x1) null)) (leaf zero))) (data (ite (is-cons (cons (leaf x1) null)) (car (cons (leaf x1) null)) (leaf zero))) zero))))) (data (leaf (succ (succ (ite (is-leaf (ite (is-cons (cons (leaf x1) null)) (car (cons (leaf x1) null)) (leaf zero))) (data (ite (is-cons (cons (leaf x1) null)) (car (cons (leaf x1) null)) (leaf zero))) zero))))) zero)))) null))) (not ((_ is zero) (ite (is-leaf (ite (is-cons (ite (is-cons (ite (is-cons null) (cdr null) null)) (cdr (ite (is-cons null) (cdr null) null)) null)) (car (ite (is-cons (ite (is-cons null) (cdr null) null)) (cdr (ite (is-cons null) (cdr null) null)) null)) (leaf zero))) (data (ite (is-cons (ite (is-cons (ite (is-cons null) (cdr null) null)) (cdr (ite (is-cons null) (cdr null) null)) null)) (car (ite (is-cons (ite (is-cons null) (cdr null) null)) (cdr (ite (is-cons null) (cdr null) null)) null)) (leaf zero))) zero)))) ((_ is node) (node null))) (not ((_ is node) (leaf (ite ((_ is succ) (ite (is-leaf (leaf (ite (is-leaf (leaf (ite (is-leaf x3) (data x3) zero))) (data (leaf (ite (is-leaf x3) (data x3) zero))) zero))) (data (leaf (ite (is-leaf (leaf (ite (is-leaf x3) (data x3) zero))) (data (leaf (ite (is-leaf x3) (data x3) zero))) zero))) zero)) (pred (ite (is-leaf (leaf (ite (is-leaf (leaf (ite (is-leaf x3) (data x3) zero))) (data (leaf (ite (is-leaf x3) (data x3) zero))) zero))) (data (leaf (ite (is-leaf (leaf (ite (is-leaf x3) (data x3) zero))) (data (leaf (ite (is-leaf x3) (data x3) zero))) zero))) zero)) zero))))) (not ((_ is null) x2))))
(check-sat)
(exit)


