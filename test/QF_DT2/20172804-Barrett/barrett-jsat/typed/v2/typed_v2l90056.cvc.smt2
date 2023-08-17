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

(assert (and (and (and (and (and (and (and (and (is-cons (ite (is-cons null) (cdr null) null)) (not (= x1 (ite ((_ is succ) (ite ((_ is leaf) (leaf (ite ((_ is leaf) x6) (data x6) zero))) (data (leaf (ite ((_ is leaf) x6) (data x6) zero))) zero)) (pred (ite ((_ is leaf) (leaf (ite ((_ is leaf) x6) (data x6) zero))) (data (leaf (ite ((_ is leaf) x6) (data x6) zero))) zero)) zero)))) ((_ is null) null)) (= (leaf (ite ((_ is leaf) x5) (data x5) zero)) (leaf (succ x2)))) (= zero (succ zero))) ((_ is null) (ite ((_ is node) (ite (is-cons (cons (leaf x2) x4)) (car (cons (leaf x2) x4)) (leaf zero))) (children (ite (is-cons (cons (leaf x2) x4)) (car (cons (leaf x2) x4)) (leaf zero))) null))) (not (= (ite ((_ is node) (leaf zero)) (children (leaf zero)) null) x3))) (not ((_ is node) x5))) (not ((_ is succ) x1))))
(check-sat)
(exit)


