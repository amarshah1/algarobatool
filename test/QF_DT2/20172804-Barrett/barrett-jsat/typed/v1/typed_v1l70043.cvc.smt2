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

(assert (and (and (and (and (and (and (not (= (succ (succ (ite ((_ is succ) (succ (succ (ite ((_ is leaf) x3) (data x3) zero)))) (pred (succ (succ (ite ((_ is leaf) x3) (data x3) zero)))) zero))) zero)) ((_ is cons) null)) (not (= (node (ite ((_ is cons) null) (cdr null) null)) (node null)))) (not (= (ite ((_ is node) x3) (children x3) null) (cons x3 x2)))) (= (ite ((_ is cons) (cons x3 (cons x3 null))) (car (cons x3 (cons x3 null))) (leaf zero)) (leaf x1))) (= null (cons x3 x2))) ((_ is succ) x1)))
(check-sat)
(exit)

