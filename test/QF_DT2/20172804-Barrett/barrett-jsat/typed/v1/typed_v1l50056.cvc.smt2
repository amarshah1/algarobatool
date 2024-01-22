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
(declare-fun x2 () list)
(declare-fun x3 () tree)

(assert (and (and (and (and (not (= (succ x1) (ite ((_ is succ) (ite ((_ is leaf) (ite ((_ is cons) x2) (car x2) (leaf zero))) (data (ite ((_ is cons) x2) (car x2) (leaf zero))) zero)) (pred (ite ((_ is leaf) (ite ((_ is cons) x2) (car x2) (leaf zero))) (data (ite ((_ is cons) x2) (car x2) (leaf zero))) zero)) zero))) (not ((_ is node) (ite ((_ is cons) x2) (car x2) (leaf zero))))) ((_ is node) (node (cons (leaf (succ x1)) (cons (leaf (succ (ite ((_ is leaf) (node x2)) (data (node x2)) zero))) x2))))) ((_ is succ) (ite ((_ is leaf) x3) (data x3) zero))) (= (ite ((_ is succ) (ite ((_ is succ) (ite ((_ is leaf) x3) (data x3) zero)) (pred (ite ((_ is leaf) x3) (data x3) zero)) zero)) (pred (ite ((_ is succ) (ite ((_ is leaf) x3) (data x3) zero)) (pred (ite ((_ is leaf) x3) (data x3) zero)) zero)) zero) (succ (succ (succ x1))))))
(check-sat)
(exit)

