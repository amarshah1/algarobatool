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

(assert (and (and (and (and (and (and (and (= (cdr x2) (cdr (cons (car x2) (cdr (cons (node (cons (leaf (pred (pred (data (leaf (data (node (cons (leaf zero) null)))))))) null)) (cons (node (cdr (cdr (cdr (cdr null))))) x2)))))) (not ((_ is zero) zero))) (not ((_ is succ) x1))) (= x3 x3)) (not ((_ is cons) (children (car null))))) (= null null)) (not (= null (cdr (cons (node x2) (cons (leaf zero) (children (car (cdr x2))))))))) (not ((_ is succ) (pred (succ (succ x1)))))))
(check-sat)
(exit)


