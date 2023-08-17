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

(assert (and (and (and (and (and (and (and (and (is-leaf (node null)) ((_ is zero) x1)) (not ((_ is succ) zero))) ((_ is null) x2)) (is-leaf (node (cdr x2)))) (not ((_ is node) (node (cons (leaf (data (node (cons (car (cons (car null) (children (car (cdr x2))))) null)))) (cdr x2)))))) (not (is-leaf (node (cdr (cdr x2)))))) (not ((_ is zero) (pred zero)))) ((_ is succ) (data x3))))
(check-sat)
(exit)


