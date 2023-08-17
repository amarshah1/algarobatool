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

(assert (and (and (and (and (and (and (and (and (not (= zero (data (car (cdr x2))))) (not (= (node null) (node (cons (leaf zero) (cons (leaf zero) (cons (car (cons x3 x2)) null))))))) (is-leaf x3)) (not (= null null))) (not ((_ is null) (cons (leaf (pred (data x3))) (cdr (cons x3 (children (car (cdr (cons (node x2) x2)))))))))) ((_ is cons) (children x3))) (= (car x2) (car (cdr x2)))) (= (car (cdr null)) (node x2))) (not (= (children (leaf (data (car x2)))) (cons (car (cdr (children (node (children x3))))) (cons (car (children (node (cons (car (cdr (cons (node x2) null))) x2)))) x2))))))
(check-sat)
(exit)


