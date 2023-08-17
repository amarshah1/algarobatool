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

(assert (and (and (and (and (and (and (and (= (car (cons (node (children (node (cdr x2)))) (cdr (cons (node x2) (cdr (cons x3 (cons (node (cdr (children (node null)))) (cdr (children (leaf (pred (data (leaf x1))))))))))))) (leaf (pred (data (car x2))))) (= (leaf (succ zero)) (car (cdr (children (node (children (node (children x3))))))))) (not ((_ is null) x2))) (not ((_ is null) (children (node null))))) (not (= (car (cdr (children x3))) (car (cons x3 (children (leaf (succ (pred (pred (data (car null)))))))))))) (not (= zero x1))) (= (data (node x2)) (pred (data (node x2))))) (= (pred (succ (pred zero))) x1)))
(check-sat)
(exit)


