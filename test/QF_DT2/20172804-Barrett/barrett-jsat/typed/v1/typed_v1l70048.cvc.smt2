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

(assert (and (and (and (and (and (and (not (= (succ (ite ((_ is leaf) (leaf (ite ((_ is leaf) (ite ((_ is cons) (cons (node x2) (ite ((_ is node) x3) (children x3) null))) (car (cons (node x2) (ite ((_ is node) x3) (children x3) null))) (leaf zero))) (data (ite ((_ is cons) (cons (node x2) (ite ((_ is node) x3) (children x3) null))) (car (cons (node x2) (ite ((_ is node) x3) (children x3) null))) (leaf zero))) zero))) (data (leaf (ite ((_ is leaf) (ite ((_ is cons) (cons (node x2) (ite ((_ is node) x3) (children x3) null))) (car (cons (node x2) (ite ((_ is node) x3) (children x3) null))) (leaf zero))) (data (ite ((_ is cons) (cons (node x2) (ite ((_ is node) x3) (children x3) null))) (car (cons (node x2) (ite ((_ is node) x3) (children x3) null))) (leaf zero))) zero))) zero)) zero)) (not (= x2 x2))) ((_ is zero) zero)) (not (= (ite ((_ is cons) (ite ((_ is node) x3) (children x3) null)) (car (ite ((_ is node) x3) (children x3) null)) (leaf zero)) (node (cons (ite ((_ is cons) (ite ((_ is cons) (cons x3 (ite ((_ is node) (node (cons (ite ((_ is cons) (ite ((_ is cons) x2) (cdr x2) null)) (car (ite ((_ is cons) x2) (cdr x2) null)) (leaf zero)) (ite ((_ is cons) (cons x3 (ite ((_ is cons) x2) (cdr x2) null))) (cdr (cons x3 (ite ((_ is cons) x2) (cdr x2) null))) null)))) (children (node (cons (ite ((_ is cons) (ite ((_ is cons) x2) (cdr x2) null)) (car (ite ((_ is cons) x2) (cdr x2) null)) (leaf zero)) (ite ((_ is cons) (cons x3 (ite ((_ is cons) x2) (cdr x2) null))) (cdr (cons x3 (ite ((_ is cons) x2) (cdr x2) null))) null)))) null))) (cdr (cons x3 (ite ((_ is node) (node (cons (ite ((_ is cons) (ite ((_ is cons) x2) (cdr x2) null)) (car (ite ((_ is cons) x2) (cdr x2) null)) (leaf zero)) (ite ((_ is cons) (cons x3 (ite ((_ is cons) x2) (cdr x2) null))) (cdr (cons x3 (ite ((_ is cons) x2) (cdr x2) null))) null)))) (children (node (cons (ite ((_ is cons) (ite ((_ is cons) x2) (cdr x2) null)) (car (ite ((_ is cons) x2) (cdr x2) null)) (leaf zero)) (ite ((_ is cons) (cons x3 (ite ((_ is cons) x2) (cdr x2) null))) (cdr (cons x3 (ite ((_ is cons) x2) (cdr x2) null))) null)))) null))) null)) (car (ite ((_ is cons) (cons x3 (ite ((_ is node) (node (cons (ite ((_ is cons) (ite ((_ is cons) x2) (cdr x2) null)) (car (ite ((_ is cons) x2) (cdr x2) null)) (leaf zero)) (ite ((_ is cons) (cons x3 (ite ((_ is cons) x2) (cdr x2) null))) (cdr (cons x3 (ite ((_ is cons) x2) (cdr x2) null))) null)))) (children (node (cons (ite ((_ is cons) (ite ((_ is cons) x2) (cdr x2) null)) (car (ite ((_ is cons) x2) (cdr x2) null)) (leaf zero)) (ite ((_ is cons) (cons x3 (ite ((_ is cons) x2) (cdr x2) null))) (cdr (cons x3 (ite ((_ is cons) x2) (cdr x2) null))) null)))) null))) (cdr (cons x3 (ite ((_ is node) (node (cons (ite ((_ is cons) (ite ((_ is cons) x2) (cdr x2) null)) (car (ite ((_ is cons) x2) (cdr x2) null)) (leaf zero)) (ite ((_ is cons) (cons x3 (ite ((_ is cons) x2) (cdr x2) null))) (cdr (cons x3 (ite ((_ is cons) x2) (cdr x2) null))) null)))) (children (node (cons (ite ((_ is cons) (ite ((_ is cons) x2) (cdr x2) null)) (car (ite ((_ is cons) x2) (cdr x2) null)) (leaf zero)) (ite ((_ is cons) (cons x3 (ite ((_ is cons) x2) (cdr x2) null))) (cdr (cons x3 (ite ((_ is cons) x2) (cdr x2) null))) null)))) null))) null)) (leaf zero)) (ite ((_ is node) x3) (children x3) null)))))) (not ((_ is leaf) (ite ((_ is cons) (ite ((_ is node) x3) (children x3) null)) (car (ite ((_ is node) x3) (children x3) null)) (leaf zero))))) (not ((_ is null) null))) (not ((_ is cons) (ite ((_ is cons) (ite ((_ is cons) (ite ((_ is cons) (ite ((_ is cons) (cons x3 x2)) (cdr (cons x3 x2)) null)) (cdr (ite ((_ is cons) (cons x3 x2)) (cdr (cons x3 x2)) null)) null)) (cdr (ite ((_ is cons) (ite ((_ is cons) (cons x3 x2)) (cdr (cons x3 x2)) null)) (cdr (ite ((_ is cons) (cons x3 x2)) (cdr (cons x3 x2)) null)) null)) null)) (cdr (ite ((_ is cons) (ite ((_ is cons) (ite ((_ is cons) (cons x3 x2)) (cdr (cons x3 x2)) null)) (cdr (ite ((_ is cons) (cons x3 x2)) (cdr (cons x3 x2)) null)) null)) (cdr (ite ((_ is cons) (ite ((_ is cons) (cons x3 x2)) (cdr (cons x3 x2)) null)) (cdr (ite ((_ is cons) (cons x3 x2)) (cdr (cons x3 x2)) null)) null)) null)) null)))))
(check-sat)
(exit)


