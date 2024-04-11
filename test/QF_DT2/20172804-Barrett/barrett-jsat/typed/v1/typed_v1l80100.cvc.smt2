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

(assert (and (and (and (and (and (and (and (not ((_ is leaf) (node (ite ((_ is node) x3) (children x3) null)))) ((_ is cons) x2)) (= (leaf x1) x3)) (= x3 (leaf (ite ((_ is leaf) (leaf (succ (ite ((_ is leaf) (ite ((_ is cons) x2) (car x2) (leaf zero))) (data (ite ((_ is cons) x2) (car x2) (leaf zero))) zero)))) (data (leaf (succ (ite ((_ is leaf) (ite ((_ is cons) x2) (car x2) (leaf zero))) (data (ite ((_ is cons) x2) (car x2) (leaf zero))) zero)))) zero)))) (not ((_ is zero) (succ (ite ((_ is leaf) x3) (data x3) zero))))) (= (cons x3 (cons x3 (cons (node (ite ((_ is cons) (cons (node null) (cons (node x2) (ite ((_ is node) x3) (children x3) null)))) (cdr (cons (node null) (cons (node x2) (ite ((_ is node) x3) (children x3) null)))) null)) (cons (ite ((_ is cons) null) (car null) (leaf zero)) (ite ((_ is node) (leaf (ite ((_ is leaf) x3) (data x3) zero))) (children (leaf (ite ((_ is leaf) x3) (data x3) zero))) null))))) (cons (ite ((_ is cons) (ite ((_ is node) (leaf (ite ((_ is leaf) (ite ((_ is cons) null) (car null) (leaf zero))) (data (ite ((_ is cons) null) (car null) (leaf zero))) zero))) (children (leaf (ite ((_ is leaf) (ite ((_ is cons) null) (car null) (leaf zero))) (data (ite ((_ is cons) null) (car null) (leaf zero))) zero))) null)) (car (ite ((_ is node) (leaf (ite ((_ is leaf) (ite ((_ is cons) null) (car null) (leaf zero))) (data (ite ((_ is cons) null) (car null) (leaf zero))) zero))) (children (leaf (ite ((_ is leaf) (ite ((_ is cons) null) (car null) (leaf zero))) (data (ite ((_ is cons) null) (car null) (leaf zero))) zero))) null)) (leaf zero)) (cons (node (ite ((_ is cons) (ite ((_ is cons) x2) (cdr x2) null)) (cdr (ite ((_ is cons) x2) (cdr x2) null)) null)) (ite ((_ is cons) (cons (leaf (succ (ite ((_ is succ) (ite ((_ is leaf) x3) (data x3) zero)) (pred (ite ((_ is leaf) x3) (data x3) zero)) zero))) x2)) (cdr (cons (leaf (succ (ite ((_ is succ) (ite ((_ is leaf) x3) (data x3) zero)) (pred (ite ((_ is leaf) x3) (data x3) zero)) zero))) x2)) null))))) ((_ is leaf) (leaf zero))) (= (leaf x1) (ite ((_ is cons) x2) (car x2) (leaf zero)))))
(check-sat)
(exit)

