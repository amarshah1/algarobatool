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

(assert (and (and (and (and (and (and (and (not ((_ is node) (node (ite ((_ is node) (leaf (ite ((_ is leaf) x3) (data x3) zero))) (children (leaf (ite ((_ is leaf) x3) (data x3) zero))) null)))) (not (= x3 (leaf (ite ((_ is leaf) x3) (data x3) zero))))) (not ((_ is null) x2))) (not ((_ is node) (ite ((_ is cons) null) (car null) (leaf zero))))) (not ((_ is succ) zero))) (not (= (ite ((_ is leaf) (node null)) (data (node null)) zero) (ite ((_ is leaf) x3) (data x3) zero)))) ((_ is cons) (ite ((_ is cons) (ite ((_ is node) (leaf (succ (ite ((_ is succ) (succ (succ (ite ((_ is leaf) x3) (data x3) zero)))) (pred (succ (succ (ite ((_ is leaf) x3) (data x3) zero)))) zero)))) (children (leaf (succ (ite ((_ is succ) (succ (succ (ite ((_ is leaf) x3) (data x3) zero)))) (pred (succ (succ (ite ((_ is leaf) x3) (data x3) zero)))) zero)))) null)) (cdr (ite ((_ is node) (leaf (succ (ite ((_ is succ) (succ (succ (ite ((_ is leaf) x3) (data x3) zero)))) (pred (succ (succ (ite ((_ is leaf) x3) (data x3) zero)))) zero)))) (children (leaf (succ (ite ((_ is succ) (succ (succ (ite ((_ is leaf) x3) (data x3) zero)))) (pred (succ (succ (ite ((_ is leaf) x3) (data x3) zero)))) zero)))) null)) null))) ((_ is node) (ite ((_ is cons) (cons (leaf (succ x1)) (ite ((_ is node) (leaf (ite ((_ is leaf) (leaf (succ (succ (ite ((_ is leaf) x3) (data x3) zero))))) (data (leaf (succ (succ (ite ((_ is leaf) x3) (data x3) zero))))) zero))) (children (leaf (ite ((_ is leaf) (leaf (succ (succ (ite ((_ is leaf) x3) (data x3) zero))))) (data (leaf (succ (succ (ite ((_ is leaf) x3) (data x3) zero))))) zero))) null))) (car (cons (leaf (succ x1)) (ite ((_ is node) (leaf (ite ((_ is leaf) (leaf (succ (succ (ite ((_ is leaf) x3) (data x3) zero))))) (data (leaf (succ (succ (ite ((_ is leaf) x3) (data x3) zero))))) zero))) (children (leaf (ite ((_ is leaf) (leaf (succ (succ (ite ((_ is leaf) x3) (data x3) zero))))) (data (leaf (succ (succ (ite ((_ is leaf) x3) (data x3) zero))))) zero))) null))) (leaf zero)))))
(check-sat)
(exit)

