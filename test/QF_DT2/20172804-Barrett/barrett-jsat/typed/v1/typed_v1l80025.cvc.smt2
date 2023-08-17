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

(assert (and (and (and (and (and (and (and (not (= (ite (is-leaf (leaf (succ (succ (ite ((_ is succ) (ite ((_ is succ) (ite ((_ is succ) zero) (pred zero) zero)) (pred (ite ((_ is succ) zero) (pred zero) zero)) zero)) (pred (ite ((_ is succ) (ite ((_ is succ) zero) (pred zero) zero)) (pred (ite ((_ is succ) zero) (pred zero) zero)) zero)) zero))))) (data (leaf (succ (succ (ite ((_ is succ) (ite ((_ is succ) (ite ((_ is succ) zero) (pred zero) zero)) (pred (ite ((_ is succ) zero) (pred zero) zero)) zero)) (pred (ite ((_ is succ) (ite ((_ is succ) zero) (pred zero) zero)) (pred (ite ((_ is succ) zero) (pred zero) zero)) zero)) zero))))) zero) (ite ((_ is succ) (succ (succ (succ (ite ((_ is succ) zero) (pred zero) zero))))) (pred (succ (succ (succ (ite ((_ is succ) zero) (pred zero) zero))))) zero))) (not (= (ite (is-cons x2) (car x2) (leaf zero)) (leaf (ite ((_ is succ) (ite (is-leaf x3) (data x3) zero)) (pred (ite (is-leaf x3) (data x3) zero)) zero))))) (= x1 (ite ((_ is succ) (ite ((_ is succ) x1) (pred x1) zero)) (pred (ite ((_ is succ) x1) (pred x1) zero)) zero))) (not ((_ is null) null))) (= x2 (cons (leaf x1) (ite ((_ is node) (ite (is-cons x2) (car x2) (leaf zero))) (children (ite (is-cons x2) (car x2) (leaf zero))) null)))) (not ((_ is succ) zero))) (= zero (ite (is-leaf (leaf (ite (is-leaf x3) (data x3) zero))) (data (leaf (ite (is-leaf x3) (data x3) zero))) zero))) (not (= zero (succ (ite ((_ is succ) x1) (pred x1) zero))))))
(check-sat)
(exit)


