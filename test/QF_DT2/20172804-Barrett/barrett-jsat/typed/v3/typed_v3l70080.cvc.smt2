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
(declare-fun x2 () nat)
(declare-fun x3 () nat)
(declare-fun x4 () list)
(declare-fun x5 () list)
(declare-fun x6 () list)
(declare-fun x7 () tree)
(declare-fun x8 () tree)
(declare-fun x9 () tree)

(assert (and (and (and (and (and (and (not (= (ite (is-leaf x7) (data x7) zero) (succ (ite ((_ is succ) zero) (pred zero) zero)))) (not (= (ite (is-cons null) (cdr null) null) (cons (leaf x3) (ite ((_ is node) (node null)) (children (node null)) null))))) ((_ is null) (ite (is-cons x6) (cdr x6) null))) (= (ite ((_ is succ) (ite ((_ is succ) (ite ((_ is succ) (ite ((_ is succ) (ite ((_ is succ) x3) (pred x3) zero)) (pred (ite ((_ is succ) x3) (pred x3) zero)) zero)) (pred (ite ((_ is succ) (ite ((_ is succ) x3) (pred x3) zero)) (pred (ite ((_ is succ) x3) (pred x3) zero)) zero)) zero)) (pred (ite ((_ is succ) (ite ((_ is succ) (ite ((_ is succ) x3) (pred x3) zero)) (pred (ite ((_ is succ) x3) (pred x3) zero)) zero)) (pred (ite ((_ is succ) (ite ((_ is succ) x3) (pred x3) zero)) (pred (ite ((_ is succ) x3) (pred x3) zero)) zero)) zero)) zero)) (pred (ite ((_ is succ) (ite ((_ is succ) (ite ((_ is succ) (ite ((_ is succ) x3) (pred x3) zero)) (pred (ite ((_ is succ) x3) (pred x3) zero)) zero)) (pred (ite ((_ is succ) (ite ((_ is succ) x3) (pred x3) zero)) (pred (ite ((_ is succ) x3) (pred x3) zero)) zero)) zero)) (pred (ite ((_ is succ) (ite ((_ is succ) (ite ((_ is succ) x3) (pred x3) zero)) (pred (ite ((_ is succ) x3) (pred x3) zero)) zero)) (pred (ite ((_ is succ) (ite ((_ is succ) x3) (pred x3) zero)) (pred (ite ((_ is succ) x3) (pred x3) zero)) zero)) zero)) zero)) zero) (ite ((_ is succ) (ite (is-leaf (node x5)) (data (node x5)) zero)) (pred (ite (is-leaf (node x5)) (data (node x5)) zero)) zero))) (not (= x6 (ite ((_ is node) (node (ite ((_ is node) x8) (children x8) null))) (children (node (ite ((_ is node) x8) (children x8) null))) null)))) (= x5 (ite (is-cons null) (cdr null) null))) (not (= (ite ((_ is node) (leaf (ite ((_ is succ) zero) (pred zero) zero))) (children (leaf (ite ((_ is succ) zero) (pred zero) zero))) null) (ite (is-cons null) (cdr null) null)))))
(check-sat)
(exit)

