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
(declare-fun x3 () list)
(declare-fun x4 () list)
(declare-fun x5 () tree)
(declare-fun x6 () tree)

(assert (and (and (and (and (and (and (and ((_ is node) (leaf x1)) (not (= (ite ((_ is succ) (succ (succ x2))) (pred (succ (succ x2))) zero) (succ (ite ((_ is succ) (ite ((_ is leaf) (ite (is-cons (ite (is-cons null) (cdr null) null)) (car (ite (is-cons null) (cdr null) null)) (leaf zero))) (data (ite (is-cons (ite (is-cons null) (cdr null) null)) (car (ite (is-cons null) (cdr null) null)) (leaf zero))) zero)) (pred (ite ((_ is leaf) (ite (is-cons (ite (is-cons null) (cdr null) null)) (car (ite (is-cons null) (cdr null) null)) (leaf zero))) (data (ite (is-cons (ite (is-cons null) (cdr null) null)) (car (ite (is-cons null) (cdr null) null)) (leaf zero))) zero)) zero))))) (not ((_ is node) x5))) (not (= (ite ((_ is succ) (ite ((_ is leaf) (leaf x2)) (data (leaf x2)) zero)) (pred (ite ((_ is leaf) (leaf x2)) (data (leaf x2)) zero)) zero) (ite ((_ is leaf) (node null)) (data (node null)) zero)))) (not (= (leaf x2) (node null)))) ((_ is node) x5)) (not ((_ is null) null))) (= x4 x4)))
(check-sat)
(exit)


