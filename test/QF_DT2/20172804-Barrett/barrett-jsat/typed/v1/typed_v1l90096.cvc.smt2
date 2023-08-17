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

(assert (and (and (and (and (and (and (and (and (not (= (ite ((_ is node) (leaf (ite ((_ is succ) (ite (is-leaf (ite ((_ is cons) (cons (leaf (ite (is-leaf (ite ((_ is cons) null) (car null) (leaf zero))) (data (ite ((_ is cons) null) (car null) (leaf zero))) zero)) null)) (car (cons (leaf (ite (is-leaf (ite ((_ is cons) null) (car null) (leaf zero))) (data (ite ((_ is cons) null) (car null) (leaf zero))) zero)) null)) (leaf zero))) (data (ite ((_ is cons) (cons (leaf (ite (is-leaf (ite ((_ is cons) null) (car null) (leaf zero))) (data (ite ((_ is cons) null) (car null) (leaf zero))) zero)) null)) (car (cons (leaf (ite (is-leaf (ite ((_ is cons) null) (car null) (leaf zero))) (data (ite ((_ is cons) null) (car null) (leaf zero))) zero)) null)) (leaf zero))) zero)) (pred (ite (is-leaf (ite ((_ is cons) (cons (leaf (ite (is-leaf (ite ((_ is cons) null) (car null) (leaf zero))) (data (ite ((_ is cons) null) (car null) (leaf zero))) zero)) null)) (car (cons (leaf (ite (is-leaf (ite ((_ is cons) null) (car null) (leaf zero))) (data (ite ((_ is cons) null) (car null) (leaf zero))) zero)) null)) (leaf zero))) (data (ite ((_ is cons) (cons (leaf (ite (is-leaf (ite ((_ is cons) null) (car null) (leaf zero))) (data (ite ((_ is cons) null) (car null) (leaf zero))) zero)) null)) (car (cons (leaf (ite (is-leaf (ite ((_ is cons) null) (car null) (leaf zero))) (data (ite ((_ is cons) null) (car null) (leaf zero))) zero)) null)) (leaf zero))) zero)) zero))) (children (leaf (ite ((_ is succ) (ite (is-leaf (ite ((_ is cons) (cons (leaf (ite (is-leaf (ite ((_ is cons) null) (car null) (leaf zero))) (data (ite ((_ is cons) null) (car null) (leaf zero))) zero)) null)) (car (cons (leaf (ite (is-leaf (ite ((_ is cons) null) (car null) (leaf zero))) (data (ite ((_ is cons) null) (car null) (leaf zero))) zero)) null)) (leaf zero))) (data (ite ((_ is cons) (cons (leaf (ite (is-leaf (ite ((_ is cons) null) (car null) (leaf zero))) (data (ite ((_ is cons) null) (car null) (leaf zero))) zero)) null)) (car (cons (leaf (ite (is-leaf (ite ((_ is cons) null) (car null) (leaf zero))) (data (ite ((_ is cons) null) (car null) (leaf zero))) zero)) null)) (leaf zero))) zero)) (pred (ite (is-leaf (ite ((_ is cons) (cons (leaf (ite (is-leaf (ite ((_ is cons) null) (car null) (leaf zero))) (data (ite ((_ is cons) null) (car null) (leaf zero))) zero)) null)) (car (cons (leaf (ite (is-leaf (ite ((_ is cons) null) (car null) (leaf zero))) (data (ite ((_ is cons) null) (car null) (leaf zero))) zero)) null)) (leaf zero))) (data (ite ((_ is cons) (cons (leaf (ite (is-leaf (ite ((_ is cons) null) (car null) (leaf zero))) (data (ite ((_ is cons) null) (car null) (leaf zero))) zero)) null)) (car (cons (leaf (ite (is-leaf (ite ((_ is cons) null) (car null) (leaf zero))) (data (ite ((_ is cons) null) (car null) (leaf zero))) zero)) null)) (leaf zero))) zero)) zero))) null) (cons x3 (cons (node (ite ((_ is cons) (ite ((_ is node) (leaf (ite ((_ is succ) (ite (is-leaf (node null)) (data (node null)) zero)) (pred (ite (is-leaf (node null)) (data (node null)) zero)) zero))) (children (leaf (ite ((_ is succ) (ite (is-leaf (node null)) (data (node null)) zero)) (pred (ite (is-leaf (node null)) (data (node null)) zero)) zero))) null)) (cdr (ite ((_ is node) (leaf (ite ((_ is succ) (ite (is-leaf (node null)) (data (node null)) zero)) (pred (ite (is-leaf (node null)) (data (node null)) zero)) zero))) (children (leaf (ite ((_ is succ) (ite (is-leaf (node null)) (data (node null)) zero)) (pred (ite (is-leaf (node null)) (data (node null)) zero)) zero))) null)) null)) (cons (leaf (succ (ite (is-leaf (ite ((_ is cons) x2) (car x2) (leaf zero))) (data (ite ((_ is cons) x2) (car x2) (leaf zero))) zero))) (ite ((_ is node) (ite ((_ is cons) null) (car null) (leaf zero))) (children (ite ((_ is cons) null) (car null) (leaf zero))) null)))))) (= (ite ((_ is cons) x2) (cdr x2) null) x2)) (is-leaf (node x2))) (not (= (ite ((_ is cons) (cons (leaf zero) (ite ((_ is node) (ite ((_ is cons) x2) (car x2) (leaf zero))) (children (ite ((_ is cons) x2) (car x2) (leaf zero))) null))) (cdr (cons (leaf zero) (ite ((_ is node) (ite ((_ is cons) x2) (car x2) (leaf zero))) (children (ite ((_ is cons) x2) (car x2) (leaf zero))) null))) null) (ite ((_ is cons) (cons (ite ((_ is cons) null) (car null) (leaf zero)) (ite ((_ is cons) (cons (node x2) x2)) (cdr (cons (node x2) x2)) null))) (cdr (cons (ite ((_ is cons) null) (car null) (leaf zero)) (ite ((_ is cons) (cons (node x2) x2)) (cdr (cons (node x2) x2)) null))) null)))) (= x3 (ite ((_ is cons) (cons (leaf (ite (is-leaf x3) (data x3) zero)) (ite ((_ is cons) null) (cdr null) null))) (car (cons (leaf (ite (is-leaf x3) (data x3) zero)) (ite ((_ is cons) null) (cdr null) null))) (leaf zero)))) ((_ is succ) (succ zero))) (not ((_ is null) (cons x3 (ite ((_ is cons) (ite ((_ is node) x3) (children x3) null)) (cdr (ite ((_ is node) x3) (children x3) null)) null))))) ((_ is null) null)) (not (is-leaf (leaf x1)))))
(check-sat)
(exit)


