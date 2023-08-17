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
(set-info :status sat)


(declare-datatypes ((nat 0)(list 0)(tree 0)) (((succ (pred nat)) (zero))
((cons (car tree) (cdr list)) (null))
((node (children list)) (leaf (data nat)))
))
(declare-fun x1 () nat)
(declare-fun x2 () nat)
(declare-fun x3 () nat)
(declare-fun x4 () nat)
(declare-fun x5 () nat)
(declare-fun x6 () nat)
(declare-fun x7 () nat)
(declare-fun x8 () nat)
(declare-fun x9 () nat)
(declare-fun x10 () nat)
(declare-fun x11 () list)
(declare-fun x12 () list)
(declare-fun x13 () list)
(declare-fun x14 () list)
(declare-fun x15 () list)
(declare-fun x16 () list)
(declare-fun x17 () list)
(declare-fun x18 () list)
(declare-fun x19 () list)
(declare-fun x20 () list)
(declare-fun x21 () tree)
(declare-fun x22 () tree)
(declare-fun x23 () tree)
(declare-fun x24 () tree)
(declare-fun x25 () tree)
(declare-fun x26 () tree)
(declare-fun x27 () tree)
(declare-fun x28 () tree)
(declare-fun x29 () tree)
(declare-fun x30 () tree)

(assert (and (not (= x4 (pred x10))) (is-leaf x22)))
(check-sat)
(exit)


