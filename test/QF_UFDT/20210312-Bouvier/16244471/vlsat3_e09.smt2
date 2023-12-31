(set-info :smt-lib-version 2.6)
(set-logic QF_UFDT)
(set-info :source |
Generated by: Pierre Bouvier
Generated on: 2021-03-12
Application: Automatic decomposition of Petri Nets into Automata Networks
Target solver: CVC4, Z3
Publications:

[1] Pierre Bouvier, Hubert Garavel, and Hernan Ponce de Leon.
    "Automatic Decomposition of Petri Nets into Automata Networks -
    A Synthetic Account". Proceedings PETRI NETS 2020, LNCS 12152,
    Springer. https://doi.org/10.1007/978-3-030-51831-8_1

[2] Hubert Garavel. "Nested-Unit Petri Nets". Journal of Logical
    and Algebraic Methods in Programming, vol. 104, Elsevier, 2019. 
    https://doi.org/10.1016/j.jlamp.2018.11.005

In [1], several methods for decomposing an ordinary, safe Petri net
into a flat, unit-safe NUPN [2], have been proposed. These methods
are implemented in a complete tool chain involving SAT solvers, SMT
solvers, and tools for graph coloring and finding maximal cliques.
From a data set of 12,000+ NUPN models, 51,000+ SMT formulas have
been generated, out of which a subset of 1200 interesting formulas
to be used as SMT-LIB 2.6 benchmarks was carefully selected.

Original filename: vlsat3_e09.smt2

Specific parameters for the present benchmark:
- number of places: 39
- number of units: 14
- number of edges in the concurrency graph: 438
- number of variables: 0
- number of uninterpreted functions: 1
- number of asserts: 451
- total number of operators in asserts: 2500
|)
(set-info :license "https://creativecommons.org/licenses/by/4.0/")
(set-info :category "industrial")
(set-info :status unsat)

(declare-datatypes ((Place 0)) (((p1) (p2) (p3) (p4) (p5) (p6) (p7) (p8) (p9) (p10) (p11) (p12) (p13) (p14) (p15) (p16) (p17) (p18) (p19) (p20) (p21) (p22) (p23) (p24) (p25) (p26) (p27) (p28) (p29) (p30) (p31) (p32) (p33) (p34) (p35) (p36) (p37) (p38) (p39))))
(declare-datatypes ((Unit 0)) (((u0) (u1) (u2) (u3) (u4) (u5) (u6) (u7) (u8) (u9) (u10) (u11) (u12) (u13))))
(declare-fun u (Place) Unit)
(assert (= (u p1) u0))
(assert (or (= (u p2) u0) (= (u p2) u1)))
(assert (or (= (u p3) u0) (= (u p3) u1) (= (u p3) u2)))
(assert (or (= (u p4) u0) (= (u p4) u1) (= (u p4) u2) (= (u p4) u3)))
(assert (or (= (u p5) u0) (= (u p5) u1) (= (u p5) u2) (= (u p5) u3) (= (u p5) u4)))
(assert (or (= (u p6) u0) (= (u p6) u1) (= (u p6) u2) (= (u p6) u3) (= (u p6) u4) (= (u p6) u5)))
(assert (or (= (u p7) u0) (= (u p7) u1) (= (u p7) u2) (= (u p7) u3) (= (u p7) u4) (= (u p7) u5) (= (u p7) u6)))
(assert (or (= (u p8) u0) (= (u p8) u1) (= (u p8) u2) (= (u p8) u3) (= (u p8) u4) (= (u p8) u5) (= (u p8) u6) (= (u p8) u7)))
(assert (or (= (u p9) u0) (= (u p9) u1) (= (u p9) u2) (= (u p9) u3) (= (u p9) u4) (= (u p9) u5) (= (u p9) u6) (= (u p9) u7) (= (u p9) u8)))
(assert (or (= (u p10) u0) (= (u p10) u1) (= (u p10) u2) (= (u p10) u3) (= (u p10) u4) (= (u p10) u5) (= (u p10) u6) (= (u p10) u7) (= (u p10) u8) (= (u p10) u9)))
(assert (or (= (u p11) u0) (= (u p11) u1) (= (u p11) u2) (= (u p11) u3) (= (u p11) u4) (= (u p11) u5) (= (u p11) u6) (= (u p11) u7) (= (u p11) u8) (= (u p11) u9) (= (u p11) u10)))
(assert (or (= (u p12) u0) (= (u p12) u1) (= (u p12) u2) (= (u p12) u3) (= (u p12) u4) (= (u p12) u5) (= (u p12) u6) (= (u p12) u7) (= (u p12) u8) (= (u p12) u9) (= (u p12) u10) (= (u p12) u11)))
(assert (or (= (u p13) u0) (= (u p13) u1) (= (u p13) u2) (= (u p13) u3) (= (u p13) u4) (= (u p13) u5) (= (u p13) u6) (= (u p13) u7) (= (u p13) u8) (= (u p13) u9) (= (u p13) u10) (= (u p13) u11) (= (u p13) u12)))
(assert (distinct (u p5) (u p31)))
(assert (distinct (u p20) (u p25)))
(assert (distinct (u p3) (u p35)))
(assert (distinct (u p21) (u p28)))
(assert (distinct (u p4) (u p36)))
(assert (distinct (u p7) (u p25)))
(assert (distinct (u p23) (u p26)))
(assert (distinct (u p13) (u p32)))
(assert (distinct (u p14) (u p31)))
(assert (distinct (u p1) (u p28)))
(assert (distinct (u p13) (u p20)))
(assert (distinct (u p2) (u p27)))
(assert (distinct (u p19) (u p39)))
(assert (distinct (u p27) (u p39)))
(assert (distinct (u p6) (u p23)))
(assert (distinct (u p21) (u p37)))
(assert (distinct (u p7) (u p22)))
(assert (distinct (u p29) (u p37)))
(assert (distinct (u p33) (u p34)))
(assert (distinct (u p16) (u p38)))
(assert (distinct (u p10) (u p36)))
(assert (distinct (u p17) (u p39)))
(assert (distinct (u p10) (u p14)))
(assert (distinct (u p1) (u p21)))
(assert (distinct (u p3) (u p11)))
(assert (distinct (u p1) (u p15)))
(assert (distinct (u p4) (u p12)))
(assert (distinct (u p24) (u p33)))
(assert (distinct (u p3) (u p17)))
(assert (distinct (u p1) (u p33)))
(assert (distinct (u p7) (u p15)))
(assert (distinct (u p16) (u p29)))
(assert (distinct (u p7) (u p37)))
(assert (distinct (u p18) (u p31)))
(assert (distinct (u p8) (u p25)))
(assert (distinct (u p10) (u p19)))
(assert (distinct (u p12) (u p29)))
(assert (distinct (u p27) (u p31)))
(assert (distinct (u p4) (u p11)))
(assert (distinct (u p2) (u p7)))
(assert (distinct (u p5) (u p10)))
(assert (distinct (u p3) (u p22)))
(assert (distinct (u p25) (u p37)))
(assert (distinct (u p4) (u p17)))
(assert (distinct (u p19) (u p27)))
(assert (distinct (u p2) (u p33)))
(assert (distinct (u p16) (u p20)))
(assert (distinct (u p6) (u p27)))
(assert (distinct (u p23) (u p33)))
(assert (distinct (u p7) (u p34)))
(assert (distinct (u p31) (u p33)))
(assert (distinct (u p23) (u p31)))
(assert (distinct (u p13) (u p37)))
(assert (distinct (u p9) (u p29)))
(assert (distinct (u p10) (u p26)))
(assert (distinct (u p1) (u p25)))
(assert (distinct (u p13) (u p25)))
(assert (distinct (u p14) (u p38)))
(assert (distinct (u p3) (u p31)))
(assert (distinct (u p4) (u p24)))
(assert (distinct (u p5) (u p29)))
(assert (distinct (u p20) (u p27)))
(assert (distinct (u p3) (u p37)))
(assert (distinct (u p27) (u p32)))
(assert (distinct (u p21) (u p26)))
(assert (distinct (u p4) (u p38)))
(assert (distinct (u p8) (u p33)))
(assert (distinct (u p7) (u p27)))
(assert (distinct (u p16) (u p33)))
(assert (distinct (u p14) (u p29)))
(assert (distinct (u p1) (u p18)))
(assert (distinct (u p13) (u p18)))
(assert (distinct (u p2) (u p25)))
(assert (distinct (u p3) (u p4)))
(assert (distinct (u p4) (u p7)))
(assert (distinct (u p6) (u p21)))
(assert (distinct (u p21) (u p35)))
(assert (distinct (u p7) (u p16)))
(assert (distinct (u p29) (u p35)))
(assert (distinct (u p16) (u p24)))
(assert (distinct (u p10) (u p34)))
(assert (distinct (u p10) (u p22)))
(assert (distinct (u p2) (u p16)))
(assert (distinct (u p3) (u p13)))
(assert (distinct (u p1) (u p13)))
(assert (distinct (u p4) (u p14)))
(assert (distinct (u p2) (u p10)))
(assert (distinct (u p3) (u p19)))
(assert (distinct (u p1) (u p39)))
(assert (distinct (u p25) (u p34)))
(assert (distinct (u p4) (u p20)))
(assert (distinct (u p20) (u p23)))
(assert (distinct (u p16) (u p31)))
(assert (distinct (u p21) (u p22)))
(assert (distinct (u p7) (u p39)))
(assert (distinct (u p18) (u p29)))
(assert (distinct (u p8) (u p27)))
(assert (distinct (u p10) (u p17)))
(assert (distinct (u p25) (u p31)))
(assert (distinct (u p12) (u p31)))
(assert (distinct (u p1) (u p6)))
(assert (distinct (u p13) (u p30)))
(assert (distinct (u p3) (u p24)))
(assert (distinct (u p6) (u p7)))
(assert (distinct (u p4) (u p19)))
(assert (distinct (u p19) (u p29)))
(assert (distinct (u p16) (u p22)))
(assert (distinct (u p6) (u p25)))
(assert (distinct (u p21) (u p31)))
(assert (distinct (u p23) (u p35)))
(assert (distinct (u p17) (u p23)))
(assert (distinct (u p7) (u p28)))
(assert (distinct (u p31) (u p35)))
(assert (distinct (u p23) (u p25)))
(assert (distinct (u p13) (u p35)))
(assert (distinct (u p10) (u p24)))
(assert (distinct (u p15) (u p29)))
(assert (distinct (u p11) (u p21)))
(assert (distinct (u p1) (u p31)))
(assert (distinct (u p13) (u p23)))
(assert (distinct (u p15) (u p33)))
(assert (distinct (u p4) (u p26)))
(assert (distinct (u p5) (u p27)))
(assert (distinct (u p3) (u p39)))
(assert (distinct (u p27) (u p34)))
(assert (distinct (u p6) (u p16)))
(assert (distinct (u p21) (u p24)))
(assert (distinct (u p4) (u p32)))
(assert (distinct (u p7) (u p21)))
(assert (distinct (u p16) (u p35)))
(assert (distinct (u p12) (u p13)))
(assert (distinct (u p1) (u p16)))
(assert (distinct (u p13) (u p16)))
(assert (distinct (u p2) (u p23)))
(assert (distinct (u p3) (u p6)))
(assert (distinct (u p1) (u p10)))
(assert (distinct (u p21) (u p33)))
(assert (distinct (u p7) (u p18)))
(assert (distinct (u p32) (u p39)))
(assert (distinct (u p29) (u p33)))
(assert (distinct (u p33) (u p38)))
(assert (distinct (u p16) (u p26)))
(assert (distinct (u p10) (u p32)))
(assert (distinct (u p17) (u p27)))
(assert (distinct (u p9) (u p13)))
(assert (distinct (u p9) (u p23)))
(assert (distinct (u p10) (u p20)))
(assert (distinct (u p11) (u p25)))
(assert (distinct (u p3) (u p15)))
(assert (distinct (u p1) (u p3)))
(assert (distinct (u p4) (u p8)))
(assert (distinct (u p5) (u p13)))
(assert (distinct (u p3) (u p21)))
(assert (distinct (u p1) (u p37)))
(assert (distinct (u p25) (u p32)))
(assert (distinct (u p4) (u p22)))
(assert (distinct (u p26) (u p39)))
(assert (distinct (u p7) (u p11)))
(assert (distinct (u p22) (u p33)))
(assert (distinct (u p5) (u p23)))
(assert (distinct (u p23) (u p36)))
(assert (distinct (u p30) (u p33)))
(assert (distinct (u p7) (u p33)))
(assert (distinct (u p22) (u p27)))
(assert (distinct (u p31) (u p36)))
(assert (distinct (u p18) (u p27)))
(assert (distinct (u p12) (u p37)))
(assert (distinct (u p8) (u p29)))
(assert (distinct (u p14) (u p23)))
(assert (distinct (u p10) (u p31)))
(assert (distinct (u p25) (u p29)))
(assert (distinct (u p12) (u p25)))
(assert (distinct (u p1) (u p4)))
(assert (distinct (u p13) (u p28)))
(assert (distinct (u p2) (u p3)))
(assert (distinct (u p3) (u p26)))
(assert (distinct (u p4) (u p29)))
(assert (distinct (u p19) (u p31)))
(assert (distinct (u p5) (u p16)))
(assert (distinct (u p3) (u p32)))
(assert (distinct (u p6) (u p31)))
(assert (distinct (u p21) (u p29)))
(assert (distinct (u p17) (u p21)))
(assert (distinct (u p7) (u p30)))
(assert (distinct (u p23) (u p27)))
(assert (distinct (u p13) (u p33)))
(assert (distinct (u p15) (u p31)))
(assert (distinct (u p11) (u p23)))
(assert (distinct (u p12) (u p16)))
(assert (distinct (u p1) (u p29)))
(assert (distinct (u p13) (u p21)))
(assert (distinct (u p5) (u p25)))
(assert (distinct (u p20) (u p39)))
(assert (distinct (u p27) (u p36)))
(assert (distinct (u p21) (u p38)))
(assert (distinct (u p4) (u p34)))
(assert (distinct (u p28) (u p39)))
(assert (distinct (u p7) (u p23)))
(assert (distinct (u p5) (u p35)))
(assert (distinct (u p29) (u p38)))
(assert (distinct (u p33) (u p35)))
(assert (distinct (u p16) (u p37)))
(assert (distinct (u p10) (u p39)))
(assert (distinct (u p9) (u p10)))
(assert (distinct (u p18) (u p39)))
(assert (distinct (u p1) (u p22)))
(assert (distinct (u p2) (u p21)))
(assert (distinct (u p3) (u p8)))
(assert (distinct (u p1) (u p8)))
(assert (distinct (u p1) (u p34)))
(assert (distinct (u p7) (u p12)))
(assert (distinct (u p33) (u p36)))
(assert (distinct (u p16) (u p28)))
(assert (distinct (u p6) (u p35)))
(assert (distinct (u p17) (u p25)))
(assert (distinct (u p9) (u p21)))
(assert (distinct (u p24) (u p27)))
(assert (distinct (u p10) (u p18)))
(assert (distinct (u p25) (u p26)))
(assert (distinct (u p11) (u p27)))
(assert (distinct (u p27) (u p28)))
(assert (distinct (u p4) (u p10)))
(assert (distinct (u p28) (u p31)))
(assert (distinct (u p24) (u p39)))
(assert (distinct (u p29) (u p30)))
(assert (distinct (u p3) (u p23)))
(assert (distinct (u p25) (u p38)))
(assert (distinct (u p4) (u p16)))
(assert (distinct (u p22) (u p39)))
(assert (distinct (u p5) (u p21)))
(assert (distinct (u p16) (u p19)))
(assert (distinct (u p23) (u p38)))
(assert (distinct (u p30) (u p39)))
(assert (distinct (u p7) (u p35)))
(assert (distinct (u p22) (u p25)))
(assert (distinct (u p31) (u p38)))
(assert (distinct (u p18) (u p25)))
(assert (distinct (u p23) (u p28)))
(assert (distinct (u p8) (u p31)))
(assert (distinct (u p14) (u p21)))
(assert (distinct (u p10) (u p29)))
(assert (distinct (u p15) (u p16)))
(assert (distinct (u p11) (u p16)))
(assert (distinct (u p12) (u p27)))
(assert (distinct (u p1) (u p26)))
(assert (distinct (u p13) (u p26)))
(assert (distinct (u p14) (u p33)))
(assert (distinct (u p3) (u p28)))
(assert (distinct (u p4) (u p31)))
(assert (distinct (u p19) (u p33)))
(assert (distinct (u p27) (u p33)))
(assert (distinct (u p6) (u p29)))
(assert (distinct (u p21) (u p27)))
(assert (distinct (u p4) (u p37)))
(assert (distinct (u p7) (u p24)))
(assert (distinct (u p16) (u p32)))
(assert (distinct (u p15) (u p25)))
(assert (distinct (u p1) (u p19)))
(assert (distinct (u p13) (u p19)))
(assert (distinct (u p3) (u p5)))
(assert (distinct (u p5) (u p7)))
(assert (distinct (u p20) (u p33)))
(assert (distinct (u p27) (u p38)))
(assert (distinct (u p21) (u p36)))
(assert (distinct (u p28) (u p33)))
(assert (distinct (u p7) (u p17)))
(assert (distinct (u p5) (u p33)))
(assert (distinct (u p29) (u p36)))
(assert (distinct (u p16) (u p39)))
(assert (distinct (u p8) (u p13)))
(assert (distinct (u p10) (u p15)))
(assert (distinct (u p1) (u p20)))
(assert (distinct (u p3) (u p10)))
(assert (distinct (u p1) (u p14)))
(assert (distinct (u p4) (u p13)))
(assert (distinct (u p2) (u p13)))
(assert (distinct (u p3) (u p16)))
(assert (distinct (u p1) (u p32)))
(assert (distinct (u p25) (u p35)))
(assert (distinct (u p19) (u p21)))
(assert (distinct (u p2) (u p39)))
(assert (distinct (u p7) (u p14)))
(assert (distinct (u p16) (u p30)))
(assert (distinct (u p6) (u p33)))
(assert (distinct (u p21) (u p23)))
(assert (distinct (u p17) (u p31)))
(assert (distinct (u p11) (u p33)))
(assert (distinct (u p9) (u p27)))
(assert (distinct (u p24) (u p29)))
(assert (distinct (u p14) (u p16)))
(assert (distinct (u p10) (u p16)))
(assert (distinct (u p15) (u p21)))
(assert (distinct (u p11) (u p29)))
(assert (distinct (u p26) (u p31)))
(assert (distinct (u p27) (u p30)))
(assert (distinct (u p1) (u p7)))
(assert (distinct (u p13) (u p31)))
(assert (distinct (u p2) (u p4)))
(assert (distinct (u p3) (u p25)))
(assert (distinct (u p25) (u p36)))
(assert (distinct (u p4) (u p18)))
(assert (distinct (u p20) (u p29)))
(assert (distinct (u p16) (u p21)))
(assert (distinct (u p23) (u p32)))
(assert (distinct (u p7) (u p29)))
(assert (distinct (u p22) (u p31)))
(assert (distinct (u p31) (u p32)))
(assert (distinct (u p18) (u p23)))
(assert (distinct (u p23) (u p30)))
(assert (distinct (u p12) (u p33)))
(assert (distinct (u p13) (u p36)))
(assert (distinct (u p14) (u p27)))
(assert (distinct (u p10) (u p27)))
(assert (distinct (u p12) (u p21)))
(assert (distinct (u p1) (u p24)))
(assert (distinct (u p13) (u p24)))
(assert (distinct (u p2) (u p31)))
(assert (distinct (u p3) (u p30)))
(assert (distinct (u p15) (u p38)))
(assert (distinct (u p4) (u p25)))
(assert (distinct (u p3) (u p36)))
(assert (distinct (u p27) (u p35)))
(assert (distinct (u p21) (u p25)))
(assert (distinct (u p4) (u p39)))
(assert (distinct (u p7) (u p26)))
(assert (distinct (u p16) (u p34)))
(assert (distinct (u p15) (u p27)))
(assert (distinct (u p1) (u p17)))
(assert (distinct (u p13) (u p17)))
(assert (distinct (u p3) (u p7)))
(assert (distinct (u p1) (u p11)))
(assert (distinct (u p6) (u p10)))
(assert (distinct (u p21) (u p34)))
(assert (distinct (u p7) (u p19)))
(assert (distinct (u p29) (u p34)))
(assert (distinct (u p9) (u p36)))
(assert (distinct (u p33) (u p39)))
(assert (distinct (u p16) (u p25)))
(assert (distinct (u p10) (u p35)))
(assert (distinct (u p10) (u p13)))
(assert (distinct (u p8) (u p21)))
(assert (distinct (u p9) (u p16)))
(assert (distinct (u p10) (u p23)))
(assert (distinct (u p3) (u p12)))
(assert (distinct (u p1) (u p12)))
(assert (distinct (u p4) (u p15)))
(assert (distinct (u p3) (u p18)))
(assert (distinct (u p1) (u p38)))
(assert (distinct (u p25) (u p33)))
(assert (distinct (u p6) (u p13)))
(assert (distinct (u p4) (u p21)))
(assert (distinct (u p19) (u p23)))
(assert (distinct (u p23) (u p37)))
(assert (distinct (u p17) (u p29)))
(assert (distinct (u p7) (u p38)))
(assert (distinct (u p31) (u p37)))
(assert (distinct (u p9) (u p25)))
(assert (distinct (u p24) (u p31)))
(assert (distinct (u p10) (u p30)))
(assert (distinct (u p25) (u p30)))
(assert (distinct (u p15) (u p23)))
(assert (distinct (u p11) (u p31)))
(assert (distinct (u p26) (u p29)))
(assert (distinct (u p1) (u p5)))
(assert (distinct (u p13) (u p29)))
(assert (distinct (u p3) (u p27)))
(assert (distinct (u p4) (u p28)))
(assert (distinct (u p26) (u p33)))
(assert (distinct (u p20) (u p31)))
(assert (distinct (u p3) (u p33)))
(assert (distinct (u p16) (u p23)))
(assert (distinct (u p23) (u p34)))
(assert (distinct (u p21) (u p30)))
(assert (distinct (u p7) (u p31)))
(assert (distinct (u p22) (u p29)))
(assert (distinct (u p31) (u p34)))
(assert (distinct (u p18) (u p21)))
(assert (distinct (u p23) (u p24)))
(assert (distinct (u p13) (u p34)))
(assert (distinct (u p14) (u p25)))
(assert (distinct (u p10) (u p25)))
(assert (distinct (u p12) (u p23)))
(assert (distinct (u p1) (u p30)))
(assert (distinct (u p13) (u p22)))
(assert (distinct (u p2) (u p29)))
(assert (distinct (u p4) (u p27)))
(assert (distinct (u p3) (u p38)))
(assert (distinct (u p27) (u p37)))
(assert (distinct (u p21) (u p39)))
(assert (distinct (u p4) (u p33)))
(assert (distinct (u p8) (u p36)))
(assert (distinct (u p7) (u p20)))
(assert (distinct (u p29) (u p39)))
(assert (distinct (u p9) (u p33)))
(assert (distinct (u p16) (u p36)))
(assert (distinct (u p10) (u p38)))
(assert (distinct (u p8) (u p10)))
(assert (distinct (u p17) (u p33)))
(assert (distinct (u p8) (u p16)))
(assert (distinct (u p11) (u p13)))
(assert (distinct (u p1) (u p23)))
(assert (distinct (u p3) (u p9)))
(assert (distinct (u p1) (u p9)))
(assert (distinct (u p1) (u p35)))
(assert (distinct (u p21) (u p32)))
(assert (distinct (u p7) (u p13)))
(assert (distinct (u p29) (u p32)))
(assert (distinct (u p33) (u p37)))
(assert (distinct (u p16) (u p27)))
(assert (distinct (u p10) (u p33)))
(assert (distinct (u p18) (u p33)))
(assert (distinct (u p8) (u p23)))
(assert (distinct (u p10) (u p21)))
(assert (distinct (u p25) (u p27)))
(assert (distinct (u p3) (u p14)))
(assert (distinct (u p27) (u p29)))
(assert (distinct (u p4) (u p9)))
(assert (distinct (u p29) (u p31)))
(assert (distinct (u p3) (u p20)))
(assert (distinct (u p1) (u p36)))
(assert (distinct (u p25) (u p39)))
(assert (distinct (u p4) (u p23)))
(assert (distinct (u p19) (u p25)))
(assert (distinct (u p7) (u p10)))
(assert (distinct (u p16) (u p18)))
(assert (distinct (u p23) (u p39)))
(assert (distinct (u p7) (u p32)))
(assert (distinct (u p31) (u p39)))
(assert (distinct (u p11) (u p37)))
(assert (distinct (u p23) (u p29)))
(assert (distinct (u p13) (u p39)))
(assert (distinct (u p9) (u p31)))
(assert (distinct (u p10) (u p28)))
(assert (distinct (u p25) (u p28)))
(assert (distinct (u p1) (u p27)))
(assert (distinct (u p13) (u p27)))
(assert (distinct (u p3) (u p29)))
(assert (distinct (u p4) (u p30)))
(check-sat)
(exit)
