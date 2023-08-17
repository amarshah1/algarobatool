; 
(set-info :status unknown)
(set-logic QF_DT)
(declare-datatypes ((Enum_A_B_C_D_E_F 0)) (((A) (B) (C) (D) (E) (F))))
 (declare-datatypes ((Tower 0)) (((stack (top Enum_A_B_C_D_E_F) (rest Tower)) (empty))))
 (declare-datatypes ((Record_left_center_right 0)) (((Record_left_center_right (left Tower) (center Tower) (right Tower)))))
 (declare-datatypes ((Enum_left-to-center_left-to-right_center-to-left_center-to-right_right-to-left_right-to-center 0)) (((left-to-center) (left-to-right) (center-to-left) (center-to-right) (right-to-left) (right-to-center))))
 (declare-fun s () Record_left_center_right)
(declare-fun s_tmp () Record_left_center_right)
(declare-fun c () Enum_left-to-center_left-to-right_center-to-left_center-to-right_right-to-left_right-to-center)
(declare-fun c_tmp () Enum_left-to-center_left-to-right_center-to-left_center-to-right_right-to-left_right-to-center)
(declare-fun s_tmp_ () Record_left_center_right)
(declare-fun s_tmp__ () Record_left_center_right)
(declare-fun s_tmp___ () Record_left_center_right)
(declare-fun c!0 () Enum_left-to-center_left-to-right_center-to-left_center-to-right_right-to-left_right-to-center)
(declare-fun c_tmp____ () Enum_left-to-center_left-to-right_center-to-left_center-to-right_right-to-left_right-to-center)
(declare-fun s_ () Record_left_center_right)
(declare-fun c_ () Enum_left-to-center_left-to-right_center-to-left_center-to-right_right-to-left_right-to-center)
(declare-fun c!1 () Enum_left-to-center_left-to-right_center-to-left_center-to-right_right-to-left_right-to-center)
(declare-fun c__ () Enum_left-to-center_left-to-right_center-to-left_center-to-right_right-to-left_right-to-center)
(declare-fun s__ () Record_left_center_right)
(declare-fun c!2 () Enum_left-to-center_left-to-right_center-to-left_center-to-right_right-to-left_right-to-center)
(declare-fun c___ () Enum_left-to-center_left-to-right_center-to-left_center-to-right_right-to-left_right-to-center)
(declare-fun s___ () Record_left_center_right)
(declare-fun c!3 () Enum_left-to-center_left-to-right_center-to-left_center-to-right_right-to-left_right-to-center)
(declare-fun c____ () Enum_left-to-center_left-to-right_center-to-left_center-to-right_right-to-left_right-to-center)
(declare-fun s____ () Record_left_center_right)
(declare-fun c!4 () Enum_left-to-center_left-to-right_center-to-left_center-to-right_right-to-left_right-to-center)
(declare-fun c_____ () Enum_left-to-center_left-to-right_center-to-left_center-to-right_right-to-left_right-to-center)
(declare-fun s_____ () Record_left_center_right)
(declare-fun c!5 () Enum_left-to-center_left-to-right_center-to-left_center-to-right_right-to-left_right-to-center)
(declare-fun c______ () Enum_left-to-center_left-to-right_center-to-left_center-to-right_right-to-left_right-to-center)
(declare-fun s______ () Record_left_center_right)
(declare-fun c!6 () Enum_left-to-center_left-to-right_center-to-left_center-to-right_right-to-left_right-to-center)
(declare-fun c_______ () Enum_left-to-center_left-to-right_center-to-left_center-to-right_right-to-left_right-to-center)
(declare-fun s_______ () Record_left_center_right)
(declare-fun c!7 () Enum_left-to-center_left-to-right_center-to-left_center-to-right_right-to-left_right-to-center)
(declare-fun c________ () Enum_left-to-center_left-to-right_center-to-left_center-to-right_right-to-left_right-to-center)
(declare-fun s________ () Record_left_center_right)
(declare-fun c!8 () Enum_left-to-center_left-to-right_center-to-left_center-to-right_right-to-left_right-to-center)
(declare-fun c_________ () Enum_left-to-center_left-to-right_center-to-left_center-to-right_right-to-left_right-to-center)
(declare-fun s_________ () Record_left_center_right)
(declare-fun c!9 () Enum_left-to-center_left-to-right_center-to-left_center-to-right_right-to-left_right-to-center)
(declare-fun c__________ () Enum_left-to-center_left-to-right_center-to-left_center-to-right_right-to-left_right-to-center)
(declare-fun s__________ () Record_left_center_right)
(assert
 (= s_tmp s))
(assert
 (= c_tmp c))
(assert
 (let ((?x76 (left s_tmp_)))
 (= ?x76 empty)))
(assert
 (let ((?x19 (stack E empty)))
 (let ((?x18 (stack A ?x19)))
 (let ((?x20 (stack B ?x18)))
 (let ((?x21 (stack C ?x20)))
 (let ((?x22 (stack D ?x21)))
 (let ((?x23 (stack F ?x22)))
 (let ((?x79 (center s_tmp__)))
 (= ?x79 ?x23)))))))))
(assert
 (let ((?x82 (right s_tmp___)))
 (= ?x82 empty)))
(assert
 (= c_tmp____ c!0))
(assert
 (= s_ s_tmp___))
(assert
 (= c_ c_tmp____))
(assert
 (let ((?x109 (left s_tmp__)))
 (= (left s_tmp___) ?x109)))
(assert
 (let ((?x79 (center s_tmp__)))
 (= (center s_tmp___) ?x79)))
(assert
 (let ((?x113 (center s_tmp_)))
 (= ?x113 (center s_tmp))))
(assert
 (let ((?x116 (right s_tmp_)))
 (= ?x116 (right s_tmp))))
(assert
 (let ((?x76 (left s_tmp_)))
 (let ((?x109 (left s_tmp__)))
 (= ?x109 ?x76))))
(assert
 (let ((?x116 (right s_tmp_)))
 (let ((?x120 (right s_tmp__)))
 (= ?x120 ?x116))))
(assert
 (= c__ c!1))
(assert
 (let ((?x53 (left s_)))
 (let ((?x182 (left s__)))
 (let (($x227 (= ?x182 ?x53)))
 (let ((?x56 (right s_)))
 (let (($x197 ((_ is stack ) ?x56)))
 (let (($x201 (and (= c_ right-to-left) $x197)))
 (let ((?x58 (center s_)))
 (let (($x202 ((_ is stack ) ?x58)))
 (let (($x203 (= c_ center-to-right)))
 (let (($x204 (and $x203 $x202)))
 (let (($x205 (= c_ center-to-left)))
 (let (($x206 (and $x205 $x202)))
 (let (($x236 (ite $x206 (= ?x182 (stack (top ?x58) ?x53)) (ite $x204 $x227 (ite $x201 (= ?x182 (stack (top ?x56) ?x53)) $x227)))))
 (let (($x214 (= ?x182 (rest ?x53))))
 (let (($x207 ((_ is stack ) ?x53)))
 (let (($x209 (and (= c_ left-to-right) $x207)))
 (let (($x211 (and (= c_ left-to-center) $x207)))
 (ite $x211 $x214 (ite $x209 $x214 $x236))))))))))))))))))))
(assert
 (let ((?x56 (right s_)))
 (let ((?x184 (right s__)))
 (let (($x223 (= ?x184 ?x56)))
 (let (($x220 (= ?x184 (rest ?x56))))
 (let (($x197 ((_ is stack ) ?x56)))
 (let (($x199 (and (= c_ right-to-center) $x197)))
 (let (($x201 (and (= c_ right-to-left) $x197)))
 (let ((?x58 (center s_)))
 (let (($x202 ((_ is stack ) ?x58)))
 (let (($x203 (= c_ center-to-right)))
 (let (($x204 (and $x203 $x202)))
 (let (($x233 (ite $x204 (= ?x184 (stack (top ?x58) ?x56)) (ite $x201 $x220 (ite $x199 $x220 $x223)))))
 (let (($x205 (= c_ center-to-left)))
 (let (($x206 (and $x205 $x202)))
 (let ((?x53 (left s_)))
 (let (($x207 ((_ is stack ) ?x53)))
 (let (($x209 (and (= c_ left-to-right) $x207)))
 (let (($x211 (and (= c_ left-to-center) $x207)))
 (ite $x211 (= ?x184 (stack (top ?x53) ?x56)) (ite $x209 $x223 (ite $x206 $x223 $x233))))))))))))))))))))))
(assert
 (let ((?x58 (center s_)))
 (let ((?x187 (center s__)))
 (let (($x225 (= ?x187 ?x58)))
 (let ((?x56 (right s_)))
 (let (($x197 ((_ is stack ) ?x56)))
 (let (($x199 (and (= c_ right-to-center) $x197)))
 (let (($x201 (and (= c_ right-to-left) $x197)))
 (let (($x217 (= ?x187 (rest ?x58))))
 (let (($x202 ((_ is stack ) ?x58)))
 (let (($x203 (= c_ center-to-right)))
 (let (($x204 (and $x203 $x202)))
 (let (($x232 (ite $x204 $x217 (ite $x201 $x225 (ite $x199 (= ?x187 (stack (top ?x56) ?x58)) $x225)))))
 (let (($x205 (= c_ center-to-left)))
 (let (($x206 (and $x205 $x202)))
 (let ((?x53 (left s_)))
 (let (($x207 ((_ is stack ) ?x53)))
 (let (($x209 (and (= c_ left-to-right) $x207)))
 (let (($x211 (and (= c_ left-to-center) $x207)))
 (ite $x211 $x225 (ite $x209 (= ?x187 (stack (top ?x53) ?x58)) (ite $x206 $x217 $x232))))))))))))))))))))))
(assert
 (= c___ c!2))
(assert
 (let ((?x182 (left s__)))
 (let ((?x123 (left s___)))
 (let (($x317 (= ?x123 ?x182)))
 (let ((?x184 (right s__)))
 (let ((?x284 (top ?x184)))
 (let ((?x285 (stack ?x284 ?x182)))
 (let (($x311 (= ?x123 ?x285)))
 (let (($x287 ((_ is stack ) ?x184)))
 (let (($x290 (= c__ right-to-left)))
 (let (($x291 (and $x290 $x287)))
 (let ((?x187 (center s__)))
 (let (($x292 ((_ is stack ) ?x187)))
 (let (($x293 (= c__ center-to-right)))
 (let (($x294 (and $x293 $x292)))
 (let ((?x280 (top ?x187)))
 (let ((?x281 (stack ?x280 ?x182)))
 (let (($x308 (= ?x123 ?x281)))
 (let (($x295 (= c__ center-to-left)))
 (let (($x296 (and $x295 $x292)))
 (let ((?x125 (rest ?x182)))
 (let (($x304 (= ?x123 ?x125)))
 (let (($x297 ((_ is stack ) ?x182)))
 (let (($x298 (= c__ left-to-right)))
 (let (($x299 (and $x298 $x297)))
 (let (($x300 (= c__ left-to-center)))
 (let (($x301 (and $x300 $x297)))
 (ite $x301 $x304 (ite $x299 $x304 (ite $x296 $x308 (ite $x294 $x317 (ite $x291 $x311 $x317))))))))))))))))))))))))))))))))
(assert
 (let ((?x184 (right s__)))
 (let ((?x126 (right s___)))
 (let (($x313 (= ?x126 ?x184)))
 (let ((?x283 (rest ?x184)))
 (let (($x310 (= ?x126 ?x283)))
 (let (($x287 ((_ is stack ) ?x184)))
 (let (($x288 (= c__ right-to-center)))
 (let (($x289 (and $x288 $x287)))
 (let (($x290 (= c__ right-to-left)))
 (let (($x291 (and $x290 $x287)))
 (let ((?x187 (center s__)))
 (let ((?x280 (top ?x187)))
 (let ((?x282 (stack ?x280 ?x184)))
 (let (($x309 (= ?x126 ?x282)))
 (let (($x292 ((_ is stack ) ?x187)))
 (let (($x293 (= c__ center-to-right)))
 (let (($x294 (and $x293 $x292)))
 (let (($x295 (= c__ center-to-left)))
 (let (($x296 (and $x295 $x292)))
 (let ((?x182 (left s__)))
 (let (($x297 ((_ is stack ) ?x182)))
 (let (($x298 (= c__ left-to-right)))
 (let (($x299 (and $x298 $x297)))
 (let (($x330 (ite $x299 $x313 (ite $x296 $x313 (ite $x294 $x309 (ite $x291 $x310 (ite $x289 $x310 $x313)))))))
 (let ((?x179 (top ?x182)))
 (let ((?x276 (stack ?x179 ?x184)))
 (let (($x305 (= ?x126 ?x276)))
 (let (($x300 (= c__ left-to-center)))
 (let (($x301 (and $x300 $x297)))
 (ite $x301 $x305 $x330)))))))))))))))))))))))))))))))
(assert
 (let ((?x187 (center s__)))
 (let ((?x277 (center s___)))
 (let (($x315 (= ?x277 ?x187)))
 (let ((?x184 (right s__)))
 (let ((?x284 (top ?x184)))
 (let ((?x286 (stack ?x284 ?x187)))
 (let (($x312 (= ?x277 ?x286)))
 (let (($x287 ((_ is stack ) ?x184)))
 (let (($x288 (= c__ right-to-center)))
 (let (($x289 (and $x288 $x287)))
 (let (($x290 (= c__ right-to-left)))
 (let (($x291 (and $x290 $x287)))
 (let ((?x279 (rest ?x187)))
 (let (($x307 (= ?x277 ?x279)))
 (let (($x292 ((_ is stack ) ?x187)))
 (let (($x293 (= c__ center-to-right)))
 (let (($x294 (and $x293 $x292)))
 (let (($x295 (= c__ center-to-left)))
 (let (($x296 (and $x295 $x292)))
 (let ((?x182 (left s__)))
 (let ((?x179 (top ?x182)))
 (let ((?x278 (stack ?x179 ?x187)))
 (let (($x306 (= ?x277 ?x278)))
 (let (($x297 ((_ is stack ) ?x182)))
 (let (($x298 (= c__ left-to-right)))
 (let (($x299 (and $x298 $x297)))
 (let (($x329 (ite $x299 $x306 (ite $x296 $x307 (ite $x294 $x307 (ite $x291 $x315 (ite $x289 $x312 $x315)))))))
 (let (($x300 (= c__ left-to-center)))
 (let (($x301 (and $x300 $x297)))
 (ite $x301 $x315 $x329)))))))))))))))))))))))))))))))
(assert
 (= c____ c!3))
(assert
 (let ((?x123 (left s___)))
 (let ((?x243 (left s____)))
 (let (($x455 (= ?x243 ?x123)))
 (let ((?x126 (right s___)))
 (let ((?x422 (top ?x126)))
 (let ((?x423 (stack ?x422 ?x123)))
 (let (($x449 (= ?x243 ?x423)))
 (let (($x425 ((_ is stack ) ?x126)))
 (let (($x428 (= c___ right-to-left)))
 (let (($x429 (and $x428 $x425)))
 (let ((?x277 (center s___)))
 (let (($x430 ((_ is stack ) ?x277)))
 (let (($x431 (= c___ center-to-right)))
 (let (($x432 (and $x431 $x430)))
 (let ((?x418 (top ?x277)))
 (let ((?x419 (stack ?x418 ?x123)))
 (let (($x446 (= ?x243 ?x419)))
 (let (($x433 (= c___ center-to-left)))
 (let (($x434 (and $x433 $x430)))
 (let ((?x258 (rest ?x123)))
 (let (($x442 (= ?x243 ?x258)))
 (let (($x435 ((_ is stack ) ?x123)))
 (let (($x436 (= c___ left-to-right)))
 (let (($x437 (and $x436 $x435)))
 (let (($x438 (= c___ left-to-center)))
 (let (($x439 (and $x438 $x435)))
 (ite $x439 $x442 (ite $x437 $x442 (ite $x434 $x446 (ite $x432 $x455 (ite $x429 $x449 $x455))))))))))))))))))))))))))))))))
(assert
 (let ((?x126 (right s___)))
 (let ((?x259 (right s____)))
 (let (($x451 (= ?x259 ?x126)))
 (let ((?x421 (rest ?x126)))
 (let (($x448 (= ?x259 ?x421)))
 (let (($x425 ((_ is stack ) ?x126)))
 (let (($x426 (= c___ right-to-center)))
 (let (($x427 (and $x426 $x425)))
 (let (($x428 (= c___ right-to-left)))
 (let (($x429 (and $x428 $x425)))
 (let ((?x277 (center s___)))
 (let ((?x418 (top ?x277)))
 (let ((?x420 (stack ?x418 ?x126)))
 (let (($x447 (= ?x259 ?x420)))
 (let (($x430 ((_ is stack ) ?x277)))
 (let (($x431 (= c___ center-to-right)))
 (let (($x432 (and $x431 $x430)))
 (let (($x433 (= c___ center-to-left)))
 (let (($x434 (and $x433 $x430)))
 (let ((?x123 (left s___)))
 (let (($x435 ((_ is stack ) ?x123)))
 (let (($x436 (= c___ left-to-right)))
 (let (($x437 (and $x436 $x435)))
 (let (($x468 (ite $x437 $x451 (ite $x434 $x451 (ite $x432 $x447 (ite $x429 $x448 (ite $x427 $x448 $x451)))))))
 (let ((?x275 (top ?x123)))
 (let ((?x414 (stack ?x275 ?x126)))
 (let (($x443 (= ?x259 ?x414)))
 (let (($x438 (= c___ left-to-center)))
 (let (($x439 (and $x438 $x435)))
 (ite $x439 $x443 $x468)))))))))))))))))))))))))))))))
(assert
 (let ((?x277 (center s___)))
 (let ((?x415 (center s____)))
 (let (($x453 (= ?x415 ?x277)))
 (let ((?x126 (right s___)))
 (let ((?x422 (top ?x126)))
 (let ((?x424 (stack ?x422 ?x277)))
 (let (($x450 (= ?x415 ?x424)))
 (let (($x425 ((_ is stack ) ?x126)))
 (let (($x426 (= c___ right-to-center)))
 (let (($x427 (and $x426 $x425)))
 (let (($x428 (= c___ right-to-left)))
 (let (($x429 (and $x428 $x425)))
 (let ((?x417 (rest ?x277)))
 (let (($x445 (= ?x415 ?x417)))
 (let (($x430 ((_ is stack ) ?x277)))
 (let (($x431 (= c___ center-to-right)))
 (let (($x432 (and $x431 $x430)))
 (let (($x433 (= c___ center-to-left)))
 (let (($x434 (and $x433 $x430)))
 (let ((?x123 (left s___)))
 (let ((?x275 (top ?x123)))
 (let ((?x416 (stack ?x275 ?x277)))
 (let (($x444 (= ?x415 ?x416)))
 (let (($x435 ((_ is stack ) ?x123)))
 (let (($x436 (= c___ left-to-right)))
 (let (($x437 (and $x436 $x435)))
 (let (($x467 (ite $x437 $x444 (ite $x434 $x445 (ite $x432 $x445 (ite $x429 $x453 (ite $x427 $x450 $x453)))))))
 (let (($x438 (= c___ left-to-center)))
 (let (($x439 (and $x438 $x435)))
 (ite $x439 $x453 $x467)))))))))))))))))))))))))))))))
(assert
 (= c_____ c!4))
(assert
 (let ((?x243 (left s____)))
 (let ((?x333 (left s_____)))
 (let (($x593 (= ?x333 ?x243)))
 (let ((?x259 (right s____)))
 (let ((?x560 (top ?x259)))
 (let ((?x561 (stack ?x560 ?x243)))
 (let (($x587 (= ?x333 ?x561)))
 (let (($x563 ((_ is stack ) ?x259)))
 (let (($x566 (= c____ right-to-left)))
 (let (($x567 (and $x566 $x563)))
 (let ((?x415 (center s____)))
 (let (($x568 ((_ is stack ) ?x415)))
 (let (($x569 (= c____ center-to-right)))
 (let (($x570 (and $x569 $x568)))
 (let ((?x556 (top ?x415)))
 (let ((?x557 (stack ?x556 ?x243)))
 (let (($x584 (= ?x333 ?x557)))
 (let (($x571 (= c____ center-to-left)))
 (let (($x572 (and $x571 $x568)))
 (let ((?x366 (rest ?x243)))
 (let (($x580 (= ?x333 ?x366)))
 (let (($x573 ((_ is stack ) ?x243)))
 (let (($x574 (= c____ left-to-right)))
 (let (($x575 (and $x574 $x573)))
 (let (($x576 (= c____ left-to-center)))
 (let (($x577 (and $x576 $x573)))
 (ite $x577 $x580 (ite $x575 $x580 (ite $x572 $x584 (ite $x570 $x593 (ite $x567 $x587 $x593))))))))))))))))))))))))))))))))
(assert
 (let ((?x259 (right s____)))
 (let ((?x367 (right s_____)))
 (let (($x589 (= ?x367 ?x259)))
 (let ((?x559 (rest ?x259)))
 (let (($x586 (= ?x367 ?x559)))
 (let (($x563 ((_ is stack ) ?x259)))
 (let (($x564 (= c____ right-to-center)))
 (let (($x565 (and $x564 $x563)))
 (let (($x566 (= c____ right-to-left)))
 (let (($x567 (and $x566 $x563)))
 (let ((?x415 (center s____)))
 (let ((?x556 (top ?x415)))
 (let ((?x558 (stack ?x556 ?x259)))
 (let (($x585 (= ?x367 ?x558)))
 (let (($x568 ((_ is stack ) ?x415)))
 (let (($x569 (= c____ center-to-right)))
 (let (($x570 (and $x569 $x568)))
 (let (($x571 (= c____ center-to-left)))
 (let (($x572 (and $x571 $x568)))
 (let ((?x243 (left s____)))
 (let (($x573 ((_ is stack ) ?x243)))
 (let (($x574 (= c____ left-to-right)))
 (let (($x575 (and $x574 $x573)))
 (let (($x606 (ite $x575 $x589 (ite $x572 $x589 (ite $x570 $x585 (ite $x567 $x586 (ite $x565 $x586 $x589)))))))
 (let ((?x413 (top ?x243)))
 (let ((?x552 (stack ?x413 ?x259)))
 (let (($x581 (= ?x367 ?x552)))
 (let (($x576 (= c____ left-to-center)))
 (let (($x577 (and $x576 $x573)))
 (ite $x577 $x581 $x606)))))))))))))))))))))))))))))))
(assert
 (let ((?x415 (center s____)))
 (let ((?x553 (center s_____)))
 (let (($x591 (= ?x553 ?x415)))
 (let ((?x259 (right s____)))
 (let ((?x560 (top ?x259)))
 (let ((?x562 (stack ?x560 ?x415)))
 (let (($x588 (= ?x553 ?x562)))
 (let (($x563 ((_ is stack ) ?x259)))
 (let (($x564 (= c____ right-to-center)))
 (let (($x565 (and $x564 $x563)))
 (let (($x566 (= c____ right-to-left)))
 (let (($x567 (and $x566 $x563)))
 (let ((?x555 (rest ?x415)))
 (let (($x583 (= ?x553 ?x555)))
 (let (($x568 ((_ is stack ) ?x415)))
 (let (($x569 (= c____ center-to-right)))
 (let (($x570 (and $x569 $x568)))
 (let (($x571 (= c____ center-to-left)))
 (let (($x572 (and $x571 $x568)))
 (let ((?x243 (left s____)))
 (let ((?x413 (top ?x243)))
 (let ((?x554 (stack ?x413 ?x415)))
 (let (($x582 (= ?x553 ?x554)))
 (let (($x573 ((_ is stack ) ?x243)))
 (let (($x574 (= c____ left-to-right)))
 (let (($x575 (and $x574 $x573)))
 (let (($x605 (ite $x575 $x582 (ite $x572 $x583 (ite $x570 $x583 (ite $x567 $x591 (ite $x565 $x588 $x591)))))))
 (let (($x576 (= c____ left-to-center)))
 (let (($x577 (and $x576 $x573)))
 (ite $x577 $x591 $x605)))))))))))))))))))))))))))))))
(assert
 (= c______ c!5))
(assert
 (let ((?x333 (left s_____)))
 (let ((?x471 (left s______)))
 (let (($x731 (= ?x471 ?x333)))
 (let ((?x367 (right s_____)))
 (let ((?x698 (top ?x367)))
 (let ((?x699 (stack ?x698 ?x333)))
 (let (($x725 (= ?x471 ?x699)))
 (let (($x701 ((_ is stack ) ?x367)))
 (let (($x704 (= c_____ right-to-left)))
 (let (($x705 (and $x704 $x701)))
 (let ((?x553 (center s_____)))
 (let (($x706 ((_ is stack ) ?x553)))
 (let (($x707 (= c_____ center-to-right)))
 (let (($x708 (and $x707 $x706)))
 (let ((?x694 (top ?x553)))
 (let ((?x695 (stack ?x694 ?x333)))
 (let (($x722 (= ?x471 ?x695)))
 (let (($x709 (= c_____ center-to-left)))
 (let (($x710 (and $x709 $x706)))
 (let ((?x504 (rest ?x333)))
 (let (($x718 (= ?x471 ?x504)))
 (let (($x711 ((_ is stack ) ?x333)))
 (let (($x712 (= c_____ left-to-right)))
 (let (($x713 (and $x712 $x711)))
 (let (($x714 (= c_____ left-to-center)))
 (let (($x715 (and $x714 $x711)))
 (ite $x715 $x718 (ite $x713 $x718 (ite $x710 $x722 (ite $x708 $x731 (ite $x705 $x725 $x731))))))))))))))))))))))))))))))))
(assert
 (let ((?x367 (right s_____)))
 (let ((?x505 (right s______)))
 (let (($x727 (= ?x505 ?x367)))
 (let ((?x697 (rest ?x367)))
 (let (($x724 (= ?x505 ?x697)))
 (let (($x701 ((_ is stack ) ?x367)))
 (let (($x702 (= c_____ right-to-center)))
 (let (($x703 (and $x702 $x701)))
 (let (($x704 (= c_____ right-to-left)))
 (let (($x705 (and $x704 $x701)))
 (let ((?x553 (center s_____)))
 (let ((?x694 (top ?x553)))
 (let ((?x696 (stack ?x694 ?x367)))
 (let (($x723 (= ?x505 ?x696)))
 (let (($x706 ((_ is stack ) ?x553)))
 (let (($x707 (= c_____ center-to-right)))
 (let (($x708 (and $x707 $x706)))
 (let (($x709 (= c_____ center-to-left)))
 (let (($x710 (and $x709 $x706)))
 (let ((?x333 (left s_____)))
 (let (($x711 ((_ is stack ) ?x333)))
 (let (($x712 (= c_____ left-to-right)))
 (let (($x713 (and $x712 $x711)))
 (let (($x744 (ite $x713 $x727 (ite $x710 $x727 (ite $x708 $x723 (ite $x705 $x724 (ite $x703 $x724 $x727)))))))
 (let ((?x551 (top ?x333)))
 (let ((?x690 (stack ?x551 ?x367)))
 (let (($x719 (= ?x505 ?x690)))
 (let (($x714 (= c_____ left-to-center)))
 (let (($x715 (and $x714 $x711)))
 (ite $x715 $x719 $x744)))))))))))))))))))))))))))))))
(assert
 (let ((?x553 (center s_____)))
 (let ((?x691 (center s______)))
 (let (($x729 (= ?x691 ?x553)))
 (let ((?x367 (right s_____)))
 (let ((?x698 (top ?x367)))
 (let ((?x700 (stack ?x698 ?x553)))
 (let (($x726 (= ?x691 ?x700)))
 (let (($x701 ((_ is stack ) ?x367)))
 (let (($x702 (= c_____ right-to-center)))
 (let (($x703 (and $x702 $x701)))
 (let (($x704 (= c_____ right-to-left)))
 (let (($x705 (and $x704 $x701)))
 (let ((?x693 (rest ?x553)))
 (let (($x721 (= ?x691 ?x693)))
 (let (($x706 ((_ is stack ) ?x553)))
 (let (($x707 (= c_____ center-to-right)))
 (let (($x708 (and $x707 $x706)))
 (let (($x709 (= c_____ center-to-left)))
 (let (($x710 (and $x709 $x706)))
 (let ((?x333 (left s_____)))
 (let ((?x551 (top ?x333)))
 (let ((?x692 (stack ?x551 ?x553)))
 (let (($x720 (= ?x691 ?x692)))
 (let (($x711 ((_ is stack ) ?x333)))
 (let (($x712 (= c_____ left-to-right)))
 (let (($x713 (and $x712 $x711)))
 (let (($x743 (ite $x713 $x720 (ite $x710 $x721 (ite $x708 $x721 (ite $x705 $x729 (ite $x703 $x726 $x729)))))))
 (let (($x714 (= c_____ left-to-center)))
 (let (($x715 (and $x714 $x711)))
 (ite $x715 $x729 $x743)))))))))))))))))))))))))))))))
(assert
 (= c_______ c!6))
(assert
 (let ((?x471 (left s______)))
 (let ((?x609 (left s_______)))
 (let (($x869 (= ?x609 ?x471)))
 (let ((?x505 (right s______)))
 (let ((?x836 (top ?x505)))
 (let ((?x837 (stack ?x836 ?x471)))
 (let (($x863 (= ?x609 ?x837)))
 (let (($x839 ((_ is stack ) ?x505)))
 (let (($x842 (= c______ right-to-left)))
 (let (($x843 (and $x842 $x839)))
 (let ((?x691 (center s______)))
 (let (($x844 ((_ is stack ) ?x691)))
 (let (($x845 (= c______ center-to-right)))
 (let (($x846 (and $x845 $x844)))
 (let ((?x832 (top ?x691)))
 (let ((?x833 (stack ?x832 ?x471)))
 (let (($x860 (= ?x609 ?x833)))
 (let (($x847 (= c______ center-to-left)))
 (let (($x848 (and $x847 $x844)))
 (let ((?x642 (rest ?x471)))
 (let (($x856 (= ?x609 ?x642)))
 (let (($x849 ((_ is stack ) ?x471)))
 (let (($x850 (= c______ left-to-right)))
 (let (($x851 (and $x850 $x849)))
 (let (($x852 (= c______ left-to-center)))
 (let (($x853 (and $x852 $x849)))
 (ite $x853 $x856 (ite $x851 $x856 (ite $x848 $x860 (ite $x846 $x869 (ite $x843 $x863 $x869))))))))))))))))))))))))))))))))
(assert
 (let ((?x505 (right s______)))
 (let ((?x643 (right s_______)))
 (let (($x865 (= ?x643 ?x505)))
 (let ((?x835 (rest ?x505)))
 (let (($x862 (= ?x643 ?x835)))
 (let (($x839 ((_ is stack ) ?x505)))
 (let (($x840 (= c______ right-to-center)))
 (let (($x841 (and $x840 $x839)))
 (let (($x842 (= c______ right-to-left)))
 (let (($x843 (and $x842 $x839)))
 (let ((?x691 (center s______)))
 (let ((?x832 (top ?x691)))
 (let ((?x834 (stack ?x832 ?x505)))
 (let (($x861 (= ?x643 ?x834)))
 (let (($x844 ((_ is stack ) ?x691)))
 (let (($x845 (= c______ center-to-right)))
 (let (($x846 (and $x845 $x844)))
 (let (($x847 (= c______ center-to-left)))
 (let (($x848 (and $x847 $x844)))
 (let ((?x471 (left s______)))
 (let (($x849 ((_ is stack ) ?x471)))
 (let (($x850 (= c______ left-to-right)))
 (let (($x851 (and $x850 $x849)))
 (let (($x882 (ite $x851 $x865 (ite $x848 $x865 (ite $x846 $x861 (ite $x843 $x862 (ite $x841 $x862 $x865)))))))
 (let ((?x689 (top ?x471)))
 (let ((?x828 (stack ?x689 ?x505)))
 (let (($x857 (= ?x643 ?x828)))
 (let (($x852 (= c______ left-to-center)))
 (let (($x853 (and $x852 $x849)))
 (ite $x853 $x857 $x882)))))))))))))))))))))))))))))))
(assert
 (let ((?x691 (center s______)))
 (let ((?x829 (center s_______)))
 (let (($x867 (= ?x829 ?x691)))
 (let ((?x505 (right s______)))
 (let ((?x836 (top ?x505)))
 (let ((?x838 (stack ?x836 ?x691)))
 (let (($x864 (= ?x829 ?x838)))
 (let (($x839 ((_ is stack ) ?x505)))
 (let (($x840 (= c______ right-to-center)))
 (let (($x841 (and $x840 $x839)))
 (let (($x842 (= c______ right-to-left)))
 (let (($x843 (and $x842 $x839)))
 (let ((?x831 (rest ?x691)))
 (let (($x859 (= ?x829 ?x831)))
 (let (($x844 ((_ is stack ) ?x691)))
 (let (($x845 (= c______ center-to-right)))
 (let (($x846 (and $x845 $x844)))
 (let (($x847 (= c______ center-to-left)))
 (let (($x848 (and $x847 $x844)))
 (let ((?x471 (left s______)))
 (let ((?x689 (top ?x471)))
 (let ((?x830 (stack ?x689 ?x691)))
 (let (($x858 (= ?x829 ?x830)))
 (let (($x849 ((_ is stack ) ?x471)))
 (let (($x850 (= c______ left-to-right)))
 (let (($x851 (and $x850 $x849)))
 (let (($x881 (ite $x851 $x858 (ite $x848 $x859 (ite $x846 $x859 (ite $x843 $x867 (ite $x841 $x864 $x867)))))))
 (let (($x852 (= c______ left-to-center)))
 (let (($x853 (and $x852 $x849)))
 (ite $x853 $x867 $x881)))))))))))))))))))))))))))))))
(assert
 (= c________ c!7))
(assert
 (let ((?x609 (left s_______)))
 (let ((?x747 (left s________)))
 (let (($x1007 (= ?x747 ?x609)))
 (let ((?x643 (right s_______)))
 (let ((?x974 (top ?x643)))
 (let ((?x975 (stack ?x974 ?x609)))
 (let (($x1001 (= ?x747 ?x975)))
 (let (($x977 ((_ is stack ) ?x643)))
 (let (($x980 (= c_______ right-to-left)))
 (let (($x981 (and $x980 $x977)))
 (let ((?x829 (center s_______)))
 (let (($x982 ((_ is stack ) ?x829)))
 (let (($x983 (= c_______ center-to-right)))
 (let (($x984 (and $x983 $x982)))
 (let ((?x970 (top ?x829)))
 (let ((?x971 (stack ?x970 ?x609)))
 (let (($x998 (= ?x747 ?x971)))
 (let (($x985 (= c_______ center-to-left)))
 (let (($x986 (and $x985 $x982)))
 (let ((?x780 (rest ?x609)))
 (let (($x994 (= ?x747 ?x780)))
 (let (($x987 ((_ is stack ) ?x609)))
 (let (($x988 (= c_______ left-to-right)))
 (let (($x989 (and $x988 $x987)))
 (let (($x990 (= c_______ left-to-center)))
 (let (($x991 (and $x990 $x987)))
 (ite $x991 $x994 (ite $x989 $x994 (ite $x986 $x998 (ite $x984 $x1007 (ite $x981 $x1001 $x1007))))))))))))))))))))))))))))))))
(assert
 (let ((?x643 (right s_______)))
 (let ((?x781 (right s________)))
 (let (($x1003 (= ?x781 ?x643)))
 (let ((?x973 (rest ?x643)))
 (let (($x1000 (= ?x781 ?x973)))
 (let (($x977 ((_ is stack ) ?x643)))
 (let (($x978 (= c_______ right-to-center)))
 (let (($x979 (and $x978 $x977)))
 (let (($x980 (= c_______ right-to-left)))
 (let (($x981 (and $x980 $x977)))
 (let ((?x829 (center s_______)))
 (let ((?x970 (top ?x829)))
 (let ((?x972 (stack ?x970 ?x643)))
 (let (($x999 (= ?x781 ?x972)))
 (let (($x982 ((_ is stack ) ?x829)))
 (let (($x983 (= c_______ center-to-right)))
 (let (($x984 (and $x983 $x982)))
 (let (($x985 (= c_______ center-to-left)))
 (let (($x986 (and $x985 $x982)))
 (let ((?x609 (left s_______)))
 (let (($x987 ((_ is stack ) ?x609)))
 (let (($x988 (= c_______ left-to-right)))
 (let (($x989 (and $x988 $x987)))
 (let (($x1020 (ite $x989 $x1003 (ite $x986 $x1003 (ite $x984 $x999 (ite $x981 $x1000 (ite $x979 $x1000 $x1003)))))))
 (let ((?x827 (top ?x609)))
 (let ((?x966 (stack ?x827 ?x643)))
 (let (($x995 (= ?x781 ?x966)))
 (let (($x990 (= c_______ left-to-center)))
 (let (($x991 (and $x990 $x987)))
 (ite $x991 $x995 $x1020)))))))))))))))))))))))))))))))
(assert
 (let ((?x829 (center s_______)))
 (let ((?x967 (center s________)))
 (let (($x1005 (= ?x967 ?x829)))
 (let ((?x643 (right s_______)))
 (let ((?x974 (top ?x643)))
 (let ((?x976 (stack ?x974 ?x829)))
 (let (($x1002 (= ?x967 ?x976)))
 (let (($x977 ((_ is stack ) ?x643)))
 (let (($x978 (= c_______ right-to-center)))
 (let (($x979 (and $x978 $x977)))
 (let (($x980 (= c_______ right-to-left)))
 (let (($x981 (and $x980 $x977)))
 (let ((?x969 (rest ?x829)))
 (let (($x997 (= ?x967 ?x969)))
 (let (($x982 ((_ is stack ) ?x829)))
 (let (($x983 (= c_______ center-to-right)))
 (let (($x984 (and $x983 $x982)))
 (let (($x985 (= c_______ center-to-left)))
 (let (($x986 (and $x985 $x982)))
 (let ((?x609 (left s_______)))
 (let ((?x827 (top ?x609)))
 (let ((?x968 (stack ?x827 ?x829)))
 (let (($x996 (= ?x967 ?x968)))
 (let (($x987 ((_ is stack ) ?x609)))
 (let (($x988 (= c_______ left-to-right)))
 (let (($x989 (and $x988 $x987)))
 (let (($x1019 (ite $x989 $x996 (ite $x986 $x997 (ite $x984 $x997 (ite $x981 $x1005 (ite $x979 $x1002 $x1005)))))))
 (let (($x990 (= c_______ left-to-center)))
 (let (($x991 (and $x990 $x987)))
 (ite $x991 $x1005 $x1019)))))))))))))))))))))))))))))))
(assert
 (= c_________ c!8))
(assert
 (let ((?x747 (left s________)))
 (let ((?x885 (left s_________)))
 (let (($x1145 (= ?x885 ?x747)))
 (let ((?x781 (right s________)))
 (let ((?x1112 (top ?x781)))
 (let ((?x1113 (stack ?x1112 ?x747)))
 (let (($x1139 (= ?x885 ?x1113)))
 (let (($x1115 ((_ is stack ) ?x781)))
 (let (($x1118 (= c________ right-to-left)))
 (let (($x1119 (and $x1118 $x1115)))
 (let ((?x967 (center s________)))
 (let (($x1120 ((_ is stack ) ?x967)))
 (let (($x1121 (= c________ center-to-right)))
 (let (($x1122 (and $x1121 $x1120)))
 (let ((?x1108 (top ?x967)))
 (let ((?x1109 (stack ?x1108 ?x747)))
 (let (($x1136 (= ?x885 ?x1109)))
 (let (($x1123 (= c________ center-to-left)))
 (let (($x1124 (and $x1123 $x1120)))
 (let ((?x918 (rest ?x747)))
 (let (($x1132 (= ?x885 ?x918)))
 (let (($x1125 ((_ is stack ) ?x747)))
 (let (($x1126 (= c________ left-to-right)))
 (let (($x1127 (and $x1126 $x1125)))
 (let (($x1128 (= c________ left-to-center)))
 (let (($x1129 (and $x1128 $x1125)))
 (ite $x1129 $x1132 (ite $x1127 $x1132 (ite $x1124 $x1136 (ite $x1122 $x1145 (ite $x1119 $x1139 $x1145))))))))))))))))))))))))))))))))
(assert
 (let ((?x781 (right s________)))
 (let ((?x919 (right s_________)))
 (let (($x1141 (= ?x919 ?x781)))
 (let ((?x1111 (rest ?x781)))
 (let (($x1138 (= ?x919 ?x1111)))
 (let (($x1115 ((_ is stack ) ?x781)))
 (let (($x1116 (= c________ right-to-center)))
 (let (($x1117 (and $x1116 $x1115)))
 (let (($x1118 (= c________ right-to-left)))
 (let (($x1119 (and $x1118 $x1115)))
 (let ((?x967 (center s________)))
 (let ((?x1108 (top ?x967)))
 (let ((?x1110 (stack ?x1108 ?x781)))
 (let (($x1137 (= ?x919 ?x1110)))
 (let (($x1120 ((_ is stack ) ?x967)))
 (let (($x1121 (= c________ center-to-right)))
 (let (($x1122 (and $x1121 $x1120)))
 (let (($x1123 (= c________ center-to-left)))
 (let (($x1124 (and $x1123 $x1120)))
 (let ((?x747 (left s________)))
 (let (($x1125 ((_ is stack ) ?x747)))
 (let (($x1126 (= c________ left-to-right)))
 (let (($x1127 (and $x1126 $x1125)))
 (let (($x1158 (ite $x1127 $x1141 (ite $x1124 $x1141 (ite $x1122 $x1137 (ite $x1119 $x1138 (ite $x1117 $x1138 $x1141)))))))
 (let ((?x965 (top ?x747)))
 (let ((?x1104 (stack ?x965 ?x781)))
 (let (($x1133 (= ?x919 ?x1104)))
 (let (($x1128 (= c________ left-to-center)))
 (let (($x1129 (and $x1128 $x1125)))
 (ite $x1129 $x1133 $x1158)))))))))))))))))))))))))))))))
(assert
 (let ((?x967 (center s________)))
 (let ((?x1105 (center s_________)))
 (let (($x1143 (= ?x1105 ?x967)))
 (let ((?x781 (right s________)))
 (let ((?x1112 (top ?x781)))
 (let ((?x1114 (stack ?x1112 ?x967)))
 (let (($x1140 (= ?x1105 ?x1114)))
 (let (($x1115 ((_ is stack ) ?x781)))
 (let (($x1116 (= c________ right-to-center)))
 (let (($x1117 (and $x1116 $x1115)))
 (let (($x1118 (= c________ right-to-left)))
 (let (($x1119 (and $x1118 $x1115)))
 (let ((?x1107 (rest ?x967)))
 (let (($x1135 (= ?x1105 ?x1107)))
 (let (($x1120 ((_ is stack ) ?x967)))
 (let (($x1121 (= c________ center-to-right)))
 (let (($x1122 (and $x1121 $x1120)))
 (let (($x1123 (= c________ center-to-left)))
 (let (($x1124 (and $x1123 $x1120)))
 (let ((?x747 (left s________)))
 (let ((?x965 (top ?x747)))
 (let ((?x1106 (stack ?x965 ?x967)))
 (let (($x1134 (= ?x1105 ?x1106)))
 (let (($x1125 ((_ is stack ) ?x747)))
 (let (($x1126 (= c________ left-to-right)))
 (let (($x1127 (and $x1126 $x1125)))
 (let (($x1157 (ite $x1127 $x1134 (ite $x1124 $x1135 (ite $x1122 $x1135 (ite $x1119 $x1143 (ite $x1117 $x1140 $x1143)))))))
 (let (($x1128 (= c________ left-to-center)))
 (let (($x1129 (and $x1128 $x1125)))
 (ite $x1129 $x1143 $x1157)))))))))))))))))))))))))))))))
(assert
 (= c__________ c!9))
(assert
 (let ((?x885 (left s_________)))
 (let ((?x1023 (left s__________)))
 (let (($x1283 (= ?x1023 ?x885)))
 (let ((?x919 (right s_________)))
 (let ((?x1250 (top ?x919)))
 (let ((?x1251 (stack ?x1250 ?x885)))
 (let (($x1277 (= ?x1023 ?x1251)))
 (let (($x1253 ((_ is stack ) ?x919)))
 (let (($x1256 (= c_________ right-to-left)))
 (let (($x1257 (and $x1256 $x1253)))
 (let ((?x1105 (center s_________)))
 (let (($x1258 ((_ is stack ) ?x1105)))
 (let (($x1259 (= c_________ center-to-right)))
 (let (($x1260 (and $x1259 $x1258)))
 (let ((?x1246 (top ?x1105)))
 (let ((?x1247 (stack ?x1246 ?x885)))
 (let (($x1274 (= ?x1023 ?x1247)))
 (let (($x1261 (= c_________ center-to-left)))
 (let (($x1262 (and $x1261 $x1258)))
 (let ((?x1056 (rest ?x885)))
 (let (($x1270 (= ?x1023 ?x1056)))
 (let (($x1263 ((_ is stack ) ?x885)))
 (let (($x1264 (= c_________ left-to-right)))
 (let (($x1265 (and $x1264 $x1263)))
 (let (($x1266 (= c_________ left-to-center)))
 (let (($x1267 (and $x1266 $x1263)))
 (ite $x1267 $x1270 (ite $x1265 $x1270 (ite $x1262 $x1274 (ite $x1260 $x1283 (ite $x1257 $x1277 $x1283))))))))))))))))))))))))))))))))
(assert
 (let ((?x919 (right s_________)))
 (let ((?x1057 (right s__________)))
 (let (($x1279 (= ?x1057 ?x919)))
 (let ((?x1249 (rest ?x919)))
 (let (($x1276 (= ?x1057 ?x1249)))
 (let (($x1253 ((_ is stack ) ?x919)))
 (let (($x1254 (= c_________ right-to-center)))
 (let (($x1255 (and $x1254 $x1253)))
 (let (($x1256 (= c_________ right-to-left)))
 (let (($x1257 (and $x1256 $x1253)))
 (let ((?x1105 (center s_________)))
 (let ((?x1246 (top ?x1105)))
 (let ((?x1248 (stack ?x1246 ?x919)))
 (let (($x1275 (= ?x1057 ?x1248)))
 (let (($x1258 ((_ is stack ) ?x1105)))
 (let (($x1259 (= c_________ center-to-right)))
 (let (($x1260 (and $x1259 $x1258)))
 (let (($x1261 (= c_________ center-to-left)))
 (let (($x1262 (and $x1261 $x1258)))
 (let ((?x885 (left s_________)))
 (let (($x1263 ((_ is stack ) ?x885)))
 (let (($x1264 (= c_________ left-to-right)))
 (let (($x1265 (and $x1264 $x1263)))
 (let (($x1296 (ite $x1265 $x1279 (ite $x1262 $x1279 (ite $x1260 $x1275 (ite $x1257 $x1276 (ite $x1255 $x1276 $x1279)))))))
 (let ((?x1103 (top ?x885)))
 (let ((?x1242 (stack ?x1103 ?x919)))
 (let (($x1271 (= ?x1057 ?x1242)))
 (let (($x1266 (= c_________ left-to-center)))
 (let (($x1267 (and $x1266 $x1263)))
 (ite $x1267 $x1271 $x1296)))))))))))))))))))))))))))))))
(assert
 (let ((?x1105 (center s_________)))
 (let ((?x1243 (center s__________)))
 (let (($x1281 (= ?x1243 ?x1105)))
 (let ((?x919 (right s_________)))
 (let ((?x1250 (top ?x919)))
 (let ((?x1252 (stack ?x1250 ?x1105)))
 (let (($x1278 (= ?x1243 ?x1252)))
 (let (($x1253 ((_ is stack ) ?x919)))
 (let (($x1254 (= c_________ right-to-center)))
 (let (($x1255 (and $x1254 $x1253)))
 (let (($x1256 (= c_________ right-to-left)))
 (let (($x1257 (and $x1256 $x1253)))
 (let ((?x1245 (rest ?x1105)))
 (let (($x1273 (= ?x1243 ?x1245)))
 (let (($x1258 ((_ is stack ) ?x1105)))
 (let (($x1259 (= c_________ center-to-right)))
 (let (($x1260 (and $x1259 $x1258)))
 (let (($x1261 (= c_________ center-to-left)))
 (let (($x1262 (and $x1261 $x1258)))
 (let ((?x885 (left s_________)))
 (let ((?x1103 (top ?x885)))
 (let ((?x1244 (stack ?x1103 ?x1105)))
 (let (($x1272 (= ?x1243 ?x1244)))
 (let (($x1263 ((_ is stack ) ?x885)))
 (let (($x1264 (= c_________ left-to-right)))
 (let (($x1265 (and $x1264 $x1263)))
 (let (($x1295 (ite $x1265 $x1272 (ite $x1262 $x1273 (ite $x1260 $x1273 (ite $x1257 $x1281 (ite $x1255 $x1278 $x1281)))))))
 (let (($x1266 (= c_________ left-to-center)))
 (let (($x1267 (and $x1266 $x1263)))
 (ite $x1267 $x1281 $x1295)))))))))))))))))))))))))))))))
(assert
 (let ((?x29 (stack C empty)))
 (let ((?x1057 (right s__________)))
 (let (($x1299 (= ?x1057 ?x29)))
 (let ((?x1243 (center s__________)))
 (let (($x1330 (= ?x1243 empty)))
 (let ((?x28 (stack D (stack A (stack B (stack E (stack F empty)))))))
 (let ((?x1023 (left s__________)))
 (let (($x1331 (= ?x1023 ?x28)))
 (let (($x1333 (not (and $x1331 $x1330 $x1299))))
 (not $x1333)))))))))))
(check-sat)
