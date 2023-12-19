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
(assert
 (= s_tmp s))
(assert
 (= c_tmp c))
(assert
 (let ((?x19 (stack B empty)))
 (let ((?x18 (stack E ?x19)))
 (let ((?x75 (left s_tmp_)))
 (= ?x75 ?x18)))))
(assert
 (let ((?x20 (stack C empty)))
 (let ((?x21 (stack A ?x20)))
 (let ((?x22 (stack D ?x21)))
 (let ((?x23 (stack F ?x22)))
 (let ((?x78 (center s_tmp__)))
 (= ?x78 ?x23)))))))
(assert
 (let ((?x81 (right s_tmp___)))
 (= ?x81 empty)))
(assert
 (= c_tmp____ c!0))
(assert
 (= s_ s_tmp___))
(assert
 (= c_ c_tmp____))
(assert
 (let ((?x108 (left s_tmp__)))
 (= (left s_tmp___) ?x108)))
(assert
 (let ((?x78 (center s_tmp__)))
 (= (center s_tmp___) ?x78)))
(assert
 (let ((?x112 (center s_tmp_)))
 (= ?x112 (center s_tmp))))
(assert
 (let ((?x115 (right s_tmp_)))
 (= ?x115 (right s_tmp))))
(assert
 (let ((?x75 (left s_tmp_)))
 (let ((?x108 (left s_tmp__)))
 (= ?x108 ?x75))))
(assert
 (let ((?x115 (right s_tmp_)))
 (let ((?x119 (right s_tmp__)))
 (= ?x119 ?x115))))
(assert
 (= c__ c!1))
(assert
 (let ((?x52 (left s_)))
 (let ((?x184 (left s__)))
 (let (($x226 (= ?x184 ?x52)))
 (let ((?x55 (right s_)))
 (let (($x196 ((_ is stack ) ?x55)))
 (let (($x200 (and (= c_ right-to-left) $x196)))
 (let ((?x57 (center s_)))
 (let (($x201 ((_ is stack ) ?x57)))
 (let (($x202 (= c_ center-to-right)))
 (let (($x203 (and $x202 $x201)))
 (let (($x204 (= c_ center-to-left)))
 (let (($x205 (and $x204 $x201)))
 (let (($x235 (ite $x205 (= ?x184 (stack (top ?x57) ?x52)) (ite $x203 $x226 (ite $x200 (= ?x184 (stack (top ?x55) ?x52)) $x226)))))
 (let (($x213 (= ?x184 (rest ?x52))))
 (let (($x206 ((_ is stack ) ?x52)))
 (let (($x207 (= c_ left-to-right)))
 (let (($x208 (and $x207 $x206)))
 (let (($x209 (= c_ left-to-center)))
 (let (($x210 (and $x209 $x206)))
 (ite $x210 $x213 (ite $x208 $x213 $x235))))))))))))))))))))))
(assert
 (let ((?x55 (right s_)))
 (let ((?x186 (right s__)))
 (let (($x222 (= ?x186 ?x55)))
 (let (($x219 (= ?x186 (rest ?x55))))
 (let (($x196 ((_ is stack ) ?x55)))
 (let (($x198 (and (= c_ right-to-center) $x196)))
 (let (($x200 (and (= c_ right-to-left) $x196)))
 (let ((?x57 (center s_)))
 (let (($x201 ((_ is stack ) ?x57)))
 (let (($x202 (= c_ center-to-right)))
 (let (($x203 (and $x202 $x201)))
 (let (($x232 (ite $x203 (= ?x186 (stack (top ?x57) ?x55)) (ite $x200 $x219 (ite $x198 $x219 $x222)))))
 (let (($x204 (= c_ center-to-left)))
 (let (($x205 (and $x204 $x201)))
 (let ((?x52 (left s_)))
 (let (($x206 ((_ is stack ) ?x52)))
 (let (($x207 (= c_ left-to-right)))
 (let (($x208 (and $x207 $x206)))
 (let (($x209 (= c_ left-to-center)))
 (let (($x210 (and $x209 $x206)))
 (ite $x210 (= ?x186 (stack (top ?x52) ?x55)) (ite $x208 $x222 (ite $x205 $x222 $x232))))))))))))))))))))))))
(assert
 (let ((?x57 (center s_)))
 (let ((?x180 (center s__)))
 (let (($x224 (= ?x180 ?x57)))
 (let ((?x55 (right s_)))
 (let (($x196 ((_ is stack ) ?x55)))
 (let (($x198 (and (= c_ right-to-center) $x196)))
 (let (($x200 (and (= c_ right-to-left) $x196)))
 (let (($x216 (= ?x180 (rest ?x57))))
 (let (($x201 ((_ is stack ) ?x57)))
 (let (($x202 (= c_ center-to-right)))
 (let (($x203 (and $x202 $x201)))
 (let (($x231 (ite $x203 $x216 (ite $x200 $x224 (ite $x198 (= ?x180 (stack (top ?x55) ?x57)) $x224)))))
 (let (($x204 (= c_ center-to-left)))
 (let (($x205 (and $x204 $x201)))
 (let ((?x52 (left s_)))
 (let (($x206 ((_ is stack ) ?x52)))
 (let (($x207 (= c_ left-to-right)))
 (let (($x208 (and $x207 $x206)))
 (let (($x209 (= c_ left-to-center)))
 (let (($x210 (and $x209 $x206)))
 (ite $x210 $x224 (ite $x208 (= ?x180 (stack (top ?x52) ?x57)) (ite $x205 $x216 $x231))))))))))))))))))))))))
(assert
 (= c___ c!2))
(assert
 (let ((?x184 (left s__)))
 (let ((?x122 (left s___)))
 (let (($x337 (= ?x122 ?x184)))
 (let ((?x186 (right s__)))
 (let ((?x304 (top ?x186)))
 (let ((?x305 (stack ?x304 ?x184)))
 (let (($x331 (= ?x122 ?x305)))
 (let (($x307 ((_ is stack ) ?x186)))
 (let (($x310 (= c__ right-to-left)))
 (let (($x311 (and $x310 $x307)))
 (let ((?x180 (center s__)))
 (let (($x312 ((_ is stack ) ?x180)))
 (let (($x313 (= c__ center-to-right)))
 (let (($x314 (and $x313 $x312)))
 (let ((?x300 (top ?x180)))
 (let ((?x301 (stack ?x300 ?x184)))
 (let (($x328 (= ?x122 ?x301)))
 (let (($x315 (= c__ center-to-left)))
 (let (($x316 (and $x315 $x312)))
 (let ((?x124 (rest ?x184)))
 (let (($x324 (= ?x122 ?x124)))
 (let (($x317 ((_ is stack ) ?x184)))
 (let (($x318 (= c__ left-to-right)))
 (let (($x319 (and $x318 $x317)))
 (let (($x320 (= c__ left-to-center)))
 (let (($x321 (and $x320 $x317)))
 (ite $x321 $x324 (ite $x319 $x324 (ite $x316 $x328 (ite $x314 $x337 (ite $x311 $x331 $x337))))))))))))))))))))))))))))))))
(assert
 (let ((?x186 (right s__)))
 (let ((?x125 (right s___)))
 (let (($x333 (= ?x125 ?x186)))
 (let ((?x303 (rest ?x186)))
 (let (($x330 (= ?x125 ?x303)))
 (let (($x307 ((_ is stack ) ?x186)))
 (let (($x308 (= c__ right-to-center)))
 (let (($x309 (and $x308 $x307)))
 (let (($x310 (= c__ right-to-left)))
 (let (($x311 (and $x310 $x307)))
 (let ((?x180 (center s__)))
 (let ((?x300 (top ?x180)))
 (let ((?x302 (stack ?x300 ?x186)))
 (let (($x329 (= ?x125 ?x302)))
 (let (($x312 ((_ is stack ) ?x180)))
 (let (($x313 (= c__ center-to-right)))
 (let (($x314 (and $x313 $x312)))
 (let (($x315 (= c__ center-to-left)))
 (let (($x316 (and $x315 $x312)))
 (let ((?x184 (left s__)))
 (let (($x317 ((_ is stack ) ?x184)))
 (let (($x318 (= c__ left-to-right)))
 (let (($x319 (and $x318 $x317)))
 (let (($x350 (ite $x319 $x333 (ite $x316 $x333 (ite $x314 $x329 (ite $x311 $x330 (ite $x309 $x330 $x333)))))))
 (let ((?x178 (top ?x184)))
 (let ((?x296 (stack ?x178 ?x186)))
 (let (($x325 (= ?x125 ?x296)))
 (let (($x320 (= c__ left-to-center)))
 (let (($x321 (and $x320 $x317)))
 (ite $x321 $x325 $x350)))))))))))))))))))))))))))))))
(assert
 (let ((?x180 (center s__)))
 (let ((?x297 (center s___)))
 (let (($x335 (= ?x297 ?x180)))
 (let ((?x186 (right s__)))
 (let ((?x304 (top ?x186)))
 (let ((?x306 (stack ?x304 ?x180)))
 (let (($x332 (= ?x297 ?x306)))
 (let (($x307 ((_ is stack ) ?x186)))
 (let (($x308 (= c__ right-to-center)))
 (let (($x309 (and $x308 $x307)))
 (let (($x310 (= c__ right-to-left)))
 (let (($x311 (and $x310 $x307)))
 (let ((?x299 (rest ?x180)))
 (let (($x327 (= ?x297 ?x299)))
 (let (($x312 ((_ is stack ) ?x180)))
 (let (($x313 (= c__ center-to-right)))
 (let (($x314 (and $x313 $x312)))
 (let (($x315 (= c__ center-to-left)))
 (let (($x316 (and $x315 $x312)))
 (let ((?x184 (left s__)))
 (let ((?x178 (top ?x184)))
 (let ((?x298 (stack ?x178 ?x180)))
 (let (($x326 (= ?x297 ?x298)))
 (let (($x317 ((_ is stack ) ?x184)))
 (let (($x318 (= c__ left-to-right)))
 (let (($x319 (and $x318 $x317)))
 (let (($x349 (ite $x319 $x326 (ite $x316 $x327 (ite $x314 $x327 (ite $x311 $x335 (ite $x309 $x332 $x335)))))))
 (let (($x320 (= c__ left-to-center)))
 (let (($x321 (and $x320 $x317)))
 (ite $x321 $x335 $x349)))))))))))))))))))))))))))))))
(assert
 (= c____ c!3))
(assert
 (let ((?x122 (left s___)))
 (let ((?x242 (left s____)))
 (let (($x475 (= ?x242 ?x122)))
 (let ((?x125 (right s___)))
 (let ((?x442 (top ?x125)))
 (let ((?x443 (stack ?x442 ?x122)))
 (let (($x469 (= ?x242 ?x443)))
 (let (($x445 ((_ is stack ) ?x125)))
 (let (($x448 (= c___ right-to-left)))
 (let (($x449 (and $x448 $x445)))
 (let ((?x297 (center s___)))
 (let (($x450 ((_ is stack ) ?x297)))
 (let (($x451 (= c___ center-to-right)))
 (let (($x452 (and $x451 $x450)))
 (let ((?x438 (top ?x297)))
 (let ((?x439 (stack ?x438 ?x122)))
 (let (($x466 (= ?x242 ?x439)))
 (let (($x453 (= c___ center-to-left)))
 (let (($x454 (and $x453 $x450)))
 (let ((?x273 (rest ?x122)))
 (let (($x462 (= ?x242 ?x273)))
 (let (($x455 ((_ is stack ) ?x122)))
 (let (($x456 (= c___ left-to-right)))
 (let (($x457 (and $x456 $x455)))
 (let (($x458 (= c___ left-to-center)))
 (let (($x459 (and $x458 $x455)))
 (ite $x459 $x462 (ite $x457 $x462 (ite $x454 $x466 (ite $x452 $x475 (ite $x449 $x469 $x475))))))))))))))))))))))))))))))))
(assert
 (let ((?x125 (right s___)))
 (let ((?x274 (right s____)))
 (let (($x471 (= ?x274 ?x125)))
 (let ((?x441 (rest ?x125)))
 (let (($x468 (= ?x274 ?x441)))
 (let (($x445 ((_ is stack ) ?x125)))
 (let (($x446 (= c___ right-to-center)))
 (let (($x447 (and $x446 $x445)))
 (let (($x448 (= c___ right-to-left)))
 (let (($x449 (and $x448 $x445)))
 (let ((?x297 (center s___)))
 (let ((?x438 (top ?x297)))
 (let ((?x440 (stack ?x438 ?x125)))
 (let (($x467 (= ?x274 ?x440)))
 (let (($x450 ((_ is stack ) ?x297)))
 (let (($x451 (= c___ center-to-right)))
 (let (($x452 (and $x451 $x450)))
 (let (($x453 (= c___ center-to-left)))
 (let (($x454 (and $x453 $x450)))
 (let ((?x122 (left s___)))
 (let (($x455 ((_ is stack ) ?x122)))
 (let (($x456 (= c___ left-to-right)))
 (let (($x457 (and $x456 $x455)))
 (let (($x488 (ite $x457 $x471 (ite $x454 $x471 (ite $x452 $x467 (ite $x449 $x468 (ite $x447 $x468 $x471)))))))
 (let ((?x295 (top ?x122)))
 (let ((?x434 (stack ?x295 ?x125)))
 (let (($x463 (= ?x274 ?x434)))
 (let (($x458 (= c___ left-to-center)))
 (let (($x459 (and $x458 $x455)))
 (ite $x459 $x463 $x488)))))))))))))))))))))))))))))))
(assert
 (let ((?x297 (center s___)))
 (let ((?x435 (center s____)))
 (let (($x473 (= ?x435 ?x297)))
 (let ((?x125 (right s___)))
 (let ((?x442 (top ?x125)))
 (let ((?x444 (stack ?x442 ?x297)))
 (let (($x470 (= ?x435 ?x444)))
 (let (($x445 ((_ is stack ) ?x125)))
 (let (($x446 (= c___ right-to-center)))
 (let (($x447 (and $x446 $x445)))
 (let (($x448 (= c___ right-to-left)))
 (let (($x449 (and $x448 $x445)))
 (let ((?x437 (rest ?x297)))
 (let (($x465 (= ?x435 ?x437)))
 (let (($x450 ((_ is stack ) ?x297)))
 (let (($x451 (= c___ center-to-right)))
 (let (($x452 (and $x451 $x450)))
 (let (($x453 (= c___ center-to-left)))
 (let (($x454 (and $x453 $x450)))
 (let ((?x122 (left s___)))
 (let ((?x295 (top ?x122)))
 (let ((?x436 (stack ?x295 ?x297)))
 (let (($x464 (= ?x435 ?x436)))
 (let (($x455 ((_ is stack ) ?x122)))
 (let (($x456 (= c___ left-to-right)))
 (let (($x457 (and $x456 $x455)))
 (let (($x487 (ite $x457 $x464 (ite $x454 $x465 (ite $x452 $x465 (ite $x449 $x473 (ite $x447 $x470 $x473)))))))
 (let (($x458 (= c___ left-to-center)))
 (let (($x459 (and $x458 $x455)))
 (ite $x459 $x473 $x487)))))))))))))))))))))))))))))))
(assert
 (= c_____ c!4))
(assert
 (let ((?x242 (left s____)))
 (let ((?x353 (left s_____)))
 (let (($x613 (= ?x353 ?x242)))
 (let ((?x274 (right s____)))
 (let ((?x580 (top ?x274)))
 (let ((?x581 (stack ?x580 ?x242)))
 (let (($x607 (= ?x353 ?x581)))
 (let (($x583 ((_ is stack ) ?x274)))
 (let (($x586 (= c____ right-to-left)))
 (let (($x587 (and $x586 $x583)))
 (let ((?x435 (center s____)))
 (let (($x588 ((_ is stack ) ?x435)))
 (let (($x589 (= c____ center-to-right)))
 (let (($x590 (and $x589 $x588)))
 (let ((?x576 (top ?x435)))
 (let ((?x577 (stack ?x576 ?x242)))
 (let (($x604 (= ?x353 ?x577)))
 (let (($x591 (= c____ center-to-left)))
 (let (($x592 (and $x591 $x588)))
 (let ((?x386 (rest ?x242)))
 (let (($x600 (= ?x353 ?x386)))
 (let (($x593 ((_ is stack ) ?x242)))
 (let (($x594 (= c____ left-to-right)))
 (let (($x595 (and $x594 $x593)))
 (let (($x596 (= c____ left-to-center)))
 (let (($x597 (and $x596 $x593)))
 (ite $x597 $x600 (ite $x595 $x600 (ite $x592 $x604 (ite $x590 $x613 (ite $x587 $x607 $x613))))))))))))))))))))))))))))))))
(assert
 (let ((?x274 (right s____)))
 (let ((?x387 (right s_____)))
 (let (($x609 (= ?x387 ?x274)))
 (let ((?x579 (rest ?x274)))
 (let (($x606 (= ?x387 ?x579)))
 (let (($x583 ((_ is stack ) ?x274)))
 (let (($x584 (= c____ right-to-center)))
 (let (($x585 (and $x584 $x583)))
 (let (($x586 (= c____ right-to-left)))
 (let (($x587 (and $x586 $x583)))
 (let ((?x435 (center s____)))
 (let ((?x576 (top ?x435)))
 (let ((?x578 (stack ?x576 ?x274)))
 (let (($x605 (= ?x387 ?x578)))
 (let (($x588 ((_ is stack ) ?x435)))
 (let (($x589 (= c____ center-to-right)))
 (let (($x590 (and $x589 $x588)))
 (let (($x591 (= c____ center-to-left)))
 (let (($x592 (and $x591 $x588)))
 (let ((?x242 (left s____)))
 (let (($x593 ((_ is stack ) ?x242)))
 (let (($x594 (= c____ left-to-right)))
 (let (($x595 (and $x594 $x593)))
 (let (($x626 (ite $x595 $x609 (ite $x592 $x609 (ite $x590 $x605 (ite $x587 $x606 (ite $x585 $x606 $x609)))))))
 (let ((?x433 (top ?x242)))
 (let ((?x572 (stack ?x433 ?x274)))
 (let (($x601 (= ?x387 ?x572)))
 (let (($x596 (= c____ left-to-center)))
 (let (($x597 (and $x596 $x593)))
 (ite $x597 $x601 $x626)))))))))))))))))))))))))))))))
(assert
 (let ((?x435 (center s____)))
 (let ((?x573 (center s_____)))
 (let (($x611 (= ?x573 ?x435)))
 (let ((?x274 (right s____)))
 (let ((?x580 (top ?x274)))
 (let ((?x582 (stack ?x580 ?x435)))
 (let (($x608 (= ?x573 ?x582)))
 (let (($x583 ((_ is stack ) ?x274)))
 (let (($x584 (= c____ right-to-center)))
 (let (($x585 (and $x584 $x583)))
 (let (($x586 (= c____ right-to-left)))
 (let (($x587 (and $x586 $x583)))
 (let ((?x575 (rest ?x435)))
 (let (($x603 (= ?x573 ?x575)))
 (let (($x588 ((_ is stack ) ?x435)))
 (let (($x589 (= c____ center-to-right)))
 (let (($x590 (and $x589 $x588)))
 (let (($x591 (= c____ center-to-left)))
 (let (($x592 (and $x591 $x588)))
 (let ((?x242 (left s____)))
 (let ((?x433 (top ?x242)))
 (let ((?x574 (stack ?x433 ?x435)))
 (let (($x602 (= ?x573 ?x574)))
 (let (($x593 ((_ is stack ) ?x242)))
 (let (($x594 (= c____ left-to-right)))
 (let (($x595 (and $x594 $x593)))
 (let (($x625 (ite $x595 $x602 (ite $x592 $x603 (ite $x590 $x603 (ite $x587 $x611 (ite $x585 $x608 $x611)))))))
 (let (($x596 (= c____ left-to-center)))
 (let (($x597 (and $x596 $x593)))
 (ite $x597 $x611 $x625)))))))))))))))))))))))))))))))
(assert
 (= c______ c!5))
(assert
 (let ((?x353 (left s_____)))
 (let ((?x491 (left s______)))
 (let (($x751 (= ?x491 ?x353)))
 (let ((?x387 (right s_____)))
 (let ((?x718 (top ?x387)))
 (let ((?x719 (stack ?x718 ?x353)))
 (let (($x745 (= ?x491 ?x719)))
 (let (($x721 ((_ is stack ) ?x387)))
 (let (($x724 (= c_____ right-to-left)))
 (let (($x725 (and $x724 $x721)))
 (let ((?x573 (center s_____)))
 (let (($x726 ((_ is stack ) ?x573)))
 (let (($x727 (= c_____ center-to-right)))
 (let (($x728 (and $x727 $x726)))
 (let ((?x714 (top ?x573)))
 (let ((?x715 (stack ?x714 ?x353)))
 (let (($x742 (= ?x491 ?x715)))
 (let (($x729 (= c_____ center-to-left)))
 (let (($x730 (and $x729 $x726)))
 (let ((?x524 (rest ?x353)))
 (let (($x738 (= ?x491 ?x524)))
 (let (($x731 ((_ is stack ) ?x353)))
 (let (($x732 (= c_____ left-to-right)))
 (let (($x733 (and $x732 $x731)))
 (let (($x734 (= c_____ left-to-center)))
 (let (($x735 (and $x734 $x731)))
 (ite $x735 $x738 (ite $x733 $x738 (ite $x730 $x742 (ite $x728 $x751 (ite $x725 $x745 $x751))))))))))))))))))))))))))))))))
(assert
 (let ((?x387 (right s_____)))
 (let ((?x525 (right s______)))
 (let (($x747 (= ?x525 ?x387)))
 (let ((?x717 (rest ?x387)))
 (let (($x744 (= ?x525 ?x717)))
 (let (($x721 ((_ is stack ) ?x387)))
 (let (($x722 (= c_____ right-to-center)))
 (let (($x723 (and $x722 $x721)))
 (let (($x724 (= c_____ right-to-left)))
 (let (($x725 (and $x724 $x721)))
 (let ((?x573 (center s_____)))
 (let ((?x714 (top ?x573)))
 (let ((?x716 (stack ?x714 ?x387)))
 (let (($x743 (= ?x525 ?x716)))
 (let (($x726 ((_ is stack ) ?x573)))
 (let (($x727 (= c_____ center-to-right)))
 (let (($x728 (and $x727 $x726)))
 (let (($x729 (= c_____ center-to-left)))
 (let (($x730 (and $x729 $x726)))
 (let ((?x353 (left s_____)))
 (let (($x731 ((_ is stack ) ?x353)))
 (let (($x732 (= c_____ left-to-right)))
 (let (($x733 (and $x732 $x731)))
 (let (($x764 (ite $x733 $x747 (ite $x730 $x747 (ite $x728 $x743 (ite $x725 $x744 (ite $x723 $x744 $x747)))))))
 (let ((?x571 (top ?x353)))
 (let ((?x710 (stack ?x571 ?x387)))
 (let (($x739 (= ?x525 ?x710)))
 (let (($x734 (= c_____ left-to-center)))
 (let (($x735 (and $x734 $x731)))
 (ite $x735 $x739 $x764)))))))))))))))))))))))))))))))
(assert
 (let ((?x573 (center s_____)))
 (let ((?x711 (center s______)))
 (let (($x749 (= ?x711 ?x573)))
 (let ((?x387 (right s_____)))
 (let ((?x718 (top ?x387)))
 (let ((?x720 (stack ?x718 ?x573)))
 (let (($x746 (= ?x711 ?x720)))
 (let (($x721 ((_ is stack ) ?x387)))
 (let (($x722 (= c_____ right-to-center)))
 (let (($x723 (and $x722 $x721)))
 (let (($x724 (= c_____ right-to-left)))
 (let (($x725 (and $x724 $x721)))
 (let ((?x713 (rest ?x573)))
 (let (($x741 (= ?x711 ?x713)))
 (let (($x726 ((_ is stack ) ?x573)))
 (let (($x727 (= c_____ center-to-right)))
 (let (($x728 (and $x727 $x726)))
 (let (($x729 (= c_____ center-to-left)))
 (let (($x730 (and $x729 $x726)))
 (let ((?x353 (left s_____)))
 (let ((?x571 (top ?x353)))
 (let ((?x712 (stack ?x571 ?x573)))
 (let (($x740 (= ?x711 ?x712)))
 (let (($x731 ((_ is stack ) ?x353)))
 (let (($x732 (= c_____ left-to-right)))
 (let (($x733 (and $x732 $x731)))
 (let (($x763 (ite $x733 $x740 (ite $x730 $x741 (ite $x728 $x741 (ite $x725 $x749 (ite $x723 $x746 $x749)))))))
 (let (($x734 (= c_____ left-to-center)))
 (let (($x735 (and $x734 $x731)))
 (ite $x735 $x749 $x763)))))))))))))))))))))))))))))))
(assert
 (= c_______ c!6))
(assert
 (let ((?x491 (left s______)))
 (let ((?x629 (left s_______)))
 (let (($x889 (= ?x629 ?x491)))
 (let ((?x525 (right s______)))
 (let ((?x856 (top ?x525)))
 (let ((?x857 (stack ?x856 ?x491)))
 (let (($x883 (= ?x629 ?x857)))
 (let (($x859 ((_ is stack ) ?x525)))
 (let (($x862 (= c______ right-to-left)))
 (let (($x863 (and $x862 $x859)))
 (let ((?x711 (center s______)))
 (let (($x864 ((_ is stack ) ?x711)))
 (let (($x865 (= c______ center-to-right)))
 (let (($x866 (and $x865 $x864)))
 (let ((?x852 (top ?x711)))
 (let ((?x853 (stack ?x852 ?x491)))
 (let (($x880 (= ?x629 ?x853)))
 (let (($x867 (= c______ center-to-left)))
 (let (($x868 (and $x867 $x864)))
 (let ((?x662 (rest ?x491)))
 (let (($x876 (= ?x629 ?x662)))
 (let (($x869 ((_ is stack ) ?x491)))
 (let (($x870 (= c______ left-to-right)))
 (let (($x871 (and $x870 $x869)))
 (let (($x872 (= c______ left-to-center)))
 (let (($x873 (and $x872 $x869)))
 (ite $x873 $x876 (ite $x871 $x876 (ite $x868 $x880 (ite $x866 $x889 (ite $x863 $x883 $x889))))))))))))))))))))))))))))))))
(assert
 (let ((?x525 (right s______)))
 (let ((?x663 (right s_______)))
 (let (($x885 (= ?x663 ?x525)))
 (let ((?x855 (rest ?x525)))
 (let (($x882 (= ?x663 ?x855)))
 (let (($x859 ((_ is stack ) ?x525)))
 (let (($x860 (= c______ right-to-center)))
 (let (($x861 (and $x860 $x859)))
 (let (($x862 (= c______ right-to-left)))
 (let (($x863 (and $x862 $x859)))
 (let ((?x711 (center s______)))
 (let ((?x852 (top ?x711)))
 (let ((?x854 (stack ?x852 ?x525)))
 (let (($x881 (= ?x663 ?x854)))
 (let (($x864 ((_ is stack ) ?x711)))
 (let (($x865 (= c______ center-to-right)))
 (let (($x866 (and $x865 $x864)))
 (let (($x867 (= c______ center-to-left)))
 (let (($x868 (and $x867 $x864)))
 (let ((?x491 (left s______)))
 (let (($x869 ((_ is stack ) ?x491)))
 (let (($x870 (= c______ left-to-right)))
 (let (($x871 (and $x870 $x869)))
 (let (($x902 (ite $x871 $x885 (ite $x868 $x885 (ite $x866 $x881 (ite $x863 $x882 (ite $x861 $x882 $x885)))))))
 (let ((?x709 (top ?x491)))
 (let ((?x848 (stack ?x709 ?x525)))
 (let (($x877 (= ?x663 ?x848)))
 (let (($x872 (= c______ left-to-center)))
 (let (($x873 (and $x872 $x869)))
 (ite $x873 $x877 $x902)))))))))))))))))))))))))))))))
(assert
 (let ((?x711 (center s______)))
 (let ((?x849 (center s_______)))
 (let (($x887 (= ?x849 ?x711)))
 (let ((?x525 (right s______)))
 (let ((?x856 (top ?x525)))
 (let ((?x858 (stack ?x856 ?x711)))
 (let (($x884 (= ?x849 ?x858)))
 (let (($x859 ((_ is stack ) ?x525)))
 (let (($x860 (= c______ right-to-center)))
 (let (($x861 (and $x860 $x859)))
 (let (($x862 (= c______ right-to-left)))
 (let (($x863 (and $x862 $x859)))
 (let ((?x851 (rest ?x711)))
 (let (($x879 (= ?x849 ?x851)))
 (let (($x864 ((_ is stack ) ?x711)))
 (let (($x865 (= c______ center-to-right)))
 (let (($x866 (and $x865 $x864)))
 (let (($x867 (= c______ center-to-left)))
 (let (($x868 (and $x867 $x864)))
 (let ((?x491 (left s______)))
 (let ((?x709 (top ?x491)))
 (let ((?x850 (stack ?x709 ?x711)))
 (let (($x878 (= ?x849 ?x850)))
 (let (($x869 ((_ is stack ) ?x491)))
 (let (($x870 (= c______ left-to-right)))
 (let (($x871 (and $x870 $x869)))
 (let (($x901 (ite $x871 $x878 (ite $x868 $x879 (ite $x866 $x879 (ite $x863 $x887 (ite $x861 $x884 $x887)))))))
 (let (($x872 (= c______ left-to-center)))
 (let (($x873 (and $x872 $x869)))
 (ite $x873 $x887 $x901)))))))))))))))))))))))))))))))
(assert
 (= c________ c!7))
(assert
 (let ((?x629 (left s_______)))
 (let ((?x767 (left s________)))
 (let (($x1027 (= ?x767 ?x629)))
 (let ((?x663 (right s_______)))
 (let ((?x994 (top ?x663)))
 (let ((?x995 (stack ?x994 ?x629)))
 (let (($x1021 (= ?x767 ?x995)))
 (let (($x997 ((_ is stack ) ?x663)))
 (let (($x1000 (= c_______ right-to-left)))
 (let (($x1001 (and $x1000 $x997)))
 (let ((?x849 (center s_______)))
 (let (($x1002 ((_ is stack ) ?x849)))
 (let (($x1003 (= c_______ center-to-right)))
 (let (($x1004 (and $x1003 $x1002)))
 (let ((?x990 (top ?x849)))
 (let ((?x991 (stack ?x990 ?x629)))
 (let (($x1018 (= ?x767 ?x991)))
 (let (($x1005 (= c_______ center-to-left)))
 (let (($x1006 (and $x1005 $x1002)))
 (let ((?x800 (rest ?x629)))
 (let (($x1014 (= ?x767 ?x800)))
 (let (($x1007 ((_ is stack ) ?x629)))
 (let (($x1008 (= c_______ left-to-right)))
 (let (($x1009 (and $x1008 $x1007)))
 (let (($x1010 (= c_______ left-to-center)))
 (let (($x1011 (and $x1010 $x1007)))
 (ite $x1011 $x1014 (ite $x1009 $x1014 (ite $x1006 $x1018 (ite $x1004 $x1027 (ite $x1001 $x1021 $x1027))))))))))))))))))))))))))))))))
(assert
 (let ((?x663 (right s_______)))
 (let ((?x801 (right s________)))
 (let (($x1023 (= ?x801 ?x663)))
 (let ((?x993 (rest ?x663)))
 (let (($x1020 (= ?x801 ?x993)))
 (let (($x997 ((_ is stack ) ?x663)))
 (let (($x998 (= c_______ right-to-center)))
 (let (($x999 (and $x998 $x997)))
 (let (($x1000 (= c_______ right-to-left)))
 (let (($x1001 (and $x1000 $x997)))
 (let ((?x849 (center s_______)))
 (let ((?x990 (top ?x849)))
 (let ((?x992 (stack ?x990 ?x663)))
 (let (($x1019 (= ?x801 ?x992)))
 (let (($x1002 ((_ is stack ) ?x849)))
 (let (($x1003 (= c_______ center-to-right)))
 (let (($x1004 (and $x1003 $x1002)))
 (let (($x1005 (= c_______ center-to-left)))
 (let (($x1006 (and $x1005 $x1002)))
 (let ((?x629 (left s_______)))
 (let (($x1007 ((_ is stack ) ?x629)))
 (let (($x1008 (= c_______ left-to-right)))
 (let (($x1009 (and $x1008 $x1007)))
 (let (($x1040 (ite $x1009 $x1023 (ite $x1006 $x1023 (ite $x1004 $x1019 (ite $x1001 $x1020 (ite $x999 $x1020 $x1023)))))))
 (let ((?x847 (top ?x629)))
 (let ((?x986 (stack ?x847 ?x663)))
 (let (($x1015 (= ?x801 ?x986)))
 (let (($x1010 (= c_______ left-to-center)))
 (let (($x1011 (and $x1010 $x1007)))
 (ite $x1011 $x1015 $x1040)))))))))))))))))))))))))))))))
(assert
 (let ((?x849 (center s_______)))
 (let ((?x987 (center s________)))
 (let (($x1025 (= ?x987 ?x849)))
 (let ((?x663 (right s_______)))
 (let ((?x994 (top ?x663)))
 (let ((?x996 (stack ?x994 ?x849)))
 (let (($x1022 (= ?x987 ?x996)))
 (let (($x997 ((_ is stack ) ?x663)))
 (let (($x998 (= c_______ right-to-center)))
 (let (($x999 (and $x998 $x997)))
 (let (($x1000 (= c_______ right-to-left)))
 (let (($x1001 (and $x1000 $x997)))
 (let ((?x989 (rest ?x849)))
 (let (($x1017 (= ?x987 ?x989)))
 (let (($x1002 ((_ is stack ) ?x849)))
 (let (($x1003 (= c_______ center-to-right)))
 (let (($x1004 (and $x1003 $x1002)))
 (let (($x1005 (= c_______ center-to-left)))
 (let (($x1006 (and $x1005 $x1002)))
 (let ((?x629 (left s_______)))
 (let ((?x847 (top ?x629)))
 (let ((?x988 (stack ?x847 ?x849)))
 (let (($x1016 (= ?x987 ?x988)))
 (let (($x1007 ((_ is stack ) ?x629)))
 (let (($x1008 (= c_______ left-to-right)))
 (let (($x1009 (and $x1008 $x1007)))
 (let (($x1039 (ite $x1009 $x1016 (ite $x1006 $x1017 (ite $x1004 $x1017 (ite $x1001 $x1025 (ite $x999 $x1022 $x1025)))))))
 (let (($x1010 (= c_______ left-to-center)))
 (let (($x1011 (and $x1010 $x1007)))
 (ite $x1011 $x1025 $x1039)))))))))))))))))))))))))))))))
(assert
 (let ((?x28 (stack E (stack B (stack A empty)))))
 (let ((?x801 (right s________)))
 (let (($x1043 (= ?x801 ?x28)))
 (let ((?x24 (stack F (stack D (stack C empty)))))
 (let ((?x987 (center s________)))
 (let (($x1074 (= ?x987 ?x24)))
 (let ((?x767 (left s________)))
 (let (($x1075 (= ?x767 empty)))
 (let (($x1077 (not (and $x1075 $x1074 $x1043))))
 (not $x1077)))))))))))
(check-sat)