; 
(set-info :status unknown)
(set-logic QF_DT)
(declare-datatypes ((Enum_A_B_C_D_E_F_G_H_I_J_K_L_M_N_O_P_Q_R_S_T_U_V_W_X_Y 0)) (((A) (B) (C) (D) (E) (F) (G) (H) (I) (J) (K) (L) (M) (N) (O) (P) (Q) (R) (S) (T) (U) (V) (W) (X) (Y))))
 (declare-datatypes ((Tower 0)) (((stack (top Enum_A_B_C_D_E_F_G_H_I_J_K_L_M_N_O_P_Q_R_S_T_U_V_W_X_Y) (rest Tower)) (empty))))
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
 (let ((?x37 (stack L empty)))
 (let ((?x38 (stack K ?x37)))
 (let ((?x39 (stack Y ?x38)))
 (let ((?x40 (stack B ?x39)))
 (let ((?x41 (stack M ?x40)))
 (let ((?x42 (stack W ?x41)))
 (let ((?x43 (stack N ?x42)))
 (let ((?x44 (stack D ?x43)))
 (let ((?x45 (stack P ?x44)))
 (let ((?x46 (stack J ?x45)))
 (let ((?x47 (stack Q ?x46)))
 (let ((?x48 (stack S ?x47)))
 (let ((?x49 (stack C ?x48)))
 (let ((?x50 (stack R ?x49)))
 (let ((?x51 (stack I ?x50)))
 (let ((?x52 (stack T ?x51)))
 (let ((?x53 (stack O ?x52)))
 (let ((?x54 (stack U ?x53)))
 (let ((?x55 (stack G ?x54)))
 (let ((?x56 (stack A ?x55)))
 (let ((?x57 (stack E ?x56)))
 (let ((?x132 (left s_tmp_)))
 (= ?x132 ?x57))))))))))))))))))))))))
(assert
 (let ((?x135 (center s_tmp__)))
 (= ?x135 empty)))
(assert
 (let ((?x58 (stack F empty)))
 (let ((?x59 (stack H ?x58)))
 (let ((?x60 (stack V ?x59)))
 (let ((?x61 (stack X ?x60)))
 (let ((?x138 (right s_tmp___)))
 (= ?x138 ?x61)))))))
(assert
 (= c_tmp____ c!0))
(assert
 (= s_ s_tmp___))
(assert
 (= c_ c_tmp____))
(assert
 (let ((?x165 (left s_tmp__)))
 (= (left s_tmp___) ?x165)))
(assert
 (let ((?x135 (center s_tmp__)))
 (= (center s_tmp___) ?x135)))
(assert
 (let ((?x169 (center s_tmp_)))
 (= ?x169 (center s_tmp))))
(assert
 (let ((?x172 (right s_tmp_)))
 (= ?x172 (right s_tmp))))
(assert
 (let ((?x132 (left s_tmp_)))
 (let ((?x165 (left s_tmp__)))
 (= ?x165 ?x132))))
(assert
 (let ((?x172 (right s_tmp_)))
 (let ((?x176 (right s_tmp__)))
 (= ?x176 ?x172))))
(assert
 (= c__ c!1))
(assert
 (let ((?x109 (left s_)))
 (let ((?x331 (left s__)))
 (let (($x340 (= ?x331 ?x109)))
 (let ((?x112 (right s_)))
 (let (($x310 ((_ is stack ) ?x112)))
 (let (($x313 (= c_ right-to-left)))
 (let (($x332 (and $x313 $x310)))
 (let ((?x114 (center s_)))
 (let (($x333 ((_ is stack ) ?x114)))
 (let (($x305 (and (= c_ center-to-right) $x333)))
 (let (($x307 (and (= c_ center-to-left) $x333)))
 (let (($x349 (ite $x307 (= ?x331 (stack (top ?x114) ?x109)) (ite $x305 $x340 (ite $x332 (= ?x331 (stack (top ?x112) ?x109)) $x340)))))
 (let (($x297 (= ?x331 (rest ?x109))))
 (let (($x314 ((_ is stack ) ?x109)))
 (let (($x315 (= c_ left-to-right)))
 (let (($x316 (and $x315 $x314)))
 (let (($x323 (= c_ left-to-center)))
 (let (($x324 (and $x323 $x314)))
 (ite $x324 $x297 (ite $x316 $x297 $x349)))))))))))))))))))))
(assert
 (let ((?x112 (right s_)))
 (let ((?x303 (right s__)))
 (let (($x336 (= ?x303 ?x112)))
 (let (($x321 (= ?x303 (rest ?x112))))
 (let (($x310 ((_ is stack ) ?x112)))
 (let (($x311 (= c_ right-to-center)))
 (let (($x312 (and $x311 $x310)))
 (let (($x313 (= c_ right-to-left)))
 (let (($x332 (and $x313 $x310)))
 (let ((?x114 (center s_)))
 (let (($x333 ((_ is stack ) ?x114)))
 (let (($x305 (and (= c_ center-to-right) $x333)))
 (let (($x346 (ite $x305 (= ?x303 (stack (top ?x114) ?x112)) (ite $x332 $x321 (ite $x312 $x321 $x336)))))
 (let (($x307 (and (= c_ center-to-left) $x333)))
 (let ((?x109 (left s_)))
 (let (($x314 ((_ is stack ) ?x109)))
 (let (($x315 (= c_ left-to-right)))
 (let (($x316 (and $x315 $x314)))
 (let (($x323 (= c_ left-to-center)))
 (let (($x324 (and $x323 $x314)))
 (ite $x324 (= ?x303 (stack (top ?x109) ?x112)) (ite $x316 $x336 (ite $x307 $x336 $x346))))))))))))))))))))))))
(assert
 (let ((?x114 (center s_)))
 (let ((?x327 (center s__)))
 (let (($x338 (= ?x327 ?x114)))
 (let ((?x112 (right s_)))
 (let (($x310 ((_ is stack ) ?x112)))
 (let (($x311 (= c_ right-to-center)))
 (let (($x312 (and $x311 $x310)))
 (let (($x313 (= c_ right-to-left)))
 (let (($x332 (and $x313 $x310)))
 (let (($x300 (= ?x327 (rest ?x114))))
 (let (($x333 ((_ is stack ) ?x114)))
 (let (($x305 (and (= c_ center-to-right) $x333)))
 (let (($x345 (ite $x305 $x300 (ite $x332 $x338 (ite $x312 (= ?x327 (stack (top ?x112) ?x114)) $x338)))))
 (let (($x307 (and (= c_ center-to-left) $x333)))
 (let ((?x109 (left s_)))
 (let (($x314 ((_ is stack ) ?x109)))
 (let (($x315 (= c_ left-to-right)))
 (let (($x316 (and $x315 $x314)))
 (let (($x323 (= c_ left-to-center)))
 (let (($x324 (and $x323 $x314)))
 (ite $x324 $x338 (ite $x316 (= ?x327 (stack (top ?x109) ?x114)) (ite $x307 $x300 $x345))))))))))))))))))))))))
(assert
 (= c___ c!2))
(assert
 (let ((?x331 (left s__)))
 (let ((?x179 (left s___)))
 (let (($x452 (= ?x179 ?x331)))
 (let ((?x303 (right s__)))
 (let ((?x419 (top ?x303)))
 (let ((?x420 (stack ?x419 ?x331)))
 (let (($x446 (= ?x179 ?x420)))
 (let (($x422 ((_ is stack ) ?x303)))
 (let (($x425 (= c__ right-to-left)))
 (let (($x426 (and $x425 $x422)))
 (let ((?x327 (center s__)))
 (let (($x427 ((_ is stack ) ?x327)))
 (let (($x428 (= c__ center-to-right)))
 (let (($x429 (and $x428 $x427)))
 (let ((?x415 (top ?x327)))
 (let ((?x416 (stack ?x415 ?x331)))
 (let (($x443 (= ?x179 ?x416)))
 (let (($x430 (= c__ center-to-left)))
 (let (($x431 (and $x430 $x427)))
 (let ((?x181 (rest ?x331)))
 (let (($x439 (= ?x179 ?x181)))
 (let (($x432 ((_ is stack ) ?x331)))
 (let (($x433 (= c__ left-to-right)))
 (let (($x434 (and $x433 $x432)))
 (let (($x435 (= c__ left-to-center)))
 (let (($x436 (and $x435 $x432)))
 (ite $x436 $x439 (ite $x434 $x439 (ite $x431 $x443 (ite $x429 $x452 (ite $x426 $x446 $x452))))))))))))))))))))))))))))))))
(assert
 (let ((?x303 (right s__)))
 (let ((?x182 (right s___)))
 (let (($x448 (= ?x182 ?x303)))
 (let ((?x418 (rest ?x303)))
 (let (($x445 (= ?x182 ?x418)))
 (let (($x422 ((_ is stack ) ?x303)))
 (let (($x423 (= c__ right-to-center)))
 (let (($x424 (and $x423 $x422)))
 (let (($x425 (= c__ right-to-left)))
 (let (($x426 (and $x425 $x422)))
 (let ((?x327 (center s__)))
 (let ((?x415 (top ?x327)))
 (let ((?x417 (stack ?x415 ?x303)))
 (let (($x444 (= ?x182 ?x417)))
 (let (($x427 ((_ is stack ) ?x327)))
 (let (($x428 (= c__ center-to-right)))
 (let (($x429 (and $x428 $x427)))
 (let (($x430 (= c__ center-to-left)))
 (let (($x431 (and $x430 $x427)))
 (let ((?x331 (left s__)))
 (let (($x432 ((_ is stack ) ?x331)))
 (let (($x433 (= c__ left-to-right)))
 (let (($x434 (and $x433 $x432)))
 (let (($x465 (ite $x434 $x448 (ite $x431 $x448 (ite $x429 $x444 (ite $x426 $x445 (ite $x424 $x445 $x448)))))))
 (let ((?x292 (top ?x331)))
 (let ((?x411 (stack ?x292 ?x303)))
 (let (($x440 (= ?x182 ?x411)))
 (let (($x435 (= c__ left-to-center)))
 (let (($x436 (and $x435 $x432)))
 (ite $x436 $x440 $x465)))))))))))))))))))))))))))))))
(assert
 (let ((?x327 (center s__)))
 (let ((?x412 (center s___)))
 (let (($x450 (= ?x412 ?x327)))
 (let ((?x303 (right s__)))
 (let ((?x419 (top ?x303)))
 (let ((?x421 (stack ?x419 ?x327)))
 (let (($x447 (= ?x412 ?x421)))
 (let (($x422 ((_ is stack ) ?x303)))
 (let (($x423 (= c__ right-to-center)))
 (let (($x424 (and $x423 $x422)))
 (let (($x425 (= c__ right-to-left)))
 (let (($x426 (and $x425 $x422)))
 (let ((?x414 (rest ?x327)))
 (let (($x442 (= ?x412 ?x414)))
 (let (($x427 ((_ is stack ) ?x327)))
 (let (($x428 (= c__ center-to-right)))
 (let (($x429 (and $x428 $x427)))
 (let (($x430 (= c__ center-to-left)))
 (let (($x431 (and $x430 $x427)))
 (let ((?x331 (left s__)))
 (let ((?x292 (top ?x331)))
 (let ((?x413 (stack ?x292 ?x327)))
 (let (($x441 (= ?x412 ?x413)))
 (let (($x432 ((_ is stack ) ?x331)))
 (let (($x433 (= c__ left-to-right)))
 (let (($x434 (and $x433 $x432)))
 (let (($x464 (ite $x434 $x441 (ite $x431 $x442 (ite $x429 $x442 (ite $x426 $x450 (ite $x424 $x447 $x450)))))))
 (let (($x435 (= c__ left-to-center)))
 (let (($x436 (and $x435 $x432)))
 (ite $x436 $x450 $x464)))))))))))))))))))))))))))))))
(assert
 (= c____ c!3))
(assert
 (let ((?x179 (left s___)))
 (let ((?x356 (left s____)))
 (let (($x590 (= ?x356 ?x179)))
 (let ((?x182 (right s___)))
 (let ((?x557 (top ?x182)))
 (let ((?x558 (stack ?x557 ?x179)))
 (let (($x584 (= ?x356 ?x558)))
 (let (($x560 ((_ is stack ) ?x182)))
 (let (($x563 (= c___ right-to-left)))
 (let (($x564 (and $x563 $x560)))
 (let ((?x412 (center s___)))
 (let (($x565 ((_ is stack ) ?x412)))
 (let (($x566 (= c___ center-to-right)))
 (let (($x567 (and $x566 $x565)))
 (let ((?x553 (top ?x412)))
 (let ((?x554 (stack ?x553 ?x179)))
 (let (($x581 (= ?x356 ?x554)))
 (let (($x568 (= c___ center-to-left)))
 (let (($x569 (and $x568 $x565)))
 (let ((?x388 (rest ?x179)))
 (let (($x577 (= ?x356 ?x388)))
 (let (($x570 ((_ is stack ) ?x179)))
 (let (($x571 (= c___ left-to-right)))
 (let (($x572 (and $x571 $x570)))
 (let (($x573 (= c___ left-to-center)))
 (let (($x574 (and $x573 $x570)))
 (ite $x574 $x577 (ite $x572 $x577 (ite $x569 $x581 (ite $x567 $x590 (ite $x564 $x584 $x590))))))))))))))))))))))))))))))))
(assert
 (let ((?x182 (right s___)))
 (let ((?x389 (right s____)))
 (let (($x586 (= ?x389 ?x182)))
 (let ((?x556 (rest ?x182)))
 (let (($x583 (= ?x389 ?x556)))
 (let (($x560 ((_ is stack ) ?x182)))
 (let (($x561 (= c___ right-to-center)))
 (let (($x562 (and $x561 $x560)))
 (let (($x563 (= c___ right-to-left)))
 (let (($x564 (and $x563 $x560)))
 (let ((?x412 (center s___)))
 (let ((?x553 (top ?x412)))
 (let ((?x555 (stack ?x553 ?x182)))
 (let (($x582 (= ?x389 ?x555)))
 (let (($x565 ((_ is stack ) ?x412)))
 (let (($x566 (= c___ center-to-right)))
 (let (($x567 (and $x566 $x565)))
 (let (($x568 (= c___ center-to-left)))
 (let (($x569 (and $x568 $x565)))
 (let ((?x179 (left s___)))
 (let (($x570 ((_ is stack ) ?x179)))
 (let (($x571 (= c___ left-to-right)))
 (let (($x572 (and $x571 $x570)))
 (let (($x603 (ite $x572 $x586 (ite $x569 $x586 (ite $x567 $x582 (ite $x564 $x583 (ite $x562 $x583 $x586)))))))
 (let ((?x410 (top ?x179)))
 (let ((?x549 (stack ?x410 ?x182)))
 (let (($x578 (= ?x389 ?x549)))
 (let (($x573 (= c___ left-to-center)))
 (let (($x574 (and $x573 $x570)))
 (ite $x574 $x578 $x603)))))))))))))))))))))))))))))))
(assert
 (let ((?x412 (center s___)))
 (let ((?x550 (center s____)))
 (let (($x588 (= ?x550 ?x412)))
 (let ((?x182 (right s___)))
 (let ((?x557 (top ?x182)))
 (let ((?x559 (stack ?x557 ?x412)))
 (let (($x585 (= ?x550 ?x559)))
 (let (($x560 ((_ is stack ) ?x182)))
 (let (($x561 (= c___ right-to-center)))
 (let (($x562 (and $x561 $x560)))
 (let (($x563 (= c___ right-to-left)))
 (let (($x564 (and $x563 $x560)))
 (let ((?x552 (rest ?x412)))
 (let (($x580 (= ?x550 ?x552)))
 (let (($x565 ((_ is stack ) ?x412)))
 (let (($x566 (= c___ center-to-right)))
 (let (($x567 (and $x566 $x565)))
 (let (($x568 (= c___ center-to-left)))
 (let (($x569 (and $x568 $x565)))
 (let ((?x179 (left s___)))
 (let ((?x410 (top ?x179)))
 (let ((?x551 (stack ?x410 ?x412)))
 (let (($x579 (= ?x550 ?x551)))
 (let (($x570 ((_ is stack ) ?x179)))
 (let (($x571 (= c___ left-to-right)))
 (let (($x572 (and $x571 $x570)))
 (let (($x602 (ite $x572 $x579 (ite $x569 $x580 (ite $x567 $x580 (ite $x564 $x588 (ite $x562 $x585 $x588)))))))
 (let (($x573 (= c___ left-to-center)))
 (let (($x574 (and $x573 $x570)))
 (ite $x574 $x588 $x602)))))))))))))))))))))))))))))))
(assert
 (= c_____ c!4))
(assert
 (let ((?x356 (left s____)))
 (let ((?x468 (left s_____)))
 (let (($x728 (= ?x468 ?x356)))
 (let ((?x389 (right s____)))
 (let ((?x695 (top ?x389)))
 (let ((?x696 (stack ?x695 ?x356)))
 (let (($x722 (= ?x468 ?x696)))
 (let (($x698 ((_ is stack ) ?x389)))
 (let (($x701 (= c____ right-to-left)))
 (let (($x702 (and $x701 $x698)))
 (let ((?x550 (center s____)))
 (let (($x703 ((_ is stack ) ?x550)))
 (let (($x704 (= c____ center-to-right)))
 (let (($x705 (and $x704 $x703)))
 (let ((?x691 (top ?x550)))
 (let ((?x692 (stack ?x691 ?x356)))
 (let (($x719 (= ?x468 ?x692)))
 (let (($x706 (= c____ center-to-left)))
 (let (($x707 (and $x706 $x703)))
 (let ((?x501 (rest ?x356)))
 (let (($x715 (= ?x468 ?x501)))
 (let (($x708 ((_ is stack ) ?x356)))
 (let (($x709 (= c____ left-to-right)))
 (let (($x710 (and $x709 $x708)))
 (let (($x711 (= c____ left-to-center)))
 (let (($x712 (and $x711 $x708)))
 (ite $x712 $x715 (ite $x710 $x715 (ite $x707 $x719 (ite $x705 $x728 (ite $x702 $x722 $x728))))))))))))))))))))))))))))))))
(assert
 (let ((?x389 (right s____)))
 (let ((?x502 (right s_____)))
 (let (($x724 (= ?x502 ?x389)))
 (let ((?x694 (rest ?x389)))
 (let (($x721 (= ?x502 ?x694)))
 (let (($x698 ((_ is stack ) ?x389)))
 (let (($x699 (= c____ right-to-center)))
 (let (($x700 (and $x699 $x698)))
 (let (($x701 (= c____ right-to-left)))
 (let (($x702 (and $x701 $x698)))
 (let ((?x550 (center s____)))
 (let ((?x691 (top ?x550)))
 (let ((?x693 (stack ?x691 ?x389)))
 (let (($x720 (= ?x502 ?x693)))
 (let (($x703 ((_ is stack ) ?x550)))
 (let (($x704 (= c____ center-to-right)))
 (let (($x705 (and $x704 $x703)))
 (let (($x706 (= c____ center-to-left)))
 (let (($x707 (and $x706 $x703)))
 (let ((?x356 (left s____)))
 (let (($x708 ((_ is stack ) ?x356)))
 (let (($x709 (= c____ left-to-right)))
 (let (($x710 (and $x709 $x708)))
 (let (($x741 (ite $x710 $x724 (ite $x707 $x724 (ite $x705 $x720 (ite $x702 $x721 (ite $x700 $x721 $x724)))))))
 (let ((?x548 (top ?x356)))
 (let ((?x687 (stack ?x548 ?x389)))
 (let (($x716 (= ?x502 ?x687)))
 (let (($x711 (= c____ left-to-center)))
 (let (($x712 (and $x711 $x708)))
 (ite $x712 $x716 $x741)))))))))))))))))))))))))))))))
(assert
 (let ((?x550 (center s____)))
 (let ((?x688 (center s_____)))
 (let (($x726 (= ?x688 ?x550)))
 (let ((?x389 (right s____)))
 (let ((?x695 (top ?x389)))
 (let ((?x697 (stack ?x695 ?x550)))
 (let (($x723 (= ?x688 ?x697)))
 (let (($x698 ((_ is stack ) ?x389)))
 (let (($x699 (= c____ right-to-center)))
 (let (($x700 (and $x699 $x698)))
 (let (($x701 (= c____ right-to-left)))
 (let (($x702 (and $x701 $x698)))
 (let ((?x690 (rest ?x550)))
 (let (($x718 (= ?x688 ?x690)))
 (let (($x703 ((_ is stack ) ?x550)))
 (let (($x704 (= c____ center-to-right)))
 (let (($x705 (and $x704 $x703)))
 (let (($x706 (= c____ center-to-left)))
 (let (($x707 (and $x706 $x703)))
 (let ((?x356 (left s____)))
 (let ((?x548 (top ?x356)))
 (let ((?x689 (stack ?x548 ?x550)))
 (let (($x717 (= ?x688 ?x689)))
 (let (($x708 ((_ is stack ) ?x356)))
 (let (($x709 (= c____ left-to-right)))
 (let (($x710 (and $x709 $x708)))
 (let (($x740 (ite $x710 $x717 (ite $x707 $x718 (ite $x705 $x718 (ite $x702 $x726 (ite $x700 $x723 $x726)))))))
 (let (($x711 (= c____ left-to-center)))
 (let (($x712 (and $x711 $x708)))
 (ite $x712 $x726 $x740)))))))))))))))))))))))))))))))
(assert
 (= c______ c!5))
(assert
 (let ((?x468 (left s_____)))
 (let ((?x606 (left s______)))
 (let (($x866 (= ?x606 ?x468)))
 (let ((?x502 (right s_____)))
 (let ((?x833 (top ?x502)))
 (let ((?x834 (stack ?x833 ?x468)))
 (let (($x860 (= ?x606 ?x834)))
 (let (($x836 ((_ is stack ) ?x502)))
 (let (($x839 (= c_____ right-to-left)))
 (let (($x840 (and $x839 $x836)))
 (let ((?x688 (center s_____)))
 (let (($x841 ((_ is stack ) ?x688)))
 (let (($x842 (= c_____ center-to-right)))
 (let (($x843 (and $x842 $x841)))
 (let ((?x829 (top ?x688)))
 (let ((?x830 (stack ?x829 ?x468)))
 (let (($x857 (= ?x606 ?x830)))
 (let (($x844 (= c_____ center-to-left)))
 (let (($x845 (and $x844 $x841)))
 (let ((?x639 (rest ?x468)))
 (let (($x853 (= ?x606 ?x639)))
 (let (($x846 ((_ is stack ) ?x468)))
 (let (($x847 (= c_____ left-to-right)))
 (let (($x848 (and $x847 $x846)))
 (let (($x849 (= c_____ left-to-center)))
 (let (($x850 (and $x849 $x846)))
 (ite $x850 $x853 (ite $x848 $x853 (ite $x845 $x857 (ite $x843 $x866 (ite $x840 $x860 $x866))))))))))))))))))))))))))))))))
(assert
 (let ((?x502 (right s_____)))
 (let ((?x640 (right s______)))
 (let (($x862 (= ?x640 ?x502)))
 (let ((?x832 (rest ?x502)))
 (let (($x859 (= ?x640 ?x832)))
 (let (($x836 ((_ is stack ) ?x502)))
 (let (($x837 (= c_____ right-to-center)))
 (let (($x838 (and $x837 $x836)))
 (let (($x839 (= c_____ right-to-left)))
 (let (($x840 (and $x839 $x836)))
 (let ((?x688 (center s_____)))
 (let ((?x829 (top ?x688)))
 (let ((?x831 (stack ?x829 ?x502)))
 (let (($x858 (= ?x640 ?x831)))
 (let (($x841 ((_ is stack ) ?x688)))
 (let (($x842 (= c_____ center-to-right)))
 (let (($x843 (and $x842 $x841)))
 (let (($x844 (= c_____ center-to-left)))
 (let (($x845 (and $x844 $x841)))
 (let ((?x468 (left s_____)))
 (let (($x846 ((_ is stack ) ?x468)))
 (let (($x847 (= c_____ left-to-right)))
 (let (($x848 (and $x847 $x846)))
 (let (($x879 (ite $x848 $x862 (ite $x845 $x862 (ite $x843 $x858 (ite $x840 $x859 (ite $x838 $x859 $x862)))))))
 (let ((?x686 (top ?x468)))
 (let ((?x825 (stack ?x686 ?x502)))
 (let (($x854 (= ?x640 ?x825)))
 (let (($x849 (= c_____ left-to-center)))
 (let (($x850 (and $x849 $x846)))
 (ite $x850 $x854 $x879)))))))))))))))))))))))))))))))
(assert
 (let ((?x688 (center s_____)))
 (let ((?x826 (center s______)))
 (let (($x864 (= ?x826 ?x688)))
 (let ((?x502 (right s_____)))
 (let ((?x833 (top ?x502)))
 (let ((?x835 (stack ?x833 ?x688)))
 (let (($x861 (= ?x826 ?x835)))
 (let (($x836 ((_ is stack ) ?x502)))
 (let (($x837 (= c_____ right-to-center)))
 (let (($x838 (and $x837 $x836)))
 (let (($x839 (= c_____ right-to-left)))
 (let (($x840 (and $x839 $x836)))
 (let ((?x828 (rest ?x688)))
 (let (($x856 (= ?x826 ?x828)))
 (let (($x841 ((_ is stack ) ?x688)))
 (let (($x842 (= c_____ center-to-right)))
 (let (($x843 (and $x842 $x841)))
 (let (($x844 (= c_____ center-to-left)))
 (let (($x845 (and $x844 $x841)))
 (let ((?x468 (left s_____)))
 (let ((?x686 (top ?x468)))
 (let ((?x827 (stack ?x686 ?x688)))
 (let (($x855 (= ?x826 ?x827)))
 (let (($x846 ((_ is stack ) ?x468)))
 (let (($x847 (= c_____ left-to-right)))
 (let (($x848 (and $x847 $x846)))
 (let (($x878 (ite $x848 $x855 (ite $x845 $x856 (ite $x843 $x856 (ite $x840 $x864 (ite $x838 $x861 $x864)))))))
 (let (($x849 (= c_____ left-to-center)))
 (let (($x850 (and $x849 $x846)))
 (ite $x850 $x864 $x878)))))))))))))))))))))))))))))))
(assert
 (= c_______ c!6))
(assert
 (let ((?x606 (left s______)))
 (let ((?x744 (left s_______)))
 (let (($x1004 (= ?x744 ?x606)))
 (let ((?x640 (right s______)))
 (let ((?x971 (top ?x640)))
 (let ((?x972 (stack ?x971 ?x606)))
 (let (($x998 (= ?x744 ?x972)))
 (let (($x974 ((_ is stack ) ?x640)))
 (let (($x977 (= c______ right-to-left)))
 (let (($x978 (and $x977 $x974)))
 (let ((?x826 (center s______)))
 (let (($x979 ((_ is stack ) ?x826)))
 (let (($x980 (= c______ center-to-right)))
 (let (($x981 (and $x980 $x979)))
 (let ((?x967 (top ?x826)))
 (let ((?x968 (stack ?x967 ?x606)))
 (let (($x995 (= ?x744 ?x968)))
 (let (($x982 (= c______ center-to-left)))
 (let (($x983 (and $x982 $x979)))
 (let ((?x777 (rest ?x606)))
 (let (($x991 (= ?x744 ?x777)))
 (let (($x984 ((_ is stack ) ?x606)))
 (let (($x985 (= c______ left-to-right)))
 (let (($x986 (and $x985 $x984)))
 (let (($x987 (= c______ left-to-center)))
 (let (($x988 (and $x987 $x984)))
 (ite $x988 $x991 (ite $x986 $x991 (ite $x983 $x995 (ite $x981 $x1004 (ite $x978 $x998 $x1004))))))))))))))))))))))))))))))))
(assert
 (let ((?x640 (right s______)))
 (let ((?x778 (right s_______)))
 (let (($x1000 (= ?x778 ?x640)))
 (let ((?x970 (rest ?x640)))
 (let (($x997 (= ?x778 ?x970)))
 (let (($x974 ((_ is stack ) ?x640)))
 (let (($x975 (= c______ right-to-center)))
 (let (($x976 (and $x975 $x974)))
 (let (($x977 (= c______ right-to-left)))
 (let (($x978 (and $x977 $x974)))
 (let ((?x826 (center s______)))
 (let ((?x967 (top ?x826)))
 (let ((?x969 (stack ?x967 ?x640)))
 (let (($x996 (= ?x778 ?x969)))
 (let (($x979 ((_ is stack ) ?x826)))
 (let (($x980 (= c______ center-to-right)))
 (let (($x981 (and $x980 $x979)))
 (let (($x982 (= c______ center-to-left)))
 (let (($x983 (and $x982 $x979)))
 (let ((?x606 (left s______)))
 (let (($x984 ((_ is stack ) ?x606)))
 (let (($x985 (= c______ left-to-right)))
 (let (($x986 (and $x985 $x984)))
 (let (($x1017 (ite $x986 $x1000 (ite $x983 $x1000 (ite $x981 $x996 (ite $x978 $x997 (ite $x976 $x997 $x1000)))))))
 (let ((?x824 (top ?x606)))
 (let ((?x963 (stack ?x824 ?x640)))
 (let (($x992 (= ?x778 ?x963)))
 (let (($x987 (= c______ left-to-center)))
 (let (($x988 (and $x987 $x984)))
 (ite $x988 $x992 $x1017)))))))))))))))))))))))))))))))
(assert
 (let ((?x826 (center s______)))
 (let ((?x964 (center s_______)))
 (let (($x1002 (= ?x964 ?x826)))
 (let ((?x640 (right s______)))
 (let ((?x971 (top ?x640)))
 (let ((?x973 (stack ?x971 ?x826)))
 (let (($x999 (= ?x964 ?x973)))
 (let (($x974 ((_ is stack ) ?x640)))
 (let (($x975 (= c______ right-to-center)))
 (let (($x976 (and $x975 $x974)))
 (let (($x977 (= c______ right-to-left)))
 (let (($x978 (and $x977 $x974)))
 (let ((?x966 (rest ?x826)))
 (let (($x994 (= ?x964 ?x966)))
 (let (($x979 ((_ is stack ) ?x826)))
 (let (($x980 (= c______ center-to-right)))
 (let (($x981 (and $x980 $x979)))
 (let (($x982 (= c______ center-to-left)))
 (let (($x983 (and $x982 $x979)))
 (let ((?x606 (left s______)))
 (let ((?x824 (top ?x606)))
 (let ((?x965 (stack ?x824 ?x826)))
 (let (($x993 (= ?x964 ?x965)))
 (let (($x984 ((_ is stack ) ?x606)))
 (let (($x985 (= c______ left-to-right)))
 (let (($x986 (and $x985 $x984)))
 (let (($x1016 (ite $x986 $x993 (ite $x983 $x994 (ite $x981 $x994 (ite $x978 $x1002 (ite $x976 $x999 $x1002)))))))
 (let (($x987 (= c______ left-to-center)))
 (let (($x988 (and $x987 $x984)))
 (ite $x988 $x1002 $x1016)))))))))))))))))))))))))))))))
(assert
 (= c________ c!7))
(assert
 (let ((?x744 (left s_______)))
 (let ((?x882 (left s________)))
 (let (($x1142 (= ?x882 ?x744)))
 (let ((?x778 (right s_______)))
 (let ((?x1109 (top ?x778)))
 (let ((?x1110 (stack ?x1109 ?x744)))
 (let (($x1136 (= ?x882 ?x1110)))
 (let (($x1112 ((_ is stack ) ?x778)))
 (let (($x1115 (= c_______ right-to-left)))
 (let (($x1116 (and $x1115 $x1112)))
 (let ((?x964 (center s_______)))
 (let (($x1117 ((_ is stack ) ?x964)))
 (let (($x1118 (= c_______ center-to-right)))
 (let (($x1119 (and $x1118 $x1117)))
 (let ((?x1105 (top ?x964)))
 (let ((?x1106 (stack ?x1105 ?x744)))
 (let (($x1133 (= ?x882 ?x1106)))
 (let (($x1120 (= c_______ center-to-left)))
 (let (($x1121 (and $x1120 $x1117)))
 (let ((?x915 (rest ?x744)))
 (let (($x1129 (= ?x882 ?x915)))
 (let (($x1122 ((_ is stack ) ?x744)))
 (let (($x1123 (= c_______ left-to-right)))
 (let (($x1124 (and $x1123 $x1122)))
 (let (($x1125 (= c_______ left-to-center)))
 (let (($x1126 (and $x1125 $x1122)))
 (ite $x1126 $x1129 (ite $x1124 $x1129 (ite $x1121 $x1133 (ite $x1119 $x1142 (ite $x1116 $x1136 $x1142))))))))))))))))))))))))))))))))
(assert
 (let ((?x778 (right s_______)))
 (let ((?x916 (right s________)))
 (let (($x1138 (= ?x916 ?x778)))
 (let ((?x1108 (rest ?x778)))
 (let (($x1135 (= ?x916 ?x1108)))
 (let (($x1112 ((_ is stack ) ?x778)))
 (let (($x1113 (= c_______ right-to-center)))
 (let (($x1114 (and $x1113 $x1112)))
 (let (($x1115 (= c_______ right-to-left)))
 (let (($x1116 (and $x1115 $x1112)))
 (let ((?x964 (center s_______)))
 (let ((?x1105 (top ?x964)))
 (let ((?x1107 (stack ?x1105 ?x778)))
 (let (($x1134 (= ?x916 ?x1107)))
 (let (($x1117 ((_ is stack ) ?x964)))
 (let (($x1118 (= c_______ center-to-right)))
 (let (($x1119 (and $x1118 $x1117)))
 (let (($x1120 (= c_______ center-to-left)))
 (let (($x1121 (and $x1120 $x1117)))
 (let ((?x744 (left s_______)))
 (let (($x1122 ((_ is stack ) ?x744)))
 (let (($x1123 (= c_______ left-to-right)))
 (let (($x1124 (and $x1123 $x1122)))
 (let (($x1155 (ite $x1124 $x1138 (ite $x1121 $x1138 (ite $x1119 $x1134 (ite $x1116 $x1135 (ite $x1114 $x1135 $x1138)))))))
 (let ((?x962 (top ?x744)))
 (let ((?x1101 (stack ?x962 ?x778)))
 (let (($x1130 (= ?x916 ?x1101)))
 (let (($x1125 (= c_______ left-to-center)))
 (let (($x1126 (and $x1125 $x1122)))
 (ite $x1126 $x1130 $x1155)))))))))))))))))))))))))))))))
(assert
 (let ((?x964 (center s_______)))
 (let ((?x1102 (center s________)))
 (let (($x1140 (= ?x1102 ?x964)))
 (let ((?x778 (right s_______)))
 (let ((?x1109 (top ?x778)))
 (let ((?x1111 (stack ?x1109 ?x964)))
 (let (($x1137 (= ?x1102 ?x1111)))
 (let (($x1112 ((_ is stack ) ?x778)))
 (let (($x1113 (= c_______ right-to-center)))
 (let (($x1114 (and $x1113 $x1112)))
 (let (($x1115 (= c_______ right-to-left)))
 (let (($x1116 (and $x1115 $x1112)))
 (let ((?x1104 (rest ?x964)))
 (let (($x1132 (= ?x1102 ?x1104)))
 (let (($x1117 ((_ is stack ) ?x964)))
 (let (($x1118 (= c_______ center-to-right)))
 (let (($x1119 (and $x1118 $x1117)))
 (let (($x1120 (= c_______ center-to-left)))
 (let (($x1121 (and $x1120 $x1117)))
 (let ((?x744 (left s_______)))
 (let ((?x962 (top ?x744)))
 (let ((?x1103 (stack ?x962 ?x964)))
 (let (($x1131 (= ?x1102 ?x1103)))
 (let (($x1122 ((_ is stack ) ?x744)))
 (let (($x1123 (= c_______ left-to-right)))
 (let (($x1124 (and $x1123 $x1122)))
 (let (($x1154 (ite $x1124 $x1131 (ite $x1121 $x1132 (ite $x1119 $x1132 (ite $x1116 $x1140 (ite $x1114 $x1137 $x1140)))))))
 (let (($x1125 (= c_______ left-to-center)))
 (let (($x1126 (and $x1125 $x1122)))
 (ite $x1126 $x1140 $x1154)))))))))))))))))))))))))))))))
(assert
 (= c_________ c!8))
(assert
 (let ((?x882 (left s________)))
 (let ((?x1020 (left s_________)))
 (let (($x1280 (= ?x1020 ?x882)))
 (let ((?x916 (right s________)))
 (let ((?x1247 (top ?x916)))
 (let ((?x1248 (stack ?x1247 ?x882)))
 (let (($x1274 (= ?x1020 ?x1248)))
 (let (($x1250 ((_ is stack ) ?x916)))
 (let (($x1253 (= c________ right-to-left)))
 (let (($x1254 (and $x1253 $x1250)))
 (let ((?x1102 (center s________)))
 (let (($x1255 ((_ is stack ) ?x1102)))
 (let (($x1256 (= c________ center-to-right)))
 (let (($x1257 (and $x1256 $x1255)))
 (let ((?x1243 (top ?x1102)))
 (let ((?x1244 (stack ?x1243 ?x882)))
 (let (($x1271 (= ?x1020 ?x1244)))
 (let (($x1258 (= c________ center-to-left)))
 (let (($x1259 (and $x1258 $x1255)))
 (let ((?x1053 (rest ?x882)))
 (let (($x1267 (= ?x1020 ?x1053)))
 (let (($x1260 ((_ is stack ) ?x882)))
 (let (($x1261 (= c________ left-to-right)))
 (let (($x1262 (and $x1261 $x1260)))
 (let (($x1263 (= c________ left-to-center)))
 (let (($x1264 (and $x1263 $x1260)))
 (ite $x1264 $x1267 (ite $x1262 $x1267 (ite $x1259 $x1271 (ite $x1257 $x1280 (ite $x1254 $x1274 $x1280))))))))))))))))))))))))))))))))
(assert
 (let ((?x916 (right s________)))
 (let ((?x1054 (right s_________)))
 (let (($x1276 (= ?x1054 ?x916)))
 (let ((?x1246 (rest ?x916)))
 (let (($x1273 (= ?x1054 ?x1246)))
 (let (($x1250 ((_ is stack ) ?x916)))
 (let (($x1251 (= c________ right-to-center)))
 (let (($x1252 (and $x1251 $x1250)))
 (let (($x1253 (= c________ right-to-left)))
 (let (($x1254 (and $x1253 $x1250)))
 (let ((?x1102 (center s________)))
 (let ((?x1243 (top ?x1102)))
 (let ((?x1245 (stack ?x1243 ?x916)))
 (let (($x1272 (= ?x1054 ?x1245)))
 (let (($x1255 ((_ is stack ) ?x1102)))
 (let (($x1256 (= c________ center-to-right)))
 (let (($x1257 (and $x1256 $x1255)))
 (let (($x1258 (= c________ center-to-left)))
 (let (($x1259 (and $x1258 $x1255)))
 (let ((?x882 (left s________)))
 (let (($x1260 ((_ is stack ) ?x882)))
 (let (($x1261 (= c________ left-to-right)))
 (let (($x1262 (and $x1261 $x1260)))
 (let (($x1293 (ite $x1262 $x1276 (ite $x1259 $x1276 (ite $x1257 $x1272 (ite $x1254 $x1273 (ite $x1252 $x1273 $x1276)))))))
 (let ((?x1100 (top ?x882)))
 (let ((?x1239 (stack ?x1100 ?x916)))
 (let (($x1268 (= ?x1054 ?x1239)))
 (let (($x1263 (= c________ left-to-center)))
 (let (($x1264 (and $x1263 $x1260)))
 (ite $x1264 $x1268 $x1293)))))))))))))))))))))))))))))))
(assert
 (let ((?x1102 (center s________)))
 (let ((?x1240 (center s_________)))
 (let (($x1278 (= ?x1240 ?x1102)))
 (let ((?x916 (right s________)))
 (let ((?x1247 (top ?x916)))
 (let ((?x1249 (stack ?x1247 ?x1102)))
 (let (($x1275 (= ?x1240 ?x1249)))
 (let (($x1250 ((_ is stack ) ?x916)))
 (let (($x1251 (= c________ right-to-center)))
 (let (($x1252 (and $x1251 $x1250)))
 (let (($x1253 (= c________ right-to-left)))
 (let (($x1254 (and $x1253 $x1250)))
 (let ((?x1242 (rest ?x1102)))
 (let (($x1270 (= ?x1240 ?x1242)))
 (let (($x1255 ((_ is stack ) ?x1102)))
 (let (($x1256 (= c________ center-to-right)))
 (let (($x1257 (and $x1256 $x1255)))
 (let (($x1258 (= c________ center-to-left)))
 (let (($x1259 (and $x1258 $x1255)))
 (let ((?x882 (left s________)))
 (let ((?x1100 (top ?x882)))
 (let ((?x1241 (stack ?x1100 ?x1102)))
 (let (($x1269 (= ?x1240 ?x1241)))
 (let (($x1260 ((_ is stack ) ?x882)))
 (let (($x1261 (= c________ left-to-right)))
 (let (($x1262 (and $x1261 $x1260)))
 (let (($x1292 (ite $x1262 $x1269 (ite $x1259 $x1270 (ite $x1257 $x1270 (ite $x1254 $x1278 (ite $x1252 $x1275 $x1278)))))))
 (let (($x1263 (= c________ left-to-center)))
 (let (($x1264 (and $x1263 $x1260)))
 (ite $x1264 $x1278 $x1292)))))))))))))))))))))))))))))))
(assert
 (= c__________ c!9))
(assert
 (let ((?x1020 (left s_________)))
 (let ((?x1158 (left s__________)))
 (let (($x1418 (= ?x1158 ?x1020)))
 (let ((?x1054 (right s_________)))
 (let ((?x1385 (top ?x1054)))
 (let ((?x1386 (stack ?x1385 ?x1020)))
 (let (($x1412 (= ?x1158 ?x1386)))
 (let (($x1388 ((_ is stack ) ?x1054)))
 (let (($x1391 (= c_________ right-to-left)))
 (let (($x1392 (and $x1391 $x1388)))
 (let ((?x1240 (center s_________)))
 (let (($x1393 ((_ is stack ) ?x1240)))
 (let (($x1394 (= c_________ center-to-right)))
 (let (($x1395 (and $x1394 $x1393)))
 (let ((?x1381 (top ?x1240)))
 (let ((?x1382 (stack ?x1381 ?x1020)))
 (let (($x1409 (= ?x1158 ?x1382)))
 (let (($x1396 (= c_________ center-to-left)))
 (let (($x1397 (and $x1396 $x1393)))
 (let ((?x1191 (rest ?x1020)))
 (let (($x1405 (= ?x1158 ?x1191)))
 (let (($x1398 ((_ is stack ) ?x1020)))
 (let (($x1399 (= c_________ left-to-right)))
 (let (($x1400 (and $x1399 $x1398)))
 (let (($x1401 (= c_________ left-to-center)))
 (let (($x1402 (and $x1401 $x1398)))
 (ite $x1402 $x1405 (ite $x1400 $x1405 (ite $x1397 $x1409 (ite $x1395 $x1418 (ite $x1392 $x1412 $x1418))))))))))))))))))))))))))))))))
(assert
 (let ((?x1054 (right s_________)))
 (let ((?x1192 (right s__________)))
 (let (($x1414 (= ?x1192 ?x1054)))
 (let ((?x1384 (rest ?x1054)))
 (let (($x1411 (= ?x1192 ?x1384)))
 (let (($x1388 ((_ is stack ) ?x1054)))
 (let (($x1389 (= c_________ right-to-center)))
 (let (($x1390 (and $x1389 $x1388)))
 (let (($x1391 (= c_________ right-to-left)))
 (let (($x1392 (and $x1391 $x1388)))
 (let ((?x1240 (center s_________)))
 (let ((?x1381 (top ?x1240)))
 (let ((?x1383 (stack ?x1381 ?x1054)))
 (let (($x1410 (= ?x1192 ?x1383)))
 (let (($x1393 ((_ is stack ) ?x1240)))
 (let (($x1394 (= c_________ center-to-right)))
 (let (($x1395 (and $x1394 $x1393)))
 (let (($x1396 (= c_________ center-to-left)))
 (let (($x1397 (and $x1396 $x1393)))
 (let ((?x1020 (left s_________)))
 (let (($x1398 ((_ is stack ) ?x1020)))
 (let (($x1399 (= c_________ left-to-right)))
 (let (($x1400 (and $x1399 $x1398)))
 (let (($x1431 (ite $x1400 $x1414 (ite $x1397 $x1414 (ite $x1395 $x1410 (ite $x1392 $x1411 (ite $x1390 $x1411 $x1414)))))))
 (let ((?x1238 (top ?x1020)))
 (let ((?x1377 (stack ?x1238 ?x1054)))
 (let (($x1406 (= ?x1192 ?x1377)))
 (let (($x1401 (= c_________ left-to-center)))
 (let (($x1402 (and $x1401 $x1398)))
 (ite $x1402 $x1406 $x1431)))))))))))))))))))))))))))))))
(assert
 (let ((?x1240 (center s_________)))
 (let ((?x1378 (center s__________)))
 (let (($x1416 (= ?x1378 ?x1240)))
 (let ((?x1054 (right s_________)))
 (let ((?x1385 (top ?x1054)))
 (let ((?x1387 (stack ?x1385 ?x1240)))
 (let (($x1413 (= ?x1378 ?x1387)))
 (let (($x1388 ((_ is stack ) ?x1054)))
 (let (($x1389 (= c_________ right-to-center)))
 (let (($x1390 (and $x1389 $x1388)))
 (let (($x1391 (= c_________ right-to-left)))
 (let (($x1392 (and $x1391 $x1388)))
 (let ((?x1380 (rest ?x1240)))
 (let (($x1408 (= ?x1378 ?x1380)))
 (let (($x1393 ((_ is stack ) ?x1240)))
 (let (($x1394 (= c_________ center-to-right)))
 (let (($x1395 (and $x1394 $x1393)))
 (let (($x1396 (= c_________ center-to-left)))
 (let (($x1397 (and $x1396 $x1393)))
 (let ((?x1020 (left s_________)))
 (let ((?x1238 (top ?x1020)))
 (let ((?x1379 (stack ?x1238 ?x1240)))
 (let (($x1407 (= ?x1378 ?x1379)))
 (let (($x1398 ((_ is stack ) ?x1020)))
 (let (($x1399 (= c_________ left-to-right)))
 (let (($x1400 (and $x1399 $x1398)))
 (let (($x1430 (ite $x1400 $x1407 (ite $x1397 $x1408 (ite $x1395 $x1408 (ite $x1392 $x1416 (ite $x1390 $x1413 $x1416)))))))
 (let (($x1401 (= c_________ left-to-center)))
 (let (($x1402 (and $x1401 $x1398)))
 (ite $x1402 $x1416 $x1430)))))))))))))))))))))))))))))))
(assert
 (let ((?x85 (stack X (stack W (stack Q (stack I (stack F empty)))))))
 (let ((?x1192 (right s__________)))
 (let (($x1434 (= ?x1192 ?x85)))
 (let ((?x77 (stack J (stack A (stack G (stack D (stack O (stack Y empty))))))))
 (let ((?x81 (stack S (stack E (stack N (stack R ?x77))))))
 (let ((?x1378 (center s__________)))
 (let (($x1465 (= ?x1378 ?x81)))
 (let ((?x67 (stack L (stack V (stack K (stack C (stack B (stack P empty))))))))
 (let ((?x71 (stack H (stack T (stack M (stack U ?x67))))))
 (let ((?x1158 (left s__________)))
 (let (($x1466 (= ?x1158 ?x71)))
 (let (($x1468 (not (and $x1466 $x1465 $x1434))))
 (not $x1468))))))))))))))
(check-sat)