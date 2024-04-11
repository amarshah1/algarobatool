; 
(set-info :status unknown)
(set-logic QF_DT)
(declare-datatypes ((Enum_A_B_C_D_E_F_G_H_I_J_K_L_M_N_O_P_Q_R_S_T_U_V_W_X 0)) (((A) (B) (C) (D) (E) (F) (G) (H) (I) (J) (K) (L) (M) (N) (O) (P) (Q) (R) (S) (T) (U) (V) (W) (X))))
 (declare-datatypes ((Tower 0)) (((stack (top Enum_A_B_C_D_E_F_G_H_I_J_K_L_M_N_O_P_Q_R_S_T_U_V_W_X) (rest Tower)) (empty))))
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
(assert
 (= s_tmp s))
(assert
 (= c_tmp c))
(assert
 (let ((?x36 (stack X empty)))
 (let ((?x37 (stack L ?x36)))
 (let ((?x38 (stack Q ?x37)))
 (let ((?x39 (stack B ?x38)))
 (let ((?x40 (stack T ?x39)))
 (let ((?x41 (stack E ?x40)))
 (let ((?x42 (stack G ?x41)))
 (let ((?x43 (stack H ?x42)))
 (let ((?x44 (stack S ?x43)))
 (let ((?x45 (stack W ?x44)))
 (let ((?x46 (stack P ?x45)))
 (let ((?x47 (stack D ?x46)))
 (let ((?x48 (stack M ?x47)))
 (let ((?x49 (stack F ?x48)))
 (let ((?x50 (stack K ?x49)))
 (let ((?x51 (stack R ?x50)))
 (let ((?x52 (stack C ?x51)))
 (let ((?x53 (stack A ?x52)))
 (let ((?x54 (stack U ?x53)))
 (let ((?x55 (stack I ?x54)))
 (let ((?x56 (stack O ?x55)))
 (let ((?x57 (stack V ?x56)))
 (let ((?x58 (stack J ?x57)))
 (let ((?x130 (left s_tmp_)))
 (= ?x130 ?x58))))))))))))))))))))))))))
(assert
 (let ((?x59 (stack N empty)))
 (let ((?x133 (center s_tmp__)))
 (= ?x133 ?x59))))
(assert
 (let ((?x136 (right s_tmp___)))
 (= ?x136 empty)))
(assert
 (= c_tmp____ c!0))
(assert
 (= s_ s_tmp___))
(assert
 (= c_ c_tmp____))
(assert
 (let ((?x163 (left s_tmp__)))
 (= (left s_tmp___) ?x163)))
(assert
 (let ((?x133 (center s_tmp__)))
 (= (center s_tmp___) ?x133)))
(assert
 (let ((?x167 (center s_tmp_)))
 (= ?x167 (center s_tmp))))
(assert
 (let ((?x170 (right s_tmp_)))
 (= ?x170 (right s_tmp))))
(assert
 (let ((?x130 (left s_tmp_)))
 (let ((?x163 (left s_tmp__)))
 (= ?x163 ?x130))))
(assert
 (let ((?x170 (right s_tmp_)))
 (let ((?x174 (right s_tmp__)))
 (= ?x174 ?x170))))
(assert
 (= c__ c!1))
(assert
 (let ((?x107 (left s_)))
 (let ((?x308 (left s__)))
 (let (($x335 (= ?x308 ?x107)))
 (let ((?x110 (right s_)))
 (let (($x311 ((_ is stack ) ?x110)))
 (let (($x303 (and (= c_ right-to-left) $x311)))
 (let ((?x112 (center s_)))
 (let (($x304 ((_ is stack ) ?x112)))
 (let (($x305 (= c_ center-to-right)))
 (let (($x297 (and $x305 $x304)))
 (let (($x298 (= c_ center-to-left)))
 (let (($x299 (and $x298 $x304)))
 (let (($x344 (ite $x299 (= ?x308 (stack (top ?x112) ?x107)) (ite $x297 $x335 (ite $x303 (= ?x308 (stack (top ?x110) ?x107)) $x335)))))
 (let (($x322 (= ?x308 (rest ?x107))))
 (let (($x315 ((_ is stack ) ?x107)))
 (let (($x316 (= c_ left-to-right)))
 (let (($x317 (and $x316 $x315)))
 (let (($x318 (= c_ left-to-center)))
 (let (($x319 (and $x318 $x315)))
 (ite $x319 $x322 (ite $x317 $x322 $x344))))))))))))))))))))))
(assert
 (let ((?x110 (right s_)))
 (let ((?x313 (right s__)))
 (let (($x331 (= ?x313 ?x110)))
 (let (($x328 (= ?x313 (rest ?x110))))
 (let (($x311 ((_ is stack ) ?x110)))
 (let (($x301 (and (= c_ right-to-center) $x311)))
 (let (($x303 (and (= c_ right-to-left) $x311)))
 (let ((?x112 (center s_)))
 (let (($x304 ((_ is stack ) ?x112)))
 (let (($x305 (= c_ center-to-right)))
 (let (($x297 (and $x305 $x304)))
 (let (($x341 (ite $x297 (= ?x313 (stack (top ?x112) ?x110)) (ite $x303 $x328 (ite $x301 $x328 $x331)))))
 (let (($x298 (= c_ center-to-left)))
 (let (($x299 (and $x298 $x304)))
 (let ((?x107 (left s_)))
 (let (($x315 ((_ is stack ) ?x107)))
 (let (($x316 (= c_ left-to-right)))
 (let (($x317 (and $x316 $x315)))
 (let (($x318 (= c_ left-to-center)))
 (let (($x319 (and $x318 $x315)))
 (ite $x319 (= ?x313 (stack (top ?x107) ?x110)) (ite $x317 $x331 (ite $x299 $x331 $x341))))))))))))))))))))))))
(assert
 (let ((?x112 (center s_)))
 (let ((?x295 (center s__)))
 (let (($x333 (= ?x295 ?x112)))
 (let ((?x110 (right s_)))
 (let (($x311 ((_ is stack ) ?x110)))
 (let (($x301 (and (= c_ right-to-center) $x311)))
 (let (($x303 (and (= c_ right-to-left) $x311)))
 (let (($x325 (= ?x295 (rest ?x112))))
 (let (($x304 ((_ is stack ) ?x112)))
 (let (($x305 (= c_ center-to-right)))
 (let (($x297 (and $x305 $x304)))
 (let (($x340 (ite $x297 $x325 (ite $x303 $x333 (ite $x301 (= ?x295 (stack (top ?x110) ?x112)) $x333)))))
 (let (($x298 (= c_ center-to-left)))
 (let (($x299 (and $x298 $x304)))
 (let ((?x107 (left s_)))
 (let (($x315 ((_ is stack ) ?x107)))
 (let (($x316 (= c_ left-to-right)))
 (let (($x317 (and $x316 $x315)))
 (let (($x318 (= c_ left-to-center)))
 (let (($x319 (and $x318 $x315)))
 (ite $x319 $x333 (ite $x317 (= ?x295 (stack (top ?x107) ?x112)) (ite $x299 $x325 $x340))))))))))))))))))))))))
(assert
 (= c___ c!2))
(assert
 (let ((?x308 (left s__)))
 (let ((?x177 (left s___)))
 (let (($x442 (= ?x177 ?x308)))
 (let ((?x313 (right s__)))
 (let ((?x409 (top ?x313)))
 (let ((?x410 (stack ?x409 ?x308)))
 (let (($x436 (= ?x177 ?x410)))
 (let (($x412 ((_ is stack ) ?x313)))
 (let (($x415 (= c__ right-to-left)))
 (let (($x416 (and $x415 $x412)))
 (let ((?x295 (center s__)))
 (let (($x417 ((_ is stack ) ?x295)))
 (let (($x418 (= c__ center-to-right)))
 (let (($x419 (and $x418 $x417)))
 (let ((?x405 (top ?x295)))
 (let ((?x406 (stack ?x405 ?x308)))
 (let (($x433 (= ?x177 ?x406)))
 (let (($x420 (= c__ center-to-left)))
 (let (($x421 (and $x420 $x417)))
 (let ((?x179 (rest ?x308)))
 (let (($x429 (= ?x177 ?x179)))
 (let (($x422 ((_ is stack ) ?x308)))
 (let (($x423 (= c__ left-to-right)))
 (let (($x424 (and $x423 $x422)))
 (let (($x425 (= c__ left-to-center)))
 (let (($x426 (and $x425 $x422)))
 (ite $x426 $x429 (ite $x424 $x429 (ite $x421 $x433 (ite $x419 $x442 (ite $x416 $x436 $x442))))))))))))))))))))))))))))))))
(assert
 (let ((?x313 (right s__)))
 (let ((?x180 (right s___)))
 (let (($x438 (= ?x180 ?x313)))
 (let ((?x408 (rest ?x313)))
 (let (($x435 (= ?x180 ?x408)))
 (let (($x412 ((_ is stack ) ?x313)))
 (let (($x413 (= c__ right-to-center)))
 (let (($x414 (and $x413 $x412)))
 (let (($x415 (= c__ right-to-left)))
 (let (($x416 (and $x415 $x412)))
 (let ((?x295 (center s__)))
 (let ((?x405 (top ?x295)))
 (let ((?x407 (stack ?x405 ?x313)))
 (let (($x434 (= ?x180 ?x407)))
 (let (($x417 ((_ is stack ) ?x295)))
 (let (($x418 (= c__ center-to-right)))
 (let (($x419 (and $x418 $x417)))
 (let (($x420 (= c__ center-to-left)))
 (let (($x421 (and $x420 $x417)))
 (let ((?x308 (left s__)))
 (let (($x422 ((_ is stack ) ?x308)))
 (let (($x423 (= c__ left-to-right)))
 (let (($x424 (and $x423 $x422)))
 (let (($x455 (ite $x424 $x438 (ite $x421 $x438 (ite $x419 $x434 (ite $x416 $x435 (ite $x414 $x435 $x438)))))))
 (let ((?x287 (top ?x308)))
 (let ((?x401 (stack ?x287 ?x313)))
 (let (($x430 (= ?x180 ?x401)))
 (let (($x425 (= c__ left-to-center)))
 (let (($x426 (and $x425 $x422)))
 (ite $x426 $x430 $x455)))))))))))))))))))))))))))))))
(assert
 (let ((?x295 (center s__)))
 (let ((?x402 (center s___)))
 (let (($x440 (= ?x402 ?x295)))
 (let ((?x313 (right s__)))
 (let ((?x409 (top ?x313)))
 (let ((?x411 (stack ?x409 ?x295)))
 (let (($x437 (= ?x402 ?x411)))
 (let (($x412 ((_ is stack ) ?x313)))
 (let (($x413 (= c__ right-to-center)))
 (let (($x414 (and $x413 $x412)))
 (let (($x415 (= c__ right-to-left)))
 (let (($x416 (and $x415 $x412)))
 (let ((?x404 (rest ?x295)))
 (let (($x432 (= ?x402 ?x404)))
 (let (($x417 ((_ is stack ) ?x295)))
 (let (($x418 (= c__ center-to-right)))
 (let (($x419 (and $x418 $x417)))
 (let (($x420 (= c__ center-to-left)))
 (let (($x421 (and $x420 $x417)))
 (let ((?x308 (left s__)))
 (let ((?x287 (top ?x308)))
 (let ((?x403 (stack ?x287 ?x295)))
 (let (($x431 (= ?x402 ?x403)))
 (let (($x422 ((_ is stack ) ?x308)))
 (let (($x423 (= c__ left-to-right)))
 (let (($x424 (and $x423 $x422)))
 (let (($x454 (ite $x424 $x431 (ite $x421 $x432 (ite $x419 $x432 (ite $x416 $x440 (ite $x414 $x437 $x440)))))))
 (let (($x425 (= c__ left-to-center)))
 (let (($x426 (and $x425 $x422)))
 (ite $x426 $x440 $x454)))))))))))))))))))))))))))))))
(assert
 (= c____ c!3))
(assert
 (let ((?x177 (left s___)))
 (let ((?x351 (left s____)))
 (let (($x580 (= ?x351 ?x177)))
 (let ((?x180 (right s___)))
 (let ((?x547 (top ?x180)))
 (let ((?x548 (stack ?x547 ?x177)))
 (let (($x574 (= ?x351 ?x548)))
 (let (($x550 ((_ is stack ) ?x180)))
 (let (($x553 (= c___ right-to-left)))
 (let (($x554 (and $x553 $x550)))
 (let ((?x402 (center s___)))
 (let (($x555 ((_ is stack ) ?x402)))
 (let (($x556 (= c___ center-to-right)))
 (let (($x557 (and $x556 $x555)))
 (let ((?x543 (top ?x402)))
 (let ((?x544 (stack ?x543 ?x177)))
 (let (($x571 (= ?x351 ?x544)))
 (let (($x558 (= c___ center-to-left)))
 (let (($x559 (and $x558 $x555)))
 (let ((?x381 (rest ?x177)))
 (let (($x567 (= ?x351 ?x381)))
 (let (($x560 ((_ is stack ) ?x177)))
 (let (($x561 (= c___ left-to-right)))
 (let (($x562 (and $x561 $x560)))
 (let (($x563 (= c___ left-to-center)))
 (let (($x564 (and $x563 $x560)))
 (ite $x564 $x567 (ite $x562 $x567 (ite $x559 $x571 (ite $x557 $x580 (ite $x554 $x574 $x580))))))))))))))))))))))))))))))))
(assert
 (let ((?x180 (right s___)))
 (let ((?x382 (right s____)))
 (let (($x576 (= ?x382 ?x180)))
 (let ((?x546 (rest ?x180)))
 (let (($x573 (= ?x382 ?x546)))
 (let (($x550 ((_ is stack ) ?x180)))
 (let (($x551 (= c___ right-to-center)))
 (let (($x552 (and $x551 $x550)))
 (let (($x553 (= c___ right-to-left)))
 (let (($x554 (and $x553 $x550)))
 (let ((?x402 (center s___)))
 (let ((?x543 (top ?x402)))
 (let ((?x545 (stack ?x543 ?x180)))
 (let (($x572 (= ?x382 ?x545)))
 (let (($x555 ((_ is stack ) ?x402)))
 (let (($x556 (= c___ center-to-right)))
 (let (($x557 (and $x556 $x555)))
 (let (($x558 (= c___ center-to-left)))
 (let (($x559 (and $x558 $x555)))
 (let ((?x177 (left s___)))
 (let (($x560 ((_ is stack ) ?x177)))
 (let (($x561 (= c___ left-to-right)))
 (let (($x562 (and $x561 $x560)))
 (let (($x593 (ite $x562 $x576 (ite $x559 $x576 (ite $x557 $x572 (ite $x554 $x573 (ite $x552 $x573 $x576)))))))
 (let ((?x400 (top ?x177)))
 (let ((?x539 (stack ?x400 ?x180)))
 (let (($x568 (= ?x382 ?x539)))
 (let (($x563 (= c___ left-to-center)))
 (let (($x564 (and $x563 $x560)))
 (ite $x564 $x568 $x593)))))))))))))))))))))))))))))))
(assert
 (let ((?x402 (center s___)))
 (let ((?x540 (center s____)))
 (let (($x578 (= ?x540 ?x402)))
 (let ((?x180 (right s___)))
 (let ((?x547 (top ?x180)))
 (let ((?x549 (stack ?x547 ?x402)))
 (let (($x575 (= ?x540 ?x549)))
 (let (($x550 ((_ is stack ) ?x180)))
 (let (($x551 (= c___ right-to-center)))
 (let (($x552 (and $x551 $x550)))
 (let (($x553 (= c___ right-to-left)))
 (let (($x554 (and $x553 $x550)))
 (let ((?x542 (rest ?x402)))
 (let (($x570 (= ?x540 ?x542)))
 (let (($x555 ((_ is stack ) ?x402)))
 (let (($x556 (= c___ center-to-right)))
 (let (($x557 (and $x556 $x555)))
 (let (($x558 (= c___ center-to-left)))
 (let (($x559 (and $x558 $x555)))
 (let ((?x177 (left s___)))
 (let ((?x400 (top ?x177)))
 (let ((?x541 (stack ?x400 ?x402)))
 (let (($x569 (= ?x540 ?x541)))
 (let (($x560 ((_ is stack ) ?x177)))
 (let (($x561 (= c___ left-to-right)))
 (let (($x562 (and $x561 $x560)))
 (let (($x592 (ite $x562 $x569 (ite $x559 $x570 (ite $x557 $x570 (ite $x554 $x578 (ite $x552 $x575 $x578)))))))
 (let (($x563 (= c___ left-to-center)))
 (let (($x564 (and $x563 $x560)))
 (ite $x564 $x578 $x592)))))))))))))))))))))))))))))))
(assert
 (= c_____ c!4))
(assert
 (let ((?x351 (left s____)))
 (let ((?x458 (left s_____)))
 (let (($x718 (= ?x458 ?x351)))
 (let ((?x382 (right s____)))
 (let ((?x685 (top ?x382)))
 (let ((?x686 (stack ?x685 ?x351)))
 (let (($x712 (= ?x458 ?x686)))
 (let (($x688 ((_ is stack ) ?x382)))
 (let (($x691 (= c____ right-to-left)))
 (let (($x692 (and $x691 $x688)))
 (let ((?x540 (center s____)))
 (let (($x693 ((_ is stack ) ?x540)))
 (let (($x694 (= c____ center-to-right)))
 (let (($x695 (and $x694 $x693)))
 (let ((?x681 (top ?x540)))
 (let ((?x682 (stack ?x681 ?x351)))
 (let (($x709 (= ?x458 ?x682)))
 (let (($x696 (= c____ center-to-left)))
 (let (($x697 (and $x696 $x693)))
 (let ((?x491 (rest ?x351)))
 (let (($x705 (= ?x458 ?x491)))
 (let (($x698 ((_ is stack ) ?x351)))
 (let (($x699 (= c____ left-to-right)))
 (let (($x700 (and $x699 $x698)))
 (let (($x701 (= c____ left-to-center)))
 (let (($x702 (and $x701 $x698)))
 (ite $x702 $x705 (ite $x700 $x705 (ite $x697 $x709 (ite $x695 $x718 (ite $x692 $x712 $x718))))))))))))))))))))))))))))))))
(assert
 (let ((?x382 (right s____)))
 (let ((?x492 (right s_____)))
 (let (($x714 (= ?x492 ?x382)))
 (let ((?x684 (rest ?x382)))
 (let (($x711 (= ?x492 ?x684)))
 (let (($x688 ((_ is stack ) ?x382)))
 (let (($x689 (= c____ right-to-center)))
 (let (($x690 (and $x689 $x688)))
 (let (($x691 (= c____ right-to-left)))
 (let (($x692 (and $x691 $x688)))
 (let ((?x540 (center s____)))
 (let ((?x681 (top ?x540)))
 (let ((?x683 (stack ?x681 ?x382)))
 (let (($x710 (= ?x492 ?x683)))
 (let (($x693 ((_ is stack ) ?x540)))
 (let (($x694 (= c____ center-to-right)))
 (let (($x695 (and $x694 $x693)))
 (let (($x696 (= c____ center-to-left)))
 (let (($x697 (and $x696 $x693)))
 (let ((?x351 (left s____)))
 (let (($x698 ((_ is stack ) ?x351)))
 (let (($x699 (= c____ left-to-right)))
 (let (($x700 (and $x699 $x698)))
 (let (($x731 (ite $x700 $x714 (ite $x697 $x714 (ite $x695 $x710 (ite $x692 $x711 (ite $x690 $x711 $x714)))))))
 (let ((?x538 (top ?x351)))
 (let ((?x677 (stack ?x538 ?x382)))
 (let (($x706 (= ?x492 ?x677)))
 (let (($x701 (= c____ left-to-center)))
 (let (($x702 (and $x701 $x698)))
 (ite $x702 $x706 $x731)))))))))))))))))))))))))))))))
(assert
 (let ((?x540 (center s____)))
 (let ((?x678 (center s_____)))
 (let (($x716 (= ?x678 ?x540)))
 (let ((?x382 (right s____)))
 (let ((?x685 (top ?x382)))
 (let ((?x687 (stack ?x685 ?x540)))
 (let (($x713 (= ?x678 ?x687)))
 (let (($x688 ((_ is stack ) ?x382)))
 (let (($x689 (= c____ right-to-center)))
 (let (($x690 (and $x689 $x688)))
 (let (($x691 (= c____ right-to-left)))
 (let (($x692 (and $x691 $x688)))
 (let ((?x680 (rest ?x540)))
 (let (($x708 (= ?x678 ?x680)))
 (let (($x693 ((_ is stack ) ?x540)))
 (let (($x694 (= c____ center-to-right)))
 (let (($x695 (and $x694 $x693)))
 (let (($x696 (= c____ center-to-left)))
 (let (($x697 (and $x696 $x693)))
 (let ((?x351 (left s____)))
 (let ((?x538 (top ?x351)))
 (let ((?x679 (stack ?x538 ?x540)))
 (let (($x707 (= ?x678 ?x679)))
 (let (($x698 ((_ is stack ) ?x351)))
 (let (($x699 (= c____ left-to-right)))
 (let (($x700 (and $x699 $x698)))
 (let (($x730 (ite $x700 $x707 (ite $x697 $x708 (ite $x695 $x708 (ite $x692 $x716 (ite $x690 $x713 $x716)))))))
 (let (($x701 (= c____ left-to-center)))
 (let (($x702 (and $x701 $x698)))
 (ite $x702 $x716 $x730)))))))))))))))))))))))))))))))
(assert
 (= c______ c!5))
(assert
 (let ((?x458 (left s_____)))
 (let ((?x596 (left s______)))
 (let (($x856 (= ?x596 ?x458)))
 (let ((?x492 (right s_____)))
 (let ((?x823 (top ?x492)))
 (let ((?x824 (stack ?x823 ?x458)))
 (let (($x850 (= ?x596 ?x824)))
 (let (($x826 ((_ is stack ) ?x492)))
 (let (($x829 (= c_____ right-to-left)))
 (let (($x830 (and $x829 $x826)))
 (let ((?x678 (center s_____)))
 (let (($x831 ((_ is stack ) ?x678)))
 (let (($x832 (= c_____ center-to-right)))
 (let (($x833 (and $x832 $x831)))
 (let ((?x819 (top ?x678)))
 (let ((?x820 (stack ?x819 ?x458)))
 (let (($x847 (= ?x596 ?x820)))
 (let (($x834 (= c_____ center-to-left)))
 (let (($x835 (and $x834 $x831)))
 (let ((?x629 (rest ?x458)))
 (let (($x843 (= ?x596 ?x629)))
 (let (($x836 ((_ is stack ) ?x458)))
 (let (($x837 (= c_____ left-to-right)))
 (let (($x838 (and $x837 $x836)))
 (let (($x839 (= c_____ left-to-center)))
 (let (($x840 (and $x839 $x836)))
 (ite $x840 $x843 (ite $x838 $x843 (ite $x835 $x847 (ite $x833 $x856 (ite $x830 $x850 $x856))))))))))))))))))))))))))))))))
(assert
 (let ((?x492 (right s_____)))
 (let ((?x630 (right s______)))
 (let (($x852 (= ?x630 ?x492)))
 (let ((?x822 (rest ?x492)))
 (let (($x849 (= ?x630 ?x822)))
 (let (($x826 ((_ is stack ) ?x492)))
 (let (($x827 (= c_____ right-to-center)))
 (let (($x828 (and $x827 $x826)))
 (let (($x829 (= c_____ right-to-left)))
 (let (($x830 (and $x829 $x826)))
 (let ((?x678 (center s_____)))
 (let ((?x819 (top ?x678)))
 (let ((?x821 (stack ?x819 ?x492)))
 (let (($x848 (= ?x630 ?x821)))
 (let (($x831 ((_ is stack ) ?x678)))
 (let (($x832 (= c_____ center-to-right)))
 (let (($x833 (and $x832 $x831)))
 (let (($x834 (= c_____ center-to-left)))
 (let (($x835 (and $x834 $x831)))
 (let ((?x458 (left s_____)))
 (let (($x836 ((_ is stack ) ?x458)))
 (let (($x837 (= c_____ left-to-right)))
 (let (($x838 (and $x837 $x836)))
 (let (($x869 (ite $x838 $x852 (ite $x835 $x852 (ite $x833 $x848 (ite $x830 $x849 (ite $x828 $x849 $x852)))))))
 (let ((?x676 (top ?x458)))
 (let ((?x815 (stack ?x676 ?x492)))
 (let (($x844 (= ?x630 ?x815)))
 (let (($x839 (= c_____ left-to-center)))
 (let (($x840 (and $x839 $x836)))
 (ite $x840 $x844 $x869)))))))))))))))))))))))))))))))
(assert
 (let ((?x678 (center s_____)))
 (let ((?x816 (center s______)))
 (let (($x854 (= ?x816 ?x678)))
 (let ((?x492 (right s_____)))
 (let ((?x823 (top ?x492)))
 (let ((?x825 (stack ?x823 ?x678)))
 (let (($x851 (= ?x816 ?x825)))
 (let (($x826 ((_ is stack ) ?x492)))
 (let (($x827 (= c_____ right-to-center)))
 (let (($x828 (and $x827 $x826)))
 (let (($x829 (= c_____ right-to-left)))
 (let (($x830 (and $x829 $x826)))
 (let ((?x818 (rest ?x678)))
 (let (($x846 (= ?x816 ?x818)))
 (let (($x831 ((_ is stack ) ?x678)))
 (let (($x832 (= c_____ center-to-right)))
 (let (($x833 (and $x832 $x831)))
 (let (($x834 (= c_____ center-to-left)))
 (let (($x835 (and $x834 $x831)))
 (let ((?x458 (left s_____)))
 (let ((?x676 (top ?x458)))
 (let ((?x817 (stack ?x676 ?x678)))
 (let (($x845 (= ?x816 ?x817)))
 (let (($x836 ((_ is stack ) ?x458)))
 (let (($x837 (= c_____ left-to-right)))
 (let (($x838 (and $x837 $x836)))
 (let (($x868 (ite $x838 $x845 (ite $x835 $x846 (ite $x833 $x846 (ite $x830 $x854 (ite $x828 $x851 $x854)))))))
 (let (($x839 (= c_____ left-to-center)))
 (let (($x840 (and $x839 $x836)))
 (ite $x840 $x854 $x868)))))))))))))))))))))))))))))))
(assert
 (let ((?x82 (stack V (stack O (stack K (stack J (stack C (stack A empty))))))))
 (let ((?x83 (stack X ?x82)))
 (let ((?x630 (right s______)))
 (let (($x872 (= ?x630 ?x83)))
 (let ((?x73 (stack M (stack L (stack D (stack S (stack G (stack P empty))))))))
 (let ((?x76 (stack U (stack T (stack E ?x73)))))
 (let ((?x816 (center s______)))
 (let (($x903 (= ?x816 ?x76)))
 (let ((?x65 (stack N (stack H (stack R (stack Q (stack I (stack W empty))))))))
 (let ((?x67 (stack F (stack B ?x65))))
 (let ((?x596 (left s______)))
 (let (($x904 (= ?x596 ?x67)))
 (let (($x906 (not (and $x904 $x903 $x872))))
 (not $x906)))))))))))))))
(check-sat)