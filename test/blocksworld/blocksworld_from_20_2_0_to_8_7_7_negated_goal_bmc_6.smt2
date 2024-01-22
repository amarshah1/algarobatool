; 
(set-info :status unknown)
(set-logic QF_DT)
(declare-datatypes ((Enum_A_B_C_D_E_F_G_H_I_J_K_L_M_N_O_P_Q_R_S_T_U_V 0)) (((A) (B) (C) (D) (E) (F) (G) (H) (I) (J) (K) (L) (M) (N) (O) (P) (Q) (R) (S) (T) (U) (V))))
 (declare-datatypes ((Tower 0)) (((stack (top Enum_A_B_C_D_E_F_G_H_I_J_K_L_M_N_O_P_Q_R_S_T_U_V) (rest Tower)) (empty))))
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
(assert
 (= s_tmp s))
(assert
 (= c_tmp c))
(assert
 (let ((?x35 (stack S empty)))
 (let ((?x34 (stack K ?x35)))
 (let ((?x36 (stack J ?x34)))
 (let ((?x37 (stack P ?x36)))
 (let ((?x38 (stack C ?x37)))
 (let ((?x39 (stack I ?x38)))
 (let ((?x40 (stack M ?x39)))
 (let ((?x41 (stack G ?x40)))
 (let ((?x42 (stack D ?x41)))
 (let ((?x43 (stack F ?x42)))
 (let ((?x44 (stack O ?x43)))
 (let ((?x45 (stack T ?x44)))
 (let ((?x46 (stack U ?x45)))
 (let ((?x47 (stack H ?x46)))
 (let ((?x48 (stack E ?x47)))
 (let ((?x49 (stack A ?x48)))
 (let ((?x50 (stack B ?x49)))
 (let ((?x51 (stack R ?x50)))
 (let ((?x52 (stack L ?x51)))
 (let ((?x53 (stack Q ?x52)))
 (let ((?x124 (left s_tmp_)))
 (= ?x124 ?x53)))))))))))))))))))))))
(assert
 (let ((?x54 (stack N empty)))
 (let ((?x55 (stack V ?x54)))
 (let ((?x127 (center s_tmp__)))
 (= ?x127 ?x55)))))
(assert
 (let ((?x130 (right s_tmp___)))
 (= ?x130 empty)))
(assert
 (= c_tmp____ c!0))
(assert
 (= s_ s_tmp___))
(assert
 (= c_ c_tmp____))
(assert
 (let ((?x157 (left s_tmp__)))
 (= (left s_tmp___) ?x157)))
(assert
 (let ((?x127 (center s_tmp__)))
 (= (center s_tmp___) ?x127)))
(assert
 (let ((?x161 (center s_tmp_)))
 (= ?x161 (center s_tmp))))
(assert
 (let ((?x164 (right s_tmp_)))
 (= ?x164 (right s_tmp))))
(assert
 (let ((?x124 (left s_tmp_)))
 (let ((?x157 (left s_tmp__)))
 (= ?x157 ?x124))))
(assert
 (let ((?x164 (right s_tmp_)))
 (let ((?x168 (right s_tmp__)))
 (= ?x168 ?x164))))
(assert
 (= c__ c!1))
(assert
 (let ((?x101 (left s_)))
 (let ((?x305 (left s__)))
 (let (($x323 (= ?x305 ?x101)))
 (let ((?x104 (right s_)))
 (let (($x302 ((_ is stack ) ?x104)))
 (let (($x288 (and (= c_ right-to-left) $x302)))
 (let ((?x106 (center s_)))
 (let (($x289 ((_ is stack ) ?x106)))
 (let (($x290 (= c_ center-to-right)))
 (let (($x276 (and $x290 $x289)))
 (let (($x277 (= c_ center-to-left)))
 (let (($x278 (and $x277 $x289)))
 (let (($x332 (ite $x278 (= ?x305 (stack (top ?x106) ?x101)) (ite $x276 $x323 (ite $x288 (= ?x305 (stack (top ?x104) ?x101)) $x323)))))
 (let (($x310 (= ?x305 (rest ?x101))))
 (let (($x291 ((_ is stack ) ?x101)))
 (let (($x292 (= c_ left-to-right)))
 (let (($x293 (and $x292 $x291)))
 (let (($x306 (= c_ left-to-center)))
 (let (($x307 (and $x306 $x291)))
 (ite $x307 $x310 (ite $x293 $x310 $x332))))))))))))))))))))))
(assert
 (let ((?x104 (right s_)))
 (let ((?x295 (right s__)))
 (let (($x319 (= ?x295 ?x104)))
 (let (($x316 (= ?x295 (rest ?x104))))
 (let (($x302 ((_ is stack ) ?x104)))
 (let (($x286 (and (= c_ right-to-center) $x302)))
 (let (($x288 (and (= c_ right-to-left) $x302)))
 (let ((?x106 (center s_)))
 (let (($x289 ((_ is stack ) ?x106)))
 (let (($x290 (= c_ center-to-right)))
 (let (($x276 (and $x290 $x289)))
 (let (($x329 (ite $x276 (= ?x295 (stack (top ?x106) ?x104)) (ite $x288 $x316 (ite $x286 $x316 $x319)))))
 (let (($x277 (= c_ center-to-left)))
 (let (($x278 (and $x277 $x289)))
 (let ((?x101 (left s_)))
 (let (($x291 ((_ is stack ) ?x101)))
 (let (($x292 (= c_ left-to-right)))
 (let (($x293 (and $x292 $x291)))
 (let (($x306 (= c_ left-to-center)))
 (let (($x307 (and $x306 $x291)))
 (ite $x307 (= ?x295 (stack (top ?x101) ?x104)) (ite $x293 $x319 (ite $x278 $x319 $x329))))))))))))))))))))))))
(assert
 (let ((?x106 (center s_)))
 (let ((?x280 (center s__)))
 (let (($x321 (= ?x280 ?x106)))
 (let ((?x104 (right s_)))
 (let (($x302 ((_ is stack ) ?x104)))
 (let (($x286 (and (= c_ right-to-center) $x302)))
 (let (($x288 (and (= c_ right-to-left) $x302)))
 (let (($x313 (= ?x280 (rest ?x106))))
 (let (($x289 ((_ is stack ) ?x106)))
 (let (($x290 (= c_ center-to-right)))
 (let (($x276 (and $x290 $x289)))
 (let (($x328 (ite $x276 $x313 (ite $x288 $x321 (ite $x286 (= ?x280 (stack (top ?x104) ?x106)) $x321)))))
 (let (($x277 (= c_ center-to-left)))
 (let (($x278 (and $x277 $x289)))
 (let ((?x101 (left s_)))
 (let (($x291 ((_ is stack ) ?x101)))
 (let (($x292 (= c_ left-to-right)))
 (let (($x293 (and $x292 $x291)))
 (let (($x306 (= c_ left-to-center)))
 (let (($x307 (and $x306 $x291)))
 (ite $x307 $x321 (ite $x293 (= ?x280 (stack (top ?x101) ?x106)) (ite $x278 $x313 $x328))))))))))))))))))))))))
(assert
 (= c___ c!2))
(assert
 (let ((?x305 (left s__)))
 (let ((?x171 (left s___)))
 (let (($x434 (= ?x171 ?x305)))
 (let ((?x295 (right s__)))
 (let ((?x401 (top ?x295)))
 (let ((?x402 (stack ?x401 ?x305)))
 (let (($x428 (= ?x171 ?x402)))
 (let (($x404 ((_ is stack ) ?x295)))
 (let (($x407 (= c__ right-to-left)))
 (let (($x408 (and $x407 $x404)))
 (let ((?x280 (center s__)))
 (let (($x409 ((_ is stack ) ?x280)))
 (let (($x410 (= c__ center-to-right)))
 (let (($x411 (and $x410 $x409)))
 (let ((?x397 (top ?x280)))
 (let ((?x398 (stack ?x397 ?x305)))
 (let (($x425 (= ?x171 ?x398)))
 (let (($x412 (= c__ center-to-left)))
 (let (($x413 (and $x412 $x409)))
 (let ((?x173 (rest ?x305)))
 (let (($x421 (= ?x171 ?x173)))
 (let (($x414 ((_ is stack ) ?x305)))
 (let (($x415 (= c__ left-to-right)))
 (let (($x416 (and $x415 $x414)))
 (let (($x417 (= c__ left-to-center)))
 (let (($x418 (and $x417 $x414)))
 (ite $x418 $x421 (ite $x416 $x421 (ite $x413 $x425 (ite $x411 $x434 (ite $x408 $x428 $x434))))))))))))))))))))))))))))))))
(assert
 (let ((?x295 (right s__)))
 (let ((?x174 (right s___)))
 (let (($x430 (= ?x174 ?x295)))
 (let ((?x400 (rest ?x295)))
 (let (($x427 (= ?x174 ?x400)))
 (let (($x404 ((_ is stack ) ?x295)))
 (let (($x405 (= c__ right-to-center)))
 (let (($x406 (and $x405 $x404)))
 (let (($x407 (= c__ right-to-left)))
 (let (($x408 (and $x407 $x404)))
 (let ((?x280 (center s__)))
 (let ((?x397 (top ?x280)))
 (let ((?x399 (stack ?x397 ?x295)))
 (let (($x426 (= ?x174 ?x399)))
 (let (($x409 ((_ is stack ) ?x280)))
 (let (($x410 (= c__ center-to-right)))
 (let (($x411 (and $x410 $x409)))
 (let (($x412 (= c__ center-to-left)))
 (let (($x413 (and $x412 $x409)))
 (let ((?x305 (left s__)))
 (let (($x414 ((_ is stack ) ?x305)))
 (let (($x415 (= c__ left-to-right)))
 (let (($x416 (and $x415 $x414)))
 (let (($x447 (ite $x416 $x430 (ite $x413 $x430 (ite $x411 $x426 (ite $x408 $x427 (ite $x406 $x427 $x430)))))))
 (let ((?x275 (top ?x305)))
 (let ((?x393 (stack ?x275 ?x295)))
 (let (($x422 (= ?x174 ?x393)))
 (let (($x417 (= c__ left-to-center)))
 (let (($x418 (and $x417 $x414)))
 (ite $x418 $x422 $x447)))))))))))))))))))))))))))))))
(assert
 (let ((?x280 (center s__)))
 (let ((?x394 (center s___)))
 (let (($x432 (= ?x394 ?x280)))
 (let ((?x295 (right s__)))
 (let ((?x401 (top ?x295)))
 (let ((?x403 (stack ?x401 ?x280)))
 (let (($x429 (= ?x394 ?x403)))
 (let (($x404 ((_ is stack ) ?x295)))
 (let (($x405 (= c__ right-to-center)))
 (let (($x406 (and $x405 $x404)))
 (let (($x407 (= c__ right-to-left)))
 (let (($x408 (and $x407 $x404)))
 (let ((?x396 (rest ?x280)))
 (let (($x424 (= ?x394 ?x396)))
 (let (($x409 ((_ is stack ) ?x280)))
 (let (($x410 (= c__ center-to-right)))
 (let (($x411 (and $x410 $x409)))
 (let (($x412 (= c__ center-to-left)))
 (let (($x413 (and $x412 $x409)))
 (let ((?x305 (left s__)))
 (let ((?x275 (top ?x305)))
 (let ((?x395 (stack ?x275 ?x280)))
 (let (($x423 (= ?x394 ?x395)))
 (let (($x414 ((_ is stack ) ?x305)))
 (let (($x415 (= c__ left-to-right)))
 (let (($x416 (and $x415 $x414)))
 (let (($x446 (ite $x416 $x423 (ite $x413 $x424 (ite $x411 $x424 (ite $x408 $x432 (ite $x406 $x429 $x432)))))))
 (let (($x417 (= c__ left-to-center)))
 (let (($x418 (and $x417 $x414)))
 (ite $x418 $x432 $x446)))))))))))))))))))))))))))))))
(assert
 (= c____ c!3))
(assert
 (let ((?x171 (left s___)))
 (let ((?x339 (left s____)))
 (let (($x572 (= ?x339 ?x171)))
 (let ((?x174 (right s___)))
 (let ((?x539 (top ?x174)))
 (let ((?x540 (stack ?x539 ?x171)))
 (let (($x566 (= ?x339 ?x540)))
 (let (($x542 ((_ is stack ) ?x174)))
 (let (($x545 (= c___ right-to-left)))
 (let (($x546 (and $x545 $x542)))
 (let ((?x394 (center s___)))
 (let (($x547 ((_ is stack ) ?x394)))
 (let (($x548 (= c___ center-to-right)))
 (let (($x549 (and $x548 $x547)))
 (let ((?x535 (top ?x394)))
 (let ((?x536 (stack ?x535 ?x171)))
 (let (($x563 (= ?x339 ?x536)))
 (let (($x550 (= c___ center-to-left)))
 (let (($x551 (and $x550 $x547)))
 (let ((?x370 (rest ?x171)))
 (let (($x559 (= ?x339 ?x370)))
 (let (($x552 ((_ is stack ) ?x171)))
 (let (($x553 (= c___ left-to-right)))
 (let (($x554 (and $x553 $x552)))
 (let (($x555 (= c___ left-to-center)))
 (let (($x556 (and $x555 $x552)))
 (ite $x556 $x559 (ite $x554 $x559 (ite $x551 $x563 (ite $x549 $x572 (ite $x546 $x566 $x572))))))))))))))))))))))))))))))))
(assert
 (let ((?x174 (right s___)))
 (let ((?x371 (right s____)))
 (let (($x568 (= ?x371 ?x174)))
 (let ((?x538 (rest ?x174)))
 (let (($x565 (= ?x371 ?x538)))
 (let (($x542 ((_ is stack ) ?x174)))
 (let (($x543 (= c___ right-to-center)))
 (let (($x544 (and $x543 $x542)))
 (let (($x545 (= c___ right-to-left)))
 (let (($x546 (and $x545 $x542)))
 (let ((?x394 (center s___)))
 (let ((?x535 (top ?x394)))
 (let ((?x537 (stack ?x535 ?x174)))
 (let (($x564 (= ?x371 ?x537)))
 (let (($x547 ((_ is stack ) ?x394)))
 (let (($x548 (= c___ center-to-right)))
 (let (($x549 (and $x548 $x547)))
 (let (($x550 (= c___ center-to-left)))
 (let (($x551 (and $x550 $x547)))
 (let ((?x171 (left s___)))
 (let (($x552 ((_ is stack ) ?x171)))
 (let (($x553 (= c___ left-to-right)))
 (let (($x554 (and $x553 $x552)))
 (let (($x585 (ite $x554 $x568 (ite $x551 $x568 (ite $x549 $x564 (ite $x546 $x565 (ite $x544 $x565 $x568)))))))
 (let ((?x392 (top ?x171)))
 (let ((?x531 (stack ?x392 ?x174)))
 (let (($x560 (= ?x371 ?x531)))
 (let (($x555 (= c___ left-to-center)))
 (let (($x556 (and $x555 $x552)))
 (ite $x556 $x560 $x585)))))))))))))))))))))))))))))))
(assert
 (let ((?x394 (center s___)))
 (let ((?x532 (center s____)))
 (let (($x570 (= ?x532 ?x394)))
 (let ((?x174 (right s___)))
 (let ((?x539 (top ?x174)))
 (let ((?x541 (stack ?x539 ?x394)))
 (let (($x567 (= ?x532 ?x541)))
 (let (($x542 ((_ is stack ) ?x174)))
 (let (($x543 (= c___ right-to-center)))
 (let (($x544 (and $x543 $x542)))
 (let (($x545 (= c___ right-to-left)))
 (let (($x546 (and $x545 $x542)))
 (let ((?x534 (rest ?x394)))
 (let (($x562 (= ?x532 ?x534)))
 (let (($x547 ((_ is stack ) ?x394)))
 (let (($x548 (= c___ center-to-right)))
 (let (($x549 (and $x548 $x547)))
 (let (($x550 (= c___ center-to-left)))
 (let (($x551 (and $x550 $x547)))
 (let ((?x171 (left s___)))
 (let ((?x392 (top ?x171)))
 (let ((?x533 (stack ?x392 ?x394)))
 (let (($x561 (= ?x532 ?x533)))
 (let (($x552 ((_ is stack ) ?x171)))
 (let (($x553 (= c___ left-to-right)))
 (let (($x554 (and $x553 $x552)))
 (let (($x584 (ite $x554 $x561 (ite $x551 $x562 (ite $x549 $x562 (ite $x546 $x570 (ite $x544 $x567 $x570)))))))
 (let (($x555 (= c___ left-to-center)))
 (let (($x556 (and $x555 $x552)))
 (ite $x556 $x570 $x584)))))))))))))))))))))))))))))))
(assert
 (= c_____ c!4))
(assert
 (let ((?x339 (left s____)))
 (let ((?x450 (left s_____)))
 (let (($x710 (= ?x450 ?x339)))
 (let ((?x371 (right s____)))
 (let ((?x677 (top ?x371)))
 (let ((?x678 (stack ?x677 ?x339)))
 (let (($x704 (= ?x450 ?x678)))
 (let (($x680 ((_ is stack ) ?x371)))
 (let (($x683 (= c____ right-to-left)))
 (let (($x684 (and $x683 $x680)))
 (let ((?x532 (center s____)))
 (let (($x685 ((_ is stack ) ?x532)))
 (let (($x686 (= c____ center-to-right)))
 (let (($x687 (and $x686 $x685)))
 (let ((?x673 (top ?x532)))
 (let ((?x674 (stack ?x673 ?x339)))
 (let (($x701 (= ?x450 ?x674)))
 (let (($x688 (= c____ center-to-left)))
 (let (($x689 (and $x688 $x685)))
 (let ((?x483 (rest ?x339)))
 (let (($x697 (= ?x450 ?x483)))
 (let (($x690 ((_ is stack ) ?x339)))
 (let (($x691 (= c____ left-to-right)))
 (let (($x692 (and $x691 $x690)))
 (let (($x693 (= c____ left-to-center)))
 (let (($x694 (and $x693 $x690)))
 (ite $x694 $x697 (ite $x692 $x697 (ite $x689 $x701 (ite $x687 $x710 (ite $x684 $x704 $x710))))))))))))))))))))))))))))))))
(assert
 (let ((?x371 (right s____)))
 (let ((?x484 (right s_____)))
 (let (($x706 (= ?x484 ?x371)))
 (let ((?x676 (rest ?x371)))
 (let (($x703 (= ?x484 ?x676)))
 (let (($x680 ((_ is stack ) ?x371)))
 (let (($x681 (= c____ right-to-center)))
 (let (($x682 (and $x681 $x680)))
 (let (($x683 (= c____ right-to-left)))
 (let (($x684 (and $x683 $x680)))
 (let ((?x532 (center s____)))
 (let ((?x673 (top ?x532)))
 (let ((?x675 (stack ?x673 ?x371)))
 (let (($x702 (= ?x484 ?x675)))
 (let (($x685 ((_ is stack ) ?x532)))
 (let (($x686 (= c____ center-to-right)))
 (let (($x687 (and $x686 $x685)))
 (let (($x688 (= c____ center-to-left)))
 (let (($x689 (and $x688 $x685)))
 (let ((?x339 (left s____)))
 (let (($x690 ((_ is stack ) ?x339)))
 (let (($x691 (= c____ left-to-right)))
 (let (($x692 (and $x691 $x690)))
 (let (($x723 (ite $x692 $x706 (ite $x689 $x706 (ite $x687 $x702 (ite $x684 $x703 (ite $x682 $x703 $x706)))))))
 (let ((?x530 (top ?x339)))
 (let ((?x669 (stack ?x530 ?x371)))
 (let (($x698 (= ?x484 ?x669)))
 (let (($x693 (= c____ left-to-center)))
 (let (($x694 (and $x693 $x690)))
 (ite $x694 $x698 $x723)))))))))))))))))))))))))))))))
(assert
 (let ((?x532 (center s____)))
 (let ((?x670 (center s_____)))
 (let (($x708 (= ?x670 ?x532)))
 (let ((?x371 (right s____)))
 (let ((?x677 (top ?x371)))
 (let ((?x679 (stack ?x677 ?x532)))
 (let (($x705 (= ?x670 ?x679)))
 (let (($x680 ((_ is stack ) ?x371)))
 (let (($x681 (= c____ right-to-center)))
 (let (($x682 (and $x681 $x680)))
 (let (($x683 (= c____ right-to-left)))
 (let (($x684 (and $x683 $x680)))
 (let ((?x672 (rest ?x532)))
 (let (($x700 (= ?x670 ?x672)))
 (let (($x685 ((_ is stack ) ?x532)))
 (let (($x686 (= c____ center-to-right)))
 (let (($x687 (and $x686 $x685)))
 (let (($x688 (= c____ center-to-left)))
 (let (($x689 (and $x688 $x685)))
 (let ((?x339 (left s____)))
 (let ((?x530 (top ?x339)))
 (let ((?x671 (stack ?x530 ?x532)))
 (let (($x699 (= ?x670 ?x671)))
 (let (($x690 ((_ is stack ) ?x339)))
 (let (($x691 (= c____ left-to-right)))
 (let (($x692 (and $x691 $x690)))
 (let (($x722 (ite $x692 $x699 (ite $x689 $x700 (ite $x687 $x700 (ite $x684 $x708 (ite $x682 $x705 $x708)))))))
 (let (($x693 (= c____ left-to-center)))
 (let (($x694 (and $x693 $x690)))
 (ite $x694 $x708 $x722)))))))))))))))))))))))))))))))
(assert
 (= c______ c!5))
(assert
 (let ((?x450 (left s_____)))
 (let ((?x588 (left s______)))
 (let (($x848 (= ?x588 ?x450)))
 (let ((?x484 (right s_____)))
 (let ((?x815 (top ?x484)))
 (let ((?x816 (stack ?x815 ?x450)))
 (let (($x842 (= ?x588 ?x816)))
 (let (($x818 ((_ is stack ) ?x484)))
 (let (($x821 (= c_____ right-to-left)))
 (let (($x822 (and $x821 $x818)))
 (let ((?x670 (center s_____)))
 (let (($x823 ((_ is stack ) ?x670)))
 (let (($x824 (= c_____ center-to-right)))
 (let (($x825 (and $x824 $x823)))
 (let ((?x811 (top ?x670)))
 (let ((?x812 (stack ?x811 ?x450)))
 (let (($x839 (= ?x588 ?x812)))
 (let (($x826 (= c_____ center-to-left)))
 (let (($x827 (and $x826 $x823)))
 (let ((?x621 (rest ?x450)))
 (let (($x835 (= ?x588 ?x621)))
 (let (($x828 ((_ is stack ) ?x450)))
 (let (($x829 (= c_____ left-to-right)))
 (let (($x830 (and $x829 $x828)))
 (let (($x831 (= c_____ left-to-center)))
 (let (($x832 (and $x831 $x828)))
 (ite $x832 $x835 (ite $x830 $x835 (ite $x827 $x839 (ite $x825 $x848 (ite $x822 $x842 $x848))))))))))))))))))))))))))))))))
(assert
 (let ((?x484 (right s_____)))
 (let ((?x622 (right s______)))
 (let (($x844 (= ?x622 ?x484)))
 (let ((?x814 (rest ?x484)))
 (let (($x841 (= ?x622 ?x814)))
 (let (($x818 ((_ is stack ) ?x484)))
 (let (($x819 (= c_____ right-to-center)))
 (let (($x820 (and $x819 $x818)))
 (let (($x821 (= c_____ right-to-left)))
 (let (($x822 (and $x821 $x818)))
 (let ((?x670 (center s_____)))
 (let ((?x811 (top ?x670)))
 (let ((?x813 (stack ?x811 ?x484)))
 (let (($x840 (= ?x622 ?x813)))
 (let (($x823 ((_ is stack ) ?x670)))
 (let (($x824 (= c_____ center-to-right)))
 (let (($x825 (and $x824 $x823)))
 (let (($x826 (= c_____ center-to-left)))
 (let (($x827 (and $x826 $x823)))
 (let ((?x450 (left s_____)))
 (let (($x828 ((_ is stack ) ?x450)))
 (let (($x829 (= c_____ left-to-right)))
 (let (($x830 (and $x829 $x828)))
 (let (($x861 (ite $x830 $x844 (ite $x827 $x844 (ite $x825 $x840 (ite $x822 $x841 (ite $x820 $x841 $x844)))))))
 (let ((?x668 (top ?x450)))
 (let ((?x807 (stack ?x668 ?x484)))
 (let (($x836 (= ?x622 ?x807)))
 (let (($x831 (= c_____ left-to-center)))
 (let (($x832 (and $x831 $x828)))
 (ite $x832 $x836 $x861)))))))))))))))))))))))))))))))
(assert
 (let ((?x670 (center s_____)))
 (let ((?x808 (center s______)))
 (let (($x846 (= ?x808 ?x670)))
 (let ((?x484 (right s_____)))
 (let ((?x815 (top ?x484)))
 (let ((?x817 (stack ?x815 ?x670)))
 (let (($x843 (= ?x808 ?x817)))
 (let (($x818 ((_ is stack ) ?x484)))
 (let (($x819 (= c_____ right-to-center)))
 (let (($x820 (and $x819 $x818)))
 (let (($x821 (= c_____ right-to-left)))
 (let (($x822 (and $x821 $x818)))
 (let ((?x810 (rest ?x670)))
 (let (($x838 (= ?x808 ?x810)))
 (let (($x823 ((_ is stack ) ?x670)))
 (let (($x824 (= c_____ center-to-right)))
 (let (($x825 (and $x824 $x823)))
 (let (($x826 (= c_____ center-to-left)))
 (let (($x827 (and $x826 $x823)))
 (let ((?x450 (left s_____)))
 (let ((?x668 (top ?x450)))
 (let ((?x809 (stack ?x668 ?x670)))
 (let (($x837 (= ?x808 ?x809)))
 (let (($x828 ((_ is stack ) ?x450)))
 (let (($x829 (= c_____ left-to-right)))
 (let (($x830 (and $x829 $x828)))
 (let (($x860 (ite $x830 $x837 (ite $x827 $x838 (ite $x825 $x838 (ite $x822 $x846 (ite $x820 $x843 $x846)))))))
 (let (($x831 (= c_____ left-to-center)))
 (let (($x832 (and $x831 $x828)))
 (ite $x832 $x846 $x860)))))))))))))))))))))))))))))))
(assert
 (= c_______ c!6))
(assert
 (let ((?x588 (left s______)))
 (let ((?x726 (left s_______)))
 (let (($x986 (= ?x726 ?x588)))
 (let ((?x622 (right s______)))
 (let ((?x953 (top ?x622)))
 (let ((?x954 (stack ?x953 ?x588)))
 (let (($x980 (= ?x726 ?x954)))
 (let (($x956 ((_ is stack ) ?x622)))
 (let (($x959 (= c______ right-to-left)))
 (let (($x960 (and $x959 $x956)))
 (let ((?x808 (center s______)))
 (let (($x961 ((_ is stack ) ?x808)))
 (let (($x962 (= c______ center-to-right)))
 (let (($x963 (and $x962 $x961)))
 (let ((?x949 (top ?x808)))
 (let ((?x950 (stack ?x949 ?x588)))
 (let (($x977 (= ?x726 ?x950)))
 (let (($x964 (= c______ center-to-left)))
 (let (($x965 (and $x964 $x961)))
 (let ((?x759 (rest ?x588)))
 (let (($x973 (= ?x726 ?x759)))
 (let (($x966 ((_ is stack ) ?x588)))
 (let (($x967 (= c______ left-to-right)))
 (let (($x968 (and $x967 $x966)))
 (let (($x969 (= c______ left-to-center)))
 (let (($x970 (and $x969 $x966)))
 (ite $x970 $x973 (ite $x968 $x973 (ite $x965 $x977 (ite $x963 $x986 (ite $x960 $x980 $x986))))))))))))))))))))))))))))))))
(assert
 (let ((?x622 (right s______)))
 (let ((?x760 (right s_______)))
 (let (($x982 (= ?x760 ?x622)))
 (let ((?x952 (rest ?x622)))
 (let (($x979 (= ?x760 ?x952)))
 (let (($x956 ((_ is stack ) ?x622)))
 (let (($x957 (= c______ right-to-center)))
 (let (($x958 (and $x957 $x956)))
 (let (($x959 (= c______ right-to-left)))
 (let (($x960 (and $x959 $x956)))
 (let ((?x808 (center s______)))
 (let ((?x949 (top ?x808)))
 (let ((?x951 (stack ?x949 ?x622)))
 (let (($x978 (= ?x760 ?x951)))
 (let (($x961 ((_ is stack ) ?x808)))
 (let (($x962 (= c______ center-to-right)))
 (let (($x963 (and $x962 $x961)))
 (let (($x964 (= c______ center-to-left)))
 (let (($x965 (and $x964 $x961)))
 (let ((?x588 (left s______)))
 (let (($x966 ((_ is stack ) ?x588)))
 (let (($x967 (= c______ left-to-right)))
 (let (($x968 (and $x967 $x966)))
 (let (($x999 (ite $x968 $x982 (ite $x965 $x982 (ite $x963 $x978 (ite $x960 $x979 (ite $x958 $x979 $x982)))))))
 (let ((?x806 (top ?x588)))
 (let ((?x945 (stack ?x806 ?x622)))
 (let (($x974 (= ?x760 ?x945)))
 (let (($x969 (= c______ left-to-center)))
 (let (($x970 (and $x969 $x966)))
 (ite $x970 $x974 $x999)))))))))))))))))))))))))))))))
(assert
 (let ((?x808 (center s______)))
 (let ((?x946 (center s_______)))
 (let (($x984 (= ?x946 ?x808)))
 (let ((?x622 (right s______)))
 (let ((?x953 (top ?x622)))
 (let ((?x955 (stack ?x953 ?x808)))
 (let (($x981 (= ?x946 ?x955)))
 (let (($x956 ((_ is stack ) ?x622)))
 (let (($x957 (= c______ right-to-center)))
 (let (($x958 (and $x957 $x956)))
 (let (($x959 (= c______ right-to-left)))
 (let (($x960 (and $x959 $x956)))
 (let ((?x948 (rest ?x808)))
 (let (($x976 (= ?x946 ?x948)))
 (let (($x961 ((_ is stack ) ?x808)))
 (let (($x962 (= c______ center-to-right)))
 (let (($x963 (and $x962 $x961)))
 (let (($x964 (= c______ center-to-left)))
 (let (($x965 (and $x964 $x961)))
 (let ((?x588 (left s______)))
 (let ((?x806 (top ?x588)))
 (let ((?x947 (stack ?x806 ?x808)))
 (let (($x975 (= ?x946 ?x947)))
 (let (($x966 ((_ is stack ) ?x588)))
 (let (($x967 (= c______ left-to-right)))
 (let (($x968 (and $x967 $x966)))
 (let (($x998 (ite $x968 $x975 (ite $x965 $x976 (ite $x963 $x976 (ite $x960 $x984 (ite $x958 $x981 $x984)))))))
 (let (($x969 (= c______ left-to-center)))
 (let (($x970 (and $x969 $x966)))
 (ite $x970 $x984 $x998)))))))))))))))))))))))))))))))
(assert
 (let ((?x76 (stack T (stack S (stack I (stack H (stack F (stack A empty))))))))
 (let ((?x77 (stack V ?x76)))
 (let ((?x760 (right s_______)))
 (let (($x1002 (= ?x760 ?x77)))
 (let ((?x69 (stack B (stack Q (stack D (stack P (stack K (stack U empty))))))))
 (let ((?x70 (stack J ?x69)))
 (let ((?x946 (center s_______)))
 (let (($x1033 (= ?x946 ?x70)))
 (let ((?x61 (stack N (stack M (stack R (stack O (stack C (stack E empty))))))))
 (let ((?x63 (stack G (stack L ?x61))))
 (let ((?x726 (left s_______)))
 (let (($x1034 (= ?x726 ?x63)))
 (let (($x1036 (not (and $x1034 $x1033 $x1002))))
 (not $x1036)))))))))))))))
(check-sat)