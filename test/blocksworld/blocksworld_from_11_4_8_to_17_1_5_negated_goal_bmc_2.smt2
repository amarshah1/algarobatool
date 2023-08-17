; 
(set-info :status unknown)
(set-logic QF_DT)
(declare-datatypes ((Enum_A_B_C_D_E_F_G_H_I_J_K_L_M_N_O_P_Q_R_S_T_U_V_W 0)) (((A) (B) (C) (D) (E) (F) (G) (H) (I) (J) (K) (L) (M) (N) (O) (P) (Q) (R) (S) (T) (U) (V) (W))))
 (declare-datatypes ((Tower 0)) (((stack (top Enum_A_B_C_D_E_F_G_H_I_J_K_L_M_N_O_P_Q_R_S_T_U_V_W) (rest Tower)) (empty))))
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
(assert
 (= s_tmp s))
(assert
 (= c_tmp c))
(assert
 (let ((?x35 (stack V empty)))
 (let ((?x36 (stack O ?x35)))
 (let ((?x37 (stack S ?x36)))
 (let ((?x38 (stack U ?x37)))
 (let ((?x39 (stack M ?x38)))
 (let ((?x40 (stack Q ?x39)))
 (let ((?x41 (stack R ?x40)))
 (let ((?x42 (stack F ?x41)))
 (let ((?x43 (stack C ?x42)))
 (let ((?x44 (stack G ?x43)))
 (let ((?x45 (stack K ?x44)))
 (let ((?x127 (left s_tmp_)))
 (= ?x127 ?x45))))))))))))))
(assert
 (let ((?x46 (stack P empty)))
 (let ((?x47 (stack H ?x46)))
 (let ((?x48 (stack B ?x47)))
 (let ((?x49 (stack L ?x48)))
 (let ((?x130 (center s_tmp__)))
 (= ?x130 ?x49)))))))
(assert
 (let ((?x50 (stack A empty)))
 (let ((?x51 (stack D ?x50)))
 (let ((?x52 (stack E ?x51)))
 (let ((?x53 (stack I ?x52)))
 (let ((?x54 (stack J ?x53)))
 (let ((?x55 (stack N ?x54)))
 (let ((?x56 (stack T ?x55)))
 (let ((?x57 (stack W ?x56)))
 (let ((?x133 (right s_tmp___)))
 (= ?x133 ?x57)))))))))))
(assert
 (= c_tmp____ c!0))
(assert
 (= s_ s_tmp___))
(assert
 (= c_ c_tmp____))
(assert
 (let ((?x160 (left s_tmp__)))
 (= (left s_tmp___) ?x160)))
(assert
 (let ((?x130 (center s_tmp__)))
 (= (center s_tmp___) ?x130)))
(assert
 (let ((?x164 (center s_tmp_)))
 (= ?x164 (center s_tmp))))
(assert
 (let ((?x167 (right s_tmp_)))
 (= ?x167 (right s_tmp))))
(assert
 (let ((?x127 (left s_tmp_)))
 (let ((?x160 (left s_tmp__)))
 (= ?x160 ?x127))))
(assert
 (let ((?x167 (right s_tmp_)))
 (let ((?x171 (right s_tmp__)))
 (= ?x171 ?x167))))
(assert
 (= c__ c!1))
(assert
 (let ((?x104 (left s_)))
 (let ((?x308 (left s__)))
 (let (($x305 (= ?x308 ?x104)))
 (let ((?x107 (right s_)))
 (let (($x299 ((_ is stack ) ?x107)))
 (let (($x329 (= c_ right-to-left)))
 (let (($x318 (and $x329 $x299)))
 (let ((?x109 (center s_)))
 (let (($x319 ((_ is stack ) ?x109)))
 (let (($x320 (= c_ center-to-right)))
 (let (($x324 (and $x320 $x319)))
 (let (($x325 (= c_ center-to-left)))
 (let (($x326 (and $x325 $x319)))
 (let (($x338 (ite $x326 (= ?x308 (stack (top ?x109) ?x104)) (ite $x324 $x305 (ite $x318 (= ?x308 (stack (top ?x107) ?x104)) $x305)))))
 (let (($x316 (= ?x308 (rest ?x104))))
 (let (($x312 ((_ is stack ) ?x104)))
 (let (($x313 (= c_ left-to-right)))
 (let (($x314 (and $x313 $x312)))
 (let (($x288 (= c_ left-to-center)))
 (let (($x289 (and $x288 $x312)))
 (ite $x289 $x316 (ite $x314 $x316 $x338)))))))))))))))))))))))
(assert
 (let ((?x107 (right s_)))
 (let ((?x283 (right s__)))
 (let (($x322 (= ?x283 ?x107)))
 (let (($x295 (= ?x283 (rest ?x107))))
 (let (($x299 ((_ is stack ) ?x107)))
 (let (($x327 (= c_ right-to-center)))
 (let (($x328 (and $x327 $x299)))
 (let (($x329 (= c_ right-to-left)))
 (let (($x318 (and $x329 $x299)))
 (let ((?x109 (center s_)))
 (let (($x319 ((_ is stack ) ?x109)))
 (let (($x320 (= c_ center-to-right)))
 (let (($x324 (and $x320 $x319)))
 (let (($x335 (ite $x324 (= ?x283 (stack (top ?x109) ?x107)) (ite $x318 $x295 (ite $x328 $x295 $x322)))))
 (let (($x325 (= c_ center-to-left)))
 (let (($x326 (and $x325 $x319)))
 (let ((?x104 (left s_)))
 (let (($x312 ((_ is stack ) ?x104)))
 (let (($x313 (= c_ left-to-right)))
 (let (($x314 (and $x313 $x312)))
 (let (($x288 (= c_ left-to-center)))
 (let (($x289 (and $x288 $x312)))
 (ite $x289 (= ?x283 (stack (top ?x104) ?x107)) (ite $x314 $x322 (ite $x326 $x322 $x335))))))))))))))))))))))))))
(assert
 (let ((?x109 (center s_)))
 (let ((?x286 (center s__)))
 (let (($x303 (= ?x286 ?x109)))
 (let ((?x107 (right s_)))
 (let (($x299 ((_ is stack ) ?x107)))
 (let (($x327 (= c_ right-to-center)))
 (let (($x328 (and $x327 $x299)))
 (let (($x329 (= c_ right-to-left)))
 (let (($x318 (and $x329 $x299)))
 (let (($x331 (= ?x286 (rest ?x109))))
 (let (($x319 ((_ is stack ) ?x109)))
 (let (($x320 (= c_ center-to-right)))
 (let (($x324 (and $x320 $x319)))
 (let (($x334 (ite $x324 $x331 (ite $x318 $x303 (ite $x328 (= ?x286 (stack (top ?x107) ?x109)) $x303)))))
 (let (($x325 (= c_ center-to-left)))
 (let (($x326 (and $x325 $x319)))
 (let ((?x104 (left s_)))
 (let (($x312 ((_ is stack ) ?x104)))
 (let (($x313 (= c_ left-to-right)))
 (let (($x314 (and $x313 $x312)))
 (let (($x288 (= c_ left-to-center)))
 (let (($x289 (and $x288 $x312)))
 (ite $x289 $x303 (ite $x314 (= ?x286 (stack (top ?x104) ?x109)) (ite $x326 $x331 $x334))))))))))))))))))))))))))
(assert
 (= c___ c!2))
(assert
 (let ((?x308 (left s__)))
 (let ((?x174 (left s___)))
 (let (($x458 (= ?x174 ?x308)))
 (let ((?x283 (right s__)))
 (let ((?x425 (top ?x283)))
 (let ((?x426 (stack ?x425 ?x308)))
 (let (($x452 (= ?x174 ?x426)))
 (let (($x428 ((_ is stack ) ?x283)))
 (let (($x431 (= c__ right-to-left)))
 (let (($x432 (and $x431 $x428)))
 (let ((?x286 (center s__)))
 (let (($x433 ((_ is stack ) ?x286)))
 (let (($x434 (= c__ center-to-right)))
 (let (($x435 (and $x434 $x433)))
 (let ((?x421 (top ?x286)))
 (let ((?x422 (stack ?x421 ?x308)))
 (let (($x449 (= ?x174 ?x422)))
 (let (($x436 (= c__ center-to-left)))
 (let (($x437 (and $x436 $x433)))
 (let ((?x176 (rest ?x308)))
 (let (($x445 (= ?x174 ?x176)))
 (let (($x438 ((_ is stack ) ?x308)))
 (let (($x439 (= c__ left-to-right)))
 (let (($x440 (and $x439 $x438)))
 (let (($x441 (= c__ left-to-center)))
 (let (($x442 (and $x441 $x438)))
 (ite $x442 $x445 (ite $x440 $x445 (ite $x437 $x449 (ite $x435 $x458 (ite $x432 $x452 $x458))))))))))))))))))))))))))))))))
(assert
 (let ((?x283 (right s__)))
 (let ((?x177 (right s___)))
 (let (($x454 (= ?x177 ?x283)))
 (let ((?x424 (rest ?x283)))
 (let (($x451 (= ?x177 ?x424)))
 (let (($x428 ((_ is stack ) ?x283)))
 (let (($x429 (= c__ right-to-center)))
 (let (($x430 (and $x429 $x428)))
 (let (($x431 (= c__ right-to-left)))
 (let (($x432 (and $x431 $x428)))
 (let ((?x286 (center s__)))
 (let ((?x421 (top ?x286)))
 (let ((?x423 (stack ?x421 ?x283)))
 (let (($x450 (= ?x177 ?x423)))
 (let (($x433 ((_ is stack ) ?x286)))
 (let (($x434 (= c__ center-to-right)))
 (let (($x435 (and $x434 $x433)))
 (let (($x436 (= c__ center-to-left)))
 (let (($x437 (and $x436 $x433)))
 (let ((?x308 (left s__)))
 (let (($x438 ((_ is stack ) ?x308)))
 (let (($x439 (= c__ left-to-right)))
 (let (($x440 (and $x439 $x438)))
 (let (($x471 (ite $x440 $x454 (ite $x437 $x454 (ite $x435 $x450 (ite $x432 $x451 (ite $x430 $x451 $x454)))))))
 (let ((?x281 (top ?x308)))
 (let ((?x417 (stack ?x281 ?x283)))
 (let (($x446 (= ?x177 ?x417)))
 (let (($x441 (= c__ left-to-center)))
 (let (($x442 (and $x441 $x438)))
 (ite $x442 $x446 $x471)))))))))))))))))))))))))))))))
(assert
 (let ((?x286 (center s__)))
 (let ((?x418 (center s___)))
 (let (($x456 (= ?x418 ?x286)))
 (let ((?x283 (right s__)))
 (let ((?x425 (top ?x283)))
 (let ((?x427 (stack ?x425 ?x286)))
 (let (($x453 (= ?x418 ?x427)))
 (let (($x428 ((_ is stack ) ?x283)))
 (let (($x429 (= c__ right-to-center)))
 (let (($x430 (and $x429 $x428)))
 (let (($x431 (= c__ right-to-left)))
 (let (($x432 (and $x431 $x428)))
 (let ((?x420 (rest ?x286)))
 (let (($x448 (= ?x418 ?x420)))
 (let (($x433 ((_ is stack ) ?x286)))
 (let (($x434 (= c__ center-to-right)))
 (let (($x435 (and $x434 $x433)))
 (let (($x436 (= c__ center-to-left)))
 (let (($x437 (and $x436 $x433)))
 (let ((?x308 (left s__)))
 (let ((?x281 (top ?x308)))
 (let ((?x419 (stack ?x281 ?x286)))
 (let (($x447 (= ?x418 ?x419)))
 (let (($x438 ((_ is stack ) ?x308)))
 (let (($x439 (= c__ left-to-right)))
 (let (($x440 (and $x439 $x438)))
 (let (($x470 (ite $x440 $x447 (ite $x437 $x448 (ite $x435 $x448 (ite $x432 $x456 (ite $x430 $x453 $x456)))))))
 (let (($x441 (= c__ left-to-center)))
 (let (($x442 (and $x441 $x438)))
 (ite $x442 $x456 $x470)))))))))))))))))))))))))))))))
(assert
 (let ((?x80 (stack P (stack G (stack E (stack D (stack C empty)))))))
 (let ((?x177 (right s___)))
 (let (($x474 (= ?x177 ?x80)))
 (let ((?x75 (stack K empty)))
 (let ((?x418 (center s___)))
 (let (($x505 (= ?x418 ?x75)))
 (let ((?x63 (stack M (stack V (stack W (stack R (stack J (stack H empty))))))))
 (let ((?x69 (stack F (stack B (stack I (stack A (stack Q (stack U ?x63))))))))
 (let ((?x74 (stack T (stack L (stack O (stack N (stack S ?x69)))))))
 (let ((?x174 (left s___)))
 (let (($x506 (= ?x174 ?x74)))
 (let (($x508 (not (and $x506 $x505 $x474))))
 (not $x508))))))))))))))
(check-sat)
