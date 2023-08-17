; 
(set-info :status unknown)
(set-logic QF_DT)
(declare-datatypes ((Enum_A_B_C_D_E_F_G_H_I_J_K_L_M_N_O_P_Q_R_S_T 0)) (((A) (B) (C) (D) (E) (F) (G) (H) (I) (J) (K) (L) (M) (N) (O) (P) (Q) (R) (S) (T))))
 (declare-datatypes ((Tower 0)) (((stack (top Enum_A_B_C_D_E_F_G_H_I_J_K_L_M_N_O_P_Q_R_S_T) (rest Tower)) (empty))))
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
 (let ((?x118 (left s_tmp_)))
 (= ?x118 empty)))
(assert
 (let ((?x33 (stack M empty)))
 (let ((?x121 (center s_tmp__)))
 (= ?x121 ?x33))))
(assert
 (let ((?x32 (stack A empty)))
 (let ((?x34 (stack B ?x32)))
 (let ((?x35 (stack C ?x34)))
 (let ((?x36 (stack D ?x35)))
 (let ((?x37 (stack E ?x36)))
 (let ((?x38 (stack F ?x37)))
 (let ((?x39 (stack G ?x38)))
 (let ((?x40 (stack H ?x39)))
 (let ((?x41 (stack I ?x40)))
 (let ((?x42 (stack J ?x41)))
 (let ((?x43 (stack K ?x42)))
 (let ((?x44 (stack L ?x43)))
 (let ((?x45 (stack N ?x44)))
 (let ((?x46 (stack O ?x45)))
 (let ((?x47 (stack P ?x46)))
 (let ((?x48 (stack Q ?x47)))
 (let ((?x49 (stack R ?x48)))
 (let ((?x50 (stack S ?x49)))
 (let ((?x51 (stack T ?x50)))
 (let ((?x124 (right s_tmp___)))
 (= ?x124 ?x51))))))))))))))))))))))
(assert
 (= c_tmp____ c!0))
(assert
 (= s_ s_tmp___))
(assert
 (= c_ c_tmp____))
(assert
 (let ((?x151 (left s_tmp__)))
 (= (left s_tmp___) ?x151)))
(assert
 (let ((?x121 (center s_tmp__)))
 (= (center s_tmp___) ?x121)))
(assert
 (let ((?x155 (center s_tmp_)))
 (= ?x155 (center s_tmp))))
(assert
 (let ((?x158 (right s_tmp_)))
 (= ?x158 (right s_tmp))))
(assert
 (let ((?x118 (left s_tmp_)))
 (let ((?x151 (left s_tmp__)))
 (= ?x151 ?x118))))
(assert
 (let ((?x158 (right s_tmp_)))
 (let ((?x162 (right s_tmp__)))
 (= ?x162 ?x158))))
(assert
 (= c__ c!1))
(assert
 (let ((?x95 (left s_)))
 (let ((?x296 (left s__)))
 (let (($x311 (= ?x296 ?x95)))
 (let ((?x98 (right s_)))
 (let (($x269 ((_ is stack ) ?x98)))
 (let (($x278 (= c_ right-to-left)))
 (let (($x279 (and $x278 $x269)))
 (let ((?x100 (center s_)))
 (let (($x280 ((_ is stack ) ?x100)))
 (let (($x281 (= c_ center-to-right)))
 (let (($x282 (and $x281 $x280)))
 (let (($x283 (= c_ center-to-left)))
 (let (($x284 (and $x283 $x280)))
 (let (($x320 (ite $x284 (= ?x296 (stack (top ?x100) ?x95)) (ite $x282 $x311 (ite $x279 (= ?x296 (stack (top ?x98) ?x95)) $x311)))))
 (let (($x289 (= ?x296 (rest ?x95))))
 (let (($x270 ((_ is stack ) ?x95)))
 (let (($x272 (and (= c_ left-to-right) $x270)))
 (let (($x292 (and (= c_ left-to-center) $x270)))
 (ite $x292 $x289 (ite $x272 $x289 $x320)))))))))))))))))))))
(assert
 (let ((?x98 (right s_)))
 (let ((?x274 (right s__)))
 (let (($x307 (= ?x274 ?x98)))
 (let (($x304 (= ?x274 (rest ?x98))))
 (let (($x269 ((_ is stack ) ?x98)))
 (let (($x276 (= c_ right-to-center)))
 (let (($x277 (and $x276 $x269)))
 (let (($x278 (= c_ right-to-left)))
 (let (($x279 (and $x278 $x269)))
 (let ((?x100 (center s_)))
 (let (($x280 ((_ is stack ) ?x100)))
 (let (($x281 (= c_ center-to-right)))
 (let (($x282 (and $x281 $x280)))
 (let (($x317 (ite $x282 (= ?x274 (stack (top ?x100) ?x98)) (ite $x279 $x304 (ite $x277 $x304 $x307)))))
 (let (($x283 (= c_ center-to-left)))
 (let (($x284 (and $x283 $x280)))
 (let ((?x95 (left s_)))
 (let (($x270 ((_ is stack ) ?x95)))
 (let (($x272 (and (= c_ left-to-right) $x270)))
 (let (($x292 (and (= c_ left-to-center) $x270)))
 (ite $x292 (= ?x274 (stack (top ?x95) ?x98)) (ite $x272 $x307 (ite $x284 $x307 $x317))))))))))))))))))))))))
(assert
 (let ((?x100 (center s_)))
 (let ((?x298 (center s__)))
 (let (($x309 (= ?x298 ?x100)))
 (let ((?x98 (right s_)))
 (let (($x269 ((_ is stack ) ?x98)))
 (let (($x276 (= c_ right-to-center)))
 (let (($x277 (and $x276 $x269)))
 (let (($x278 (= c_ right-to-left)))
 (let (($x279 (and $x278 $x269)))
 (let (($x301 (= ?x298 (rest ?x100))))
 (let (($x280 ((_ is stack ) ?x100)))
 (let (($x281 (= c_ center-to-right)))
 (let (($x282 (and $x281 $x280)))
 (let (($x316 (ite $x282 $x301 (ite $x279 $x309 (ite $x277 (= ?x298 (stack (top ?x98) ?x100)) $x309)))))
 (let (($x283 (= c_ center-to-left)))
 (let (($x284 (and $x283 $x280)))
 (let ((?x95 (left s_)))
 (let (($x270 ((_ is stack ) ?x95)))
 (let (($x272 (and (= c_ left-to-right) $x270)))
 (let (($x292 (and (= c_ left-to-center) $x270)))
 (ite $x292 $x309 (ite $x272 (= ?x298 (stack (top ?x95) ?x100)) (ite $x284 $x301 $x316))))))))))))))))))))))))
(assert
 (= c___ c!2))
(assert
 (let ((?x296 (left s__)))
 (let ((?x165 (left s___)))
 (let (($x419 (= ?x165 ?x296)))
 (let ((?x274 (right s__)))
 (let ((?x386 (top ?x274)))
 (let ((?x387 (stack ?x386 ?x296)))
 (let (($x413 (= ?x165 ?x387)))
 (let (($x389 ((_ is stack ) ?x274)))
 (let (($x392 (= c__ right-to-left)))
 (let (($x393 (and $x392 $x389)))
 (let ((?x298 (center s__)))
 (let (($x394 ((_ is stack ) ?x298)))
 (let (($x395 (= c__ center-to-right)))
 (let (($x396 (and $x395 $x394)))
 (let ((?x382 (top ?x298)))
 (let ((?x383 (stack ?x382 ?x296)))
 (let (($x410 (= ?x165 ?x383)))
 (let (($x397 (= c__ center-to-left)))
 (let (($x398 (and $x397 $x394)))
 (let ((?x167 (rest ?x296)))
 (let (($x406 (= ?x165 ?x167)))
 (let (($x399 ((_ is stack ) ?x296)))
 (let (($x400 (= c__ left-to-right)))
 (let (($x401 (and $x400 $x399)))
 (let (($x402 (= c__ left-to-center)))
 (let (($x403 (and $x402 $x399)))
 (ite $x403 $x406 (ite $x401 $x406 (ite $x398 $x410 (ite $x396 $x419 (ite $x393 $x413 $x419))))))))))))))))))))))))))))))))
(assert
 (let ((?x274 (right s__)))
 (let ((?x168 (right s___)))
 (let (($x415 (= ?x168 ?x274)))
 (let ((?x385 (rest ?x274)))
 (let (($x412 (= ?x168 ?x385)))
 (let (($x389 ((_ is stack ) ?x274)))
 (let (($x390 (= c__ right-to-center)))
 (let (($x391 (and $x390 $x389)))
 (let (($x392 (= c__ right-to-left)))
 (let (($x393 (and $x392 $x389)))
 (let ((?x298 (center s__)))
 (let ((?x382 (top ?x298)))
 (let ((?x384 (stack ?x382 ?x274)))
 (let (($x411 (= ?x168 ?x384)))
 (let (($x394 ((_ is stack ) ?x298)))
 (let (($x395 (= c__ center-to-right)))
 (let (($x396 (and $x395 $x394)))
 (let (($x397 (= c__ center-to-left)))
 (let (($x398 (and $x397 $x394)))
 (let ((?x296 (left s__)))
 (let (($x399 ((_ is stack ) ?x296)))
 (let (($x400 (= c__ left-to-right)))
 (let (($x401 (and $x400 $x399)))
 (let (($x432 (ite $x401 $x415 (ite $x398 $x415 (ite $x396 $x411 (ite $x393 $x412 (ite $x391 $x412 $x415)))))))
 (let ((?x263 (top ?x296)))
 (let ((?x378 (stack ?x263 ?x274)))
 (let (($x407 (= ?x168 ?x378)))
 (let (($x402 (= c__ left-to-center)))
 (let (($x403 (and $x402 $x399)))
 (ite $x403 $x407 $x432)))))))))))))))))))))))))))))))
(assert
 (let ((?x298 (center s__)))
 (let ((?x379 (center s___)))
 (let (($x417 (= ?x379 ?x298)))
 (let ((?x274 (right s__)))
 (let ((?x386 (top ?x274)))
 (let ((?x388 (stack ?x386 ?x298)))
 (let (($x414 (= ?x379 ?x388)))
 (let (($x389 ((_ is stack ) ?x274)))
 (let (($x390 (= c__ right-to-center)))
 (let (($x391 (and $x390 $x389)))
 (let (($x392 (= c__ right-to-left)))
 (let (($x393 (and $x392 $x389)))
 (let ((?x381 (rest ?x298)))
 (let (($x409 (= ?x379 ?x381)))
 (let (($x394 ((_ is stack ) ?x298)))
 (let (($x395 (= c__ center-to-right)))
 (let (($x396 (and $x395 $x394)))
 (let (($x397 (= c__ center-to-left)))
 (let (($x398 (and $x397 $x394)))
 (let ((?x296 (left s__)))
 (let ((?x263 (top ?x296)))
 (let ((?x380 (stack ?x263 ?x298)))
 (let (($x408 (= ?x379 ?x380)))
 (let (($x399 ((_ is stack ) ?x296)))
 (let (($x400 (= c__ left-to-right)))
 (let (($x401 (and $x400 $x399)))
 (let (($x431 (ite $x401 $x408 (ite $x398 $x409 (ite $x396 $x409 (ite $x393 $x417 (ite $x391 $x414 $x417)))))))
 (let (($x402 (= c__ left-to-center)))
 (let (($x403 (and $x402 $x399)))
 (ite $x403 $x417 $x431)))))))))))))))))))))))))))))))
(assert
 (= c____ c!3))
(assert
 (let ((?x165 (left s___)))
 (let ((?x327 (left s____)))
 (let (($x557 (= ?x327 ?x165)))
 (let ((?x168 (right s___)))
 (let ((?x524 (top ?x168)))
 (let ((?x525 (stack ?x524 ?x165)))
 (let (($x551 (= ?x327 ?x525)))
 (let (($x527 ((_ is stack ) ?x168)))
 (let (($x530 (= c___ right-to-left)))
 (let (($x531 (and $x530 $x527)))
 (let ((?x379 (center s___)))
 (let (($x532 ((_ is stack ) ?x379)))
 (let (($x533 (= c___ center-to-right)))
 (let (($x534 (and $x533 $x532)))
 (let ((?x520 (top ?x379)))
 (let ((?x521 (stack ?x520 ?x165)))
 (let (($x548 (= ?x327 ?x521)))
 (let (($x535 (= c___ center-to-left)))
 (let (($x536 (and $x535 $x532)))
 (let ((?x357 (rest ?x165)))
 (let (($x544 (= ?x327 ?x357)))
 (let (($x537 ((_ is stack ) ?x165)))
 (let (($x538 (= c___ left-to-right)))
 (let (($x539 (and $x538 $x537)))
 (let (($x540 (= c___ left-to-center)))
 (let (($x541 (and $x540 $x537)))
 (ite $x541 $x544 (ite $x539 $x544 (ite $x536 $x548 (ite $x534 $x557 (ite $x531 $x551 $x557))))))))))))))))))))))))))))))))
(assert
 (let ((?x168 (right s___)))
 (let ((?x358 (right s____)))
 (let (($x553 (= ?x358 ?x168)))
 (let ((?x523 (rest ?x168)))
 (let (($x550 (= ?x358 ?x523)))
 (let (($x527 ((_ is stack ) ?x168)))
 (let (($x528 (= c___ right-to-center)))
 (let (($x529 (and $x528 $x527)))
 (let (($x530 (= c___ right-to-left)))
 (let (($x531 (and $x530 $x527)))
 (let ((?x379 (center s___)))
 (let ((?x520 (top ?x379)))
 (let ((?x522 (stack ?x520 ?x168)))
 (let (($x549 (= ?x358 ?x522)))
 (let (($x532 ((_ is stack ) ?x379)))
 (let (($x533 (= c___ center-to-right)))
 (let (($x534 (and $x533 $x532)))
 (let (($x535 (= c___ center-to-left)))
 (let (($x536 (and $x535 $x532)))
 (let ((?x165 (left s___)))
 (let (($x537 ((_ is stack ) ?x165)))
 (let (($x538 (= c___ left-to-right)))
 (let (($x539 (and $x538 $x537)))
 (let (($x570 (ite $x539 $x553 (ite $x536 $x553 (ite $x534 $x549 (ite $x531 $x550 (ite $x529 $x550 $x553)))))))
 (let ((?x377 (top ?x165)))
 (let ((?x516 (stack ?x377 ?x168)))
 (let (($x545 (= ?x358 ?x516)))
 (let (($x540 (= c___ left-to-center)))
 (let (($x541 (and $x540 $x537)))
 (ite $x541 $x545 $x570)))))))))))))))))))))))))))))))
(assert
 (let ((?x379 (center s___)))
 (let ((?x517 (center s____)))
 (let (($x555 (= ?x517 ?x379)))
 (let ((?x168 (right s___)))
 (let ((?x524 (top ?x168)))
 (let ((?x526 (stack ?x524 ?x379)))
 (let (($x552 (= ?x517 ?x526)))
 (let (($x527 ((_ is stack ) ?x168)))
 (let (($x528 (= c___ right-to-center)))
 (let (($x529 (and $x528 $x527)))
 (let (($x530 (= c___ right-to-left)))
 (let (($x531 (and $x530 $x527)))
 (let ((?x519 (rest ?x379)))
 (let (($x547 (= ?x517 ?x519)))
 (let (($x532 ((_ is stack ) ?x379)))
 (let (($x533 (= c___ center-to-right)))
 (let (($x534 (and $x533 $x532)))
 (let (($x535 (= c___ center-to-left)))
 (let (($x536 (and $x535 $x532)))
 (let ((?x165 (left s___)))
 (let ((?x377 (top ?x165)))
 (let ((?x518 (stack ?x377 ?x379)))
 (let (($x546 (= ?x517 ?x518)))
 (let (($x537 ((_ is stack ) ?x165)))
 (let (($x538 (= c___ left-to-right)))
 (let (($x539 (and $x538 $x537)))
 (let (($x569 (ite $x539 $x546 (ite $x536 $x547 (ite $x534 $x547 (ite $x531 $x555 (ite $x529 $x552 $x555)))))))
 (let (($x540 (= c___ left-to-center)))
 (let (($x541 (and $x540 $x537)))
 (ite $x541 $x555 $x569)))))))))))))))))))))))))))))))
(assert
 (= c_____ c!4))
(assert
 (let ((?x327 (left s____)))
 (let ((?x435 (left s_____)))
 (let (($x695 (= ?x435 ?x327)))
 (let ((?x358 (right s____)))
 (let ((?x662 (top ?x358)))
 (let ((?x663 (stack ?x662 ?x327)))
 (let (($x689 (= ?x435 ?x663)))
 (let (($x665 ((_ is stack ) ?x358)))
 (let (($x668 (= c____ right-to-left)))
 (let (($x669 (and $x668 $x665)))
 (let ((?x517 (center s____)))
 (let (($x670 ((_ is stack ) ?x517)))
 (let (($x671 (= c____ center-to-right)))
 (let (($x672 (and $x671 $x670)))
 (let ((?x658 (top ?x517)))
 (let ((?x659 (stack ?x658 ?x327)))
 (let (($x686 (= ?x435 ?x659)))
 (let (($x673 (= c____ center-to-left)))
 (let (($x674 (and $x673 $x670)))
 (let ((?x468 (rest ?x327)))
 (let (($x682 (= ?x435 ?x468)))
 (let (($x675 ((_ is stack ) ?x327)))
 (let (($x676 (= c____ left-to-right)))
 (let (($x677 (and $x676 $x675)))
 (let (($x678 (= c____ left-to-center)))
 (let (($x679 (and $x678 $x675)))
 (ite $x679 $x682 (ite $x677 $x682 (ite $x674 $x686 (ite $x672 $x695 (ite $x669 $x689 $x695))))))))))))))))))))))))))))))))
(assert
 (let ((?x358 (right s____)))
 (let ((?x469 (right s_____)))
 (let (($x691 (= ?x469 ?x358)))
 (let ((?x661 (rest ?x358)))
 (let (($x688 (= ?x469 ?x661)))
 (let (($x665 ((_ is stack ) ?x358)))
 (let (($x666 (= c____ right-to-center)))
 (let (($x667 (and $x666 $x665)))
 (let (($x668 (= c____ right-to-left)))
 (let (($x669 (and $x668 $x665)))
 (let ((?x517 (center s____)))
 (let ((?x658 (top ?x517)))
 (let ((?x660 (stack ?x658 ?x358)))
 (let (($x687 (= ?x469 ?x660)))
 (let (($x670 ((_ is stack ) ?x517)))
 (let (($x671 (= c____ center-to-right)))
 (let (($x672 (and $x671 $x670)))
 (let (($x673 (= c____ center-to-left)))
 (let (($x674 (and $x673 $x670)))
 (let ((?x327 (left s____)))
 (let (($x675 ((_ is stack ) ?x327)))
 (let (($x676 (= c____ left-to-right)))
 (let (($x677 (and $x676 $x675)))
 (let (($x708 (ite $x677 $x691 (ite $x674 $x691 (ite $x672 $x687 (ite $x669 $x688 (ite $x667 $x688 $x691)))))))
 (let ((?x515 (top ?x327)))
 (let ((?x654 (stack ?x515 ?x358)))
 (let (($x683 (= ?x469 ?x654)))
 (let (($x678 (= c____ left-to-center)))
 (let (($x679 (and $x678 $x675)))
 (ite $x679 $x683 $x708)))))))))))))))))))))))))))))))
(assert
 (let ((?x517 (center s____)))
 (let ((?x655 (center s_____)))
 (let (($x693 (= ?x655 ?x517)))
 (let ((?x358 (right s____)))
 (let ((?x662 (top ?x358)))
 (let ((?x664 (stack ?x662 ?x517)))
 (let (($x690 (= ?x655 ?x664)))
 (let (($x665 ((_ is stack ) ?x358)))
 (let (($x666 (= c____ right-to-center)))
 (let (($x667 (and $x666 $x665)))
 (let (($x668 (= c____ right-to-left)))
 (let (($x669 (and $x668 $x665)))
 (let ((?x657 (rest ?x517)))
 (let (($x685 (= ?x655 ?x657)))
 (let (($x670 ((_ is stack ) ?x517)))
 (let (($x671 (= c____ center-to-right)))
 (let (($x672 (and $x671 $x670)))
 (let (($x673 (= c____ center-to-left)))
 (let (($x674 (and $x673 $x670)))
 (let ((?x327 (left s____)))
 (let ((?x515 (top ?x327)))
 (let ((?x656 (stack ?x515 ?x517)))
 (let (($x684 (= ?x655 ?x656)))
 (let (($x675 ((_ is stack ) ?x327)))
 (let (($x676 (= c____ left-to-right)))
 (let (($x677 (and $x676 $x675)))
 (let (($x707 (ite $x677 $x684 (ite $x674 $x685 (ite $x672 $x685 (ite $x669 $x693 (ite $x667 $x690 $x693)))))))
 (let (($x678 (= c____ left-to-center)))
 (let (($x679 (and $x678 $x675)))
 (ite $x679 $x693 $x707)))))))))))))))))))))))))))))))
(assert
 (= c______ c!5))
(assert
 (let ((?x435 (left s_____)))
 (let ((?x573 (left s______)))
 (let (($x833 (= ?x573 ?x435)))
 (let ((?x469 (right s_____)))
 (let ((?x800 (top ?x469)))
 (let ((?x801 (stack ?x800 ?x435)))
 (let (($x827 (= ?x573 ?x801)))
 (let (($x803 ((_ is stack ) ?x469)))
 (let (($x806 (= c_____ right-to-left)))
 (let (($x807 (and $x806 $x803)))
 (let ((?x655 (center s_____)))
 (let (($x808 ((_ is stack ) ?x655)))
 (let (($x809 (= c_____ center-to-right)))
 (let (($x810 (and $x809 $x808)))
 (let ((?x796 (top ?x655)))
 (let ((?x797 (stack ?x796 ?x435)))
 (let (($x824 (= ?x573 ?x797)))
 (let (($x811 (= c_____ center-to-left)))
 (let (($x812 (and $x811 $x808)))
 (let ((?x606 (rest ?x435)))
 (let (($x820 (= ?x573 ?x606)))
 (let (($x813 ((_ is stack ) ?x435)))
 (let (($x814 (= c_____ left-to-right)))
 (let (($x815 (and $x814 $x813)))
 (let (($x816 (= c_____ left-to-center)))
 (let (($x817 (and $x816 $x813)))
 (ite $x817 $x820 (ite $x815 $x820 (ite $x812 $x824 (ite $x810 $x833 (ite $x807 $x827 $x833))))))))))))))))))))))))))))))))
(assert
 (let ((?x469 (right s_____)))
 (let ((?x607 (right s______)))
 (let (($x829 (= ?x607 ?x469)))
 (let ((?x799 (rest ?x469)))
 (let (($x826 (= ?x607 ?x799)))
 (let (($x803 ((_ is stack ) ?x469)))
 (let (($x804 (= c_____ right-to-center)))
 (let (($x805 (and $x804 $x803)))
 (let (($x806 (= c_____ right-to-left)))
 (let (($x807 (and $x806 $x803)))
 (let ((?x655 (center s_____)))
 (let ((?x796 (top ?x655)))
 (let ((?x798 (stack ?x796 ?x469)))
 (let (($x825 (= ?x607 ?x798)))
 (let (($x808 ((_ is stack ) ?x655)))
 (let (($x809 (= c_____ center-to-right)))
 (let (($x810 (and $x809 $x808)))
 (let (($x811 (= c_____ center-to-left)))
 (let (($x812 (and $x811 $x808)))
 (let ((?x435 (left s_____)))
 (let (($x813 ((_ is stack ) ?x435)))
 (let (($x814 (= c_____ left-to-right)))
 (let (($x815 (and $x814 $x813)))
 (let (($x846 (ite $x815 $x829 (ite $x812 $x829 (ite $x810 $x825 (ite $x807 $x826 (ite $x805 $x826 $x829)))))))
 (let ((?x653 (top ?x435)))
 (let ((?x792 (stack ?x653 ?x469)))
 (let (($x821 (= ?x607 ?x792)))
 (let (($x816 (= c_____ left-to-center)))
 (let (($x817 (and $x816 $x813)))
 (ite $x817 $x821 $x846)))))))))))))))))))))))))))))))
(assert
 (let ((?x655 (center s_____)))
 (let ((?x793 (center s______)))
 (let (($x831 (= ?x793 ?x655)))
 (let ((?x469 (right s_____)))
 (let ((?x800 (top ?x469)))
 (let ((?x802 (stack ?x800 ?x655)))
 (let (($x828 (= ?x793 ?x802)))
 (let (($x803 ((_ is stack ) ?x469)))
 (let (($x804 (= c_____ right-to-center)))
 (let (($x805 (and $x804 $x803)))
 (let (($x806 (= c_____ right-to-left)))
 (let (($x807 (and $x806 $x803)))
 (let ((?x795 (rest ?x655)))
 (let (($x823 (= ?x793 ?x795)))
 (let (($x808 ((_ is stack ) ?x655)))
 (let (($x809 (= c_____ center-to-right)))
 (let (($x810 (and $x809 $x808)))
 (let (($x811 (= c_____ center-to-left)))
 (let (($x812 (and $x811 $x808)))
 (let ((?x435 (left s_____)))
 (let ((?x653 (top ?x435)))
 (let ((?x794 (stack ?x653 ?x655)))
 (let (($x822 (= ?x793 ?x794)))
 (let (($x813 ((_ is stack ) ?x435)))
 (let (($x814 (= c_____ left-to-right)))
 (let (($x815 (and $x814 $x813)))
 (let (($x845 (ite $x815 $x822 (ite $x812 $x823 (ite $x810 $x823 (ite $x807 $x831 (ite $x805 $x828 $x831)))))))
 (let (($x816 (= c_____ left-to-center)))
 (let (($x817 (and $x816 $x813)))
 (ite $x817 $x831 $x845)))))))))))))))))))))))))))))))
(assert
 (= c_______ c!6))
(assert
 (let ((?x573 (left s______)))
 (let ((?x711 (left s_______)))
 (let (($x971 (= ?x711 ?x573)))
 (let ((?x607 (right s______)))
 (let ((?x938 (top ?x607)))
 (let ((?x939 (stack ?x938 ?x573)))
 (let (($x965 (= ?x711 ?x939)))
 (let (($x941 ((_ is stack ) ?x607)))
 (let (($x944 (= c______ right-to-left)))
 (let (($x945 (and $x944 $x941)))
 (let ((?x793 (center s______)))
 (let (($x946 ((_ is stack ) ?x793)))
 (let (($x947 (= c______ center-to-right)))
 (let (($x948 (and $x947 $x946)))
 (let ((?x934 (top ?x793)))
 (let ((?x935 (stack ?x934 ?x573)))
 (let (($x962 (= ?x711 ?x935)))
 (let (($x949 (= c______ center-to-left)))
 (let (($x950 (and $x949 $x946)))
 (let ((?x744 (rest ?x573)))
 (let (($x958 (= ?x711 ?x744)))
 (let (($x951 ((_ is stack ) ?x573)))
 (let (($x952 (= c______ left-to-right)))
 (let (($x953 (and $x952 $x951)))
 (let (($x954 (= c______ left-to-center)))
 (let (($x955 (and $x954 $x951)))
 (ite $x955 $x958 (ite $x953 $x958 (ite $x950 $x962 (ite $x948 $x971 (ite $x945 $x965 $x971))))))))))))))))))))))))))))))))
(assert
 (let ((?x607 (right s______)))
 (let ((?x745 (right s_______)))
 (let (($x967 (= ?x745 ?x607)))
 (let ((?x937 (rest ?x607)))
 (let (($x964 (= ?x745 ?x937)))
 (let (($x941 ((_ is stack ) ?x607)))
 (let (($x942 (= c______ right-to-center)))
 (let (($x943 (and $x942 $x941)))
 (let (($x944 (= c______ right-to-left)))
 (let (($x945 (and $x944 $x941)))
 (let ((?x793 (center s______)))
 (let ((?x934 (top ?x793)))
 (let ((?x936 (stack ?x934 ?x607)))
 (let (($x963 (= ?x745 ?x936)))
 (let (($x946 ((_ is stack ) ?x793)))
 (let (($x947 (= c______ center-to-right)))
 (let (($x948 (and $x947 $x946)))
 (let (($x949 (= c______ center-to-left)))
 (let (($x950 (and $x949 $x946)))
 (let ((?x573 (left s______)))
 (let (($x951 ((_ is stack ) ?x573)))
 (let (($x952 (= c______ left-to-right)))
 (let (($x953 (and $x952 $x951)))
 (let (($x984 (ite $x953 $x967 (ite $x950 $x967 (ite $x948 $x963 (ite $x945 $x964 (ite $x943 $x964 $x967)))))))
 (let ((?x791 (top ?x573)))
 (let ((?x930 (stack ?x791 ?x607)))
 (let (($x959 (= ?x745 ?x930)))
 (let (($x954 (= c______ left-to-center)))
 (let (($x955 (and $x954 $x951)))
 (ite $x955 $x959 $x984)))))))))))))))))))))))))))))))
(assert
 (let ((?x793 (center s______)))
 (let ((?x931 (center s_______)))
 (let (($x969 (= ?x931 ?x793)))
 (let ((?x607 (right s______)))
 (let ((?x938 (top ?x607)))
 (let ((?x940 (stack ?x938 ?x793)))
 (let (($x966 (= ?x931 ?x940)))
 (let (($x941 ((_ is stack ) ?x607)))
 (let (($x942 (= c______ right-to-center)))
 (let (($x943 (and $x942 $x941)))
 (let (($x944 (= c______ right-to-left)))
 (let (($x945 (and $x944 $x941)))
 (let ((?x933 (rest ?x793)))
 (let (($x961 (= ?x931 ?x933)))
 (let (($x946 ((_ is stack ) ?x793)))
 (let (($x947 (= c______ center-to-right)))
 (let (($x948 (and $x947 $x946)))
 (let (($x949 (= c______ center-to-left)))
 (let (($x950 (and $x949 $x946)))
 (let ((?x573 (left s______)))
 (let ((?x791 (top ?x573)))
 (let ((?x932 (stack ?x791 ?x793)))
 (let (($x960 (= ?x931 ?x932)))
 (let (($x951 ((_ is stack ) ?x573)))
 (let (($x952 (= c______ left-to-right)))
 (let (($x953 (and $x952 $x951)))
 (let (($x983 (ite $x953 $x960 (ite $x950 $x961 (ite $x948 $x961 (ite $x945 $x969 (ite $x943 $x966 $x969)))))))
 (let (($x954 (= c______ left-to-center)))
 (let (($x955 (and $x954 $x951)))
 (ite $x955 $x969 $x983)))))))))))))))))))))))))))))))
(assert
 (= c________ c!7))
(assert
 (let ((?x711 (left s_______)))
 (let ((?x849 (left s________)))
 (let (($x1109 (= ?x849 ?x711)))
 (let ((?x745 (right s_______)))
 (let ((?x1076 (top ?x745)))
 (let ((?x1077 (stack ?x1076 ?x711)))
 (let (($x1103 (= ?x849 ?x1077)))
 (let (($x1079 ((_ is stack ) ?x745)))
 (let (($x1082 (= c_______ right-to-left)))
 (let (($x1083 (and $x1082 $x1079)))
 (let ((?x931 (center s_______)))
 (let (($x1084 ((_ is stack ) ?x931)))
 (let (($x1085 (= c_______ center-to-right)))
 (let (($x1086 (and $x1085 $x1084)))
 (let ((?x1072 (top ?x931)))
 (let ((?x1073 (stack ?x1072 ?x711)))
 (let (($x1100 (= ?x849 ?x1073)))
 (let (($x1087 (= c_______ center-to-left)))
 (let (($x1088 (and $x1087 $x1084)))
 (let ((?x882 (rest ?x711)))
 (let (($x1096 (= ?x849 ?x882)))
 (let (($x1089 ((_ is stack ) ?x711)))
 (let (($x1090 (= c_______ left-to-right)))
 (let (($x1091 (and $x1090 $x1089)))
 (let (($x1092 (= c_______ left-to-center)))
 (let (($x1093 (and $x1092 $x1089)))
 (ite $x1093 $x1096 (ite $x1091 $x1096 (ite $x1088 $x1100 (ite $x1086 $x1109 (ite $x1083 $x1103 $x1109))))))))))))))))))))))))))))))))
(assert
 (let ((?x745 (right s_______)))
 (let ((?x883 (right s________)))
 (let (($x1105 (= ?x883 ?x745)))
 (let ((?x1075 (rest ?x745)))
 (let (($x1102 (= ?x883 ?x1075)))
 (let (($x1079 ((_ is stack ) ?x745)))
 (let (($x1080 (= c_______ right-to-center)))
 (let (($x1081 (and $x1080 $x1079)))
 (let (($x1082 (= c_______ right-to-left)))
 (let (($x1083 (and $x1082 $x1079)))
 (let ((?x931 (center s_______)))
 (let ((?x1072 (top ?x931)))
 (let ((?x1074 (stack ?x1072 ?x745)))
 (let (($x1101 (= ?x883 ?x1074)))
 (let (($x1084 ((_ is stack ) ?x931)))
 (let (($x1085 (= c_______ center-to-right)))
 (let (($x1086 (and $x1085 $x1084)))
 (let (($x1087 (= c_______ center-to-left)))
 (let (($x1088 (and $x1087 $x1084)))
 (let ((?x711 (left s_______)))
 (let (($x1089 ((_ is stack ) ?x711)))
 (let (($x1090 (= c_______ left-to-right)))
 (let (($x1091 (and $x1090 $x1089)))
 (let (($x1122 (ite $x1091 $x1105 (ite $x1088 $x1105 (ite $x1086 $x1101 (ite $x1083 $x1102 (ite $x1081 $x1102 $x1105)))))))
 (let ((?x929 (top ?x711)))
 (let ((?x1068 (stack ?x929 ?x745)))
 (let (($x1097 (= ?x883 ?x1068)))
 (let (($x1092 (= c_______ left-to-center)))
 (let (($x1093 (and $x1092 $x1089)))
 (ite $x1093 $x1097 $x1122)))))))))))))))))))))))))))))))
(assert
 (let ((?x931 (center s_______)))
 (let ((?x1069 (center s________)))
 (let (($x1107 (= ?x1069 ?x931)))
 (let ((?x745 (right s_______)))
 (let ((?x1076 (top ?x745)))
 (let ((?x1078 (stack ?x1076 ?x931)))
 (let (($x1104 (= ?x1069 ?x1078)))
 (let (($x1079 ((_ is stack ) ?x745)))
 (let (($x1080 (= c_______ right-to-center)))
 (let (($x1081 (and $x1080 $x1079)))
 (let (($x1082 (= c_______ right-to-left)))
 (let (($x1083 (and $x1082 $x1079)))
 (let ((?x1071 (rest ?x931)))
 (let (($x1099 (= ?x1069 ?x1071)))
 (let (($x1084 ((_ is stack ) ?x931)))
 (let (($x1085 (= c_______ center-to-right)))
 (let (($x1086 (and $x1085 $x1084)))
 (let (($x1087 (= c_______ center-to-left)))
 (let (($x1088 (and $x1087 $x1084)))
 (let ((?x711 (left s_______)))
 (let ((?x929 (top ?x711)))
 (let ((?x1070 (stack ?x929 ?x931)))
 (let (($x1098 (= ?x1069 ?x1070)))
 (let (($x1089 ((_ is stack ) ?x711)))
 (let (($x1090 (= c_______ left-to-right)))
 (let (($x1091 (and $x1090 $x1089)))
 (let (($x1121 (ite $x1091 $x1098 (ite $x1088 $x1099 (ite $x1086 $x1099 (ite $x1083 $x1107 (ite $x1081 $x1104 $x1107)))))))
 (let (($x1092 (= c_______ left-to-center)))
 (let (($x1093 (and $x1092 $x1089)))
 (ite $x1093 $x1107 $x1121)))))))))))))))))))))))))))))))
(assert
 (= c_________ c!8))
(assert
 (let ((?x849 (left s________)))
 (let ((?x987 (left s_________)))
 (let (($x1247 (= ?x987 ?x849)))
 (let ((?x883 (right s________)))
 (let ((?x1214 (top ?x883)))
 (let ((?x1215 (stack ?x1214 ?x849)))
 (let (($x1241 (= ?x987 ?x1215)))
 (let (($x1217 ((_ is stack ) ?x883)))
 (let (($x1220 (= c________ right-to-left)))
 (let (($x1221 (and $x1220 $x1217)))
 (let ((?x1069 (center s________)))
 (let (($x1222 ((_ is stack ) ?x1069)))
 (let (($x1223 (= c________ center-to-right)))
 (let (($x1224 (and $x1223 $x1222)))
 (let ((?x1210 (top ?x1069)))
 (let ((?x1211 (stack ?x1210 ?x849)))
 (let (($x1238 (= ?x987 ?x1211)))
 (let (($x1225 (= c________ center-to-left)))
 (let (($x1226 (and $x1225 $x1222)))
 (let ((?x1020 (rest ?x849)))
 (let (($x1234 (= ?x987 ?x1020)))
 (let (($x1227 ((_ is stack ) ?x849)))
 (let (($x1228 (= c________ left-to-right)))
 (let (($x1229 (and $x1228 $x1227)))
 (let (($x1230 (= c________ left-to-center)))
 (let (($x1231 (and $x1230 $x1227)))
 (ite $x1231 $x1234 (ite $x1229 $x1234 (ite $x1226 $x1238 (ite $x1224 $x1247 (ite $x1221 $x1241 $x1247))))))))))))))))))))))))))))))))
(assert
 (let ((?x883 (right s________)))
 (let ((?x1021 (right s_________)))
 (let (($x1243 (= ?x1021 ?x883)))
 (let ((?x1213 (rest ?x883)))
 (let (($x1240 (= ?x1021 ?x1213)))
 (let (($x1217 ((_ is stack ) ?x883)))
 (let (($x1218 (= c________ right-to-center)))
 (let (($x1219 (and $x1218 $x1217)))
 (let (($x1220 (= c________ right-to-left)))
 (let (($x1221 (and $x1220 $x1217)))
 (let ((?x1069 (center s________)))
 (let ((?x1210 (top ?x1069)))
 (let ((?x1212 (stack ?x1210 ?x883)))
 (let (($x1239 (= ?x1021 ?x1212)))
 (let (($x1222 ((_ is stack ) ?x1069)))
 (let (($x1223 (= c________ center-to-right)))
 (let (($x1224 (and $x1223 $x1222)))
 (let (($x1225 (= c________ center-to-left)))
 (let (($x1226 (and $x1225 $x1222)))
 (let ((?x849 (left s________)))
 (let (($x1227 ((_ is stack ) ?x849)))
 (let (($x1228 (= c________ left-to-right)))
 (let (($x1229 (and $x1228 $x1227)))
 (let (($x1260 (ite $x1229 $x1243 (ite $x1226 $x1243 (ite $x1224 $x1239 (ite $x1221 $x1240 (ite $x1219 $x1240 $x1243)))))))
 (let ((?x1067 (top ?x849)))
 (let ((?x1206 (stack ?x1067 ?x883)))
 (let (($x1235 (= ?x1021 ?x1206)))
 (let (($x1230 (= c________ left-to-center)))
 (let (($x1231 (and $x1230 $x1227)))
 (ite $x1231 $x1235 $x1260)))))))))))))))))))))))))))))))
(assert
 (let ((?x1069 (center s________)))
 (let ((?x1207 (center s_________)))
 (let (($x1245 (= ?x1207 ?x1069)))
 (let ((?x883 (right s________)))
 (let ((?x1214 (top ?x883)))
 (let ((?x1216 (stack ?x1214 ?x1069)))
 (let (($x1242 (= ?x1207 ?x1216)))
 (let (($x1217 ((_ is stack ) ?x883)))
 (let (($x1218 (= c________ right-to-center)))
 (let (($x1219 (and $x1218 $x1217)))
 (let (($x1220 (= c________ right-to-left)))
 (let (($x1221 (and $x1220 $x1217)))
 (let ((?x1209 (rest ?x1069)))
 (let (($x1237 (= ?x1207 ?x1209)))
 (let (($x1222 ((_ is stack ) ?x1069)))
 (let (($x1223 (= c________ center-to-right)))
 (let (($x1224 (and $x1223 $x1222)))
 (let (($x1225 (= c________ center-to-left)))
 (let (($x1226 (and $x1225 $x1222)))
 (let ((?x849 (left s________)))
 (let ((?x1067 (top ?x849)))
 (let ((?x1208 (stack ?x1067 ?x1069)))
 (let (($x1236 (= ?x1207 ?x1208)))
 (let (($x1227 ((_ is stack ) ?x849)))
 (let (($x1228 (= c________ left-to-right)))
 (let (($x1229 (and $x1228 $x1227)))
 (let (($x1259 (ite $x1229 $x1236 (ite $x1226 $x1237 (ite $x1224 $x1237 (ite $x1221 $x1245 (ite $x1219 $x1242 $x1245)))))))
 (let (($x1230 (= c________ left-to-center)))
 (let (($x1231 (and $x1230 $x1227)))
 (ite $x1231 $x1245 $x1259)))))))))))))))))))))))))))))))
(assert
 (= c__________ c!9))
(assert
 (let ((?x987 (left s_________)))
 (let ((?x1125 (left s__________)))
 (let (($x1385 (= ?x1125 ?x987)))
 (let ((?x1021 (right s_________)))
 (let ((?x1352 (top ?x1021)))
 (let ((?x1353 (stack ?x1352 ?x987)))
 (let (($x1379 (= ?x1125 ?x1353)))
 (let (($x1355 ((_ is stack ) ?x1021)))
 (let (($x1358 (= c_________ right-to-left)))
 (let (($x1359 (and $x1358 $x1355)))
 (let ((?x1207 (center s_________)))
 (let (($x1360 ((_ is stack ) ?x1207)))
 (let (($x1361 (= c_________ center-to-right)))
 (let (($x1362 (and $x1361 $x1360)))
 (let ((?x1348 (top ?x1207)))
 (let ((?x1349 (stack ?x1348 ?x987)))
 (let (($x1376 (= ?x1125 ?x1349)))
 (let (($x1363 (= c_________ center-to-left)))
 (let (($x1364 (and $x1363 $x1360)))
 (let ((?x1158 (rest ?x987)))
 (let (($x1372 (= ?x1125 ?x1158)))
 (let (($x1365 ((_ is stack ) ?x987)))
 (let (($x1366 (= c_________ left-to-right)))
 (let (($x1367 (and $x1366 $x1365)))
 (let (($x1368 (= c_________ left-to-center)))
 (let (($x1369 (and $x1368 $x1365)))
 (ite $x1369 $x1372 (ite $x1367 $x1372 (ite $x1364 $x1376 (ite $x1362 $x1385 (ite $x1359 $x1379 $x1385))))))))))))))))))))))))))))))))
(assert
 (let ((?x1021 (right s_________)))
 (let ((?x1159 (right s__________)))
 (let (($x1381 (= ?x1159 ?x1021)))
 (let ((?x1351 (rest ?x1021)))
 (let (($x1378 (= ?x1159 ?x1351)))
 (let (($x1355 ((_ is stack ) ?x1021)))
 (let (($x1356 (= c_________ right-to-center)))
 (let (($x1357 (and $x1356 $x1355)))
 (let (($x1358 (= c_________ right-to-left)))
 (let (($x1359 (and $x1358 $x1355)))
 (let ((?x1207 (center s_________)))
 (let ((?x1348 (top ?x1207)))
 (let ((?x1350 (stack ?x1348 ?x1021)))
 (let (($x1377 (= ?x1159 ?x1350)))
 (let (($x1360 ((_ is stack ) ?x1207)))
 (let (($x1361 (= c_________ center-to-right)))
 (let (($x1362 (and $x1361 $x1360)))
 (let (($x1363 (= c_________ center-to-left)))
 (let (($x1364 (and $x1363 $x1360)))
 (let ((?x987 (left s_________)))
 (let (($x1365 ((_ is stack ) ?x987)))
 (let (($x1366 (= c_________ left-to-right)))
 (let (($x1367 (and $x1366 $x1365)))
 (let (($x1398 (ite $x1367 $x1381 (ite $x1364 $x1381 (ite $x1362 $x1377 (ite $x1359 $x1378 (ite $x1357 $x1378 $x1381)))))))
 (let ((?x1205 (top ?x987)))
 (let ((?x1344 (stack ?x1205 ?x1021)))
 (let (($x1373 (= ?x1159 ?x1344)))
 (let (($x1368 (= c_________ left-to-center)))
 (let (($x1369 (and $x1368 $x1365)))
 (ite $x1369 $x1373 $x1398)))))))))))))))))))))))))))))))
(assert
 (let ((?x1207 (center s_________)))
 (let ((?x1345 (center s__________)))
 (let (($x1383 (= ?x1345 ?x1207)))
 (let ((?x1021 (right s_________)))
 (let ((?x1352 (top ?x1021)))
 (let ((?x1354 (stack ?x1352 ?x1207)))
 (let (($x1380 (= ?x1345 ?x1354)))
 (let (($x1355 ((_ is stack ) ?x1021)))
 (let (($x1356 (= c_________ right-to-center)))
 (let (($x1357 (and $x1356 $x1355)))
 (let (($x1358 (= c_________ right-to-left)))
 (let (($x1359 (and $x1358 $x1355)))
 (let ((?x1347 (rest ?x1207)))
 (let (($x1375 (= ?x1345 ?x1347)))
 (let (($x1360 ((_ is stack ) ?x1207)))
 (let (($x1361 (= c_________ center-to-right)))
 (let (($x1362 (and $x1361 $x1360)))
 (let (($x1363 (= c_________ center-to-left)))
 (let (($x1364 (and $x1363 $x1360)))
 (let ((?x987 (left s_________)))
 (let ((?x1205 (top ?x987)))
 (let ((?x1346 (stack ?x1205 ?x1207)))
 (let (($x1374 (= ?x1345 ?x1346)))
 (let (($x1365 ((_ is stack ) ?x987)))
 (let (($x1366 (= c_________ left-to-right)))
 (let (($x1367 (and $x1366 $x1365)))
 (let (($x1397 (ite $x1367 $x1374 (ite $x1364 $x1375 (ite $x1362 $x1375 (ite $x1359 $x1383 (ite $x1357 $x1380 $x1383)))))))
 (let (($x1368 (= c_________ left-to-center)))
 (let (($x1369 (and $x1368 $x1365)))
 (ite $x1369 $x1383 $x1397)))))))))))))))))))))))))))))))
(assert
 (let ((?x66 (stack L (stack K (stack I (stack G (stack F (stack C empty))))))))
 (let ((?x71 (stack T (stack S (stack R (stack Q (stack M ?x66)))))))
 (let ((?x1159 (right s__________)))
 (let (($x1401 (= ?x1159 ?x71)))
 (let ((?x60 (stack P (stack B (stack E empty)))))
 (let ((?x1345 (center s__________)))
 (let (($x1432 (= ?x1345 ?x60)))
 (let ((?x57 (stack J (stack D (stack A (stack O (stack N (stack H empty))))))))
 (let ((?x1125 (left s__________)))
 (let (($x1433 (= ?x1125 ?x57)))
 (let (($x1435 (not (and $x1433 $x1432 $x1401))))
 (not $x1435)))))))))))))
(check-sat)
