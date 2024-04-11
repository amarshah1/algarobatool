; 
(set-info :status unknown)
(set-logic QF_DT)
(declare-datatypes ((Enum_A_B_C_D_E_F_G_H_I_J_K_L_M_N 0)) (((A) (B) (C) (D) (E) (F) (G) (H) (I) (J) (K) (L) (M) (N))))
 (declare-datatypes ((Tower 0)) (((stack (top Enum_A_B_C_D_E_F_G_H_I_J_K_L_M_N) (rest Tower)) (empty))))
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
 (let ((?x27 (stack G empty)))
 (let ((?x26 (stack M ?x27)))
 (let ((?x99 (left s_tmp_)))
 (= ?x99 ?x26)))))
(assert
 (let ((?x28 (stack I empty)))
 (let ((?x102 (center s_tmp__)))
 (= ?x102 ?x28))))
(assert
 (let ((?x29 (stack A empty)))
 (let ((?x30 (stack B ?x29)))
 (let ((?x31 (stack C ?x30)))
 (let ((?x32 (stack D ?x31)))
 (let ((?x33 (stack E ?x32)))
 (let ((?x34 (stack F ?x33)))
 (let ((?x35 (stack H ?x34)))
 (let ((?x36 (stack J ?x35)))
 (let ((?x37 (stack K ?x36)))
 (let ((?x38 (stack L ?x37)))
 (let ((?x39 (stack N ?x38)))
 (let ((?x105 (right s_tmp___)))
 (= ?x105 ?x39))))))))))))))
(assert
 (= c_tmp____ c!0))
(assert
 (= s_ s_tmp___))
(assert
 (= c_ c_tmp____))
(assert
 (let ((?x132 (left s_tmp__)))
 (= (left s_tmp___) ?x132)))
(assert
 (let ((?x102 (center s_tmp__)))
 (= (center s_tmp___) ?x102)))
(assert
 (let ((?x136 (center s_tmp_)))
 (= ?x136 (center s_tmp))))
(assert
 (let ((?x139 (right s_tmp_)))
 (= ?x139 (right s_tmp))))
(assert
 (let ((?x99 (left s_tmp_)))
 (let ((?x132 (left s_tmp__)))
 (= ?x132 ?x99))))
(assert
 (let ((?x139 (right s_tmp_)))
 (let ((?x143 (right s_tmp__)))
 (= ?x143 ?x139))))
(assert
 (= c__ c!1))
(assert
 (let ((?x76 (left s_)))
 (let ((?x232 (left s__)))
 (let (($x274 (= ?x232 ?x76)))
 (let ((?x79 (right s_)))
 (let (($x244 ((_ is stack ) ?x79)))
 (let (($x247 (= c_ right-to-left)))
 (let (($x248 (and $x247 $x244)))
 (let ((?x81 (center s_)))
 (let (($x249 ((_ is stack ) ?x81)))
 (let (($x250 (= c_ center-to-right)))
 (let (($x251 (and $x250 $x249)))
 (let (($x252 (= c_ center-to-left)))
 (let (($x253 (and $x252 $x249)))
 (let (($x283 (ite $x253 (= ?x232 (stack (top ?x81) ?x76)) (ite $x251 $x274 (ite $x248 (= ?x232 (stack (top ?x79) ?x76)) $x274)))))
 (let (($x261 (= ?x232 (rest ?x76))))
 (let (($x254 ((_ is stack ) ?x76)))
 (let (($x255 (= c_ left-to-right)))
 (let (($x256 (and $x255 $x254)))
 (let (($x257 (= c_ left-to-center)))
 (let (($x258 (and $x257 $x254)))
 (ite $x258 $x261 (ite $x256 $x261 $x283)))))))))))))))))))))))
(assert
 (let ((?x79 (right s_)))
 (let ((?x240 (right s__)))
 (let (($x270 (= ?x240 ?x79)))
 (let (($x267 (= ?x240 (rest ?x79))))
 (let (($x244 ((_ is stack ) ?x79)))
 (let (($x245 (= c_ right-to-center)))
 (let (($x246 (and $x245 $x244)))
 (let (($x247 (= c_ right-to-left)))
 (let (($x248 (and $x247 $x244)))
 (let ((?x81 (center s_)))
 (let (($x249 ((_ is stack ) ?x81)))
 (let (($x250 (= c_ center-to-right)))
 (let (($x251 (and $x250 $x249)))
 (let (($x280 (ite $x251 (= ?x240 (stack (top ?x81) ?x79)) (ite $x248 $x267 (ite $x246 $x267 $x270)))))
 (let (($x252 (= c_ center-to-left)))
 (let (($x253 (and $x252 $x249)))
 (let ((?x76 (left s_)))
 (let (($x254 ((_ is stack ) ?x76)))
 (let (($x255 (= c_ left-to-right)))
 (let (($x256 (and $x255 $x254)))
 (let (($x257 (= c_ left-to-center)))
 (let (($x258 (and $x257 $x254)))
 (ite $x258 (= ?x240 (stack (top ?x76) ?x79)) (ite $x256 $x270 (ite $x253 $x270 $x280))))))))))))))))))))))))))
(assert
 (let ((?x81 (center s_)))
 (let ((?x228 (center s__)))
 (let (($x272 (= ?x228 ?x81)))
 (let ((?x79 (right s_)))
 (let (($x244 ((_ is stack ) ?x79)))
 (let (($x245 (= c_ right-to-center)))
 (let (($x246 (and $x245 $x244)))
 (let (($x247 (= c_ right-to-left)))
 (let (($x248 (and $x247 $x244)))
 (let (($x264 (= ?x228 (rest ?x81))))
 (let (($x249 ((_ is stack ) ?x81)))
 (let (($x250 (= c_ center-to-right)))
 (let (($x251 (and $x250 $x249)))
 (let (($x279 (ite $x251 $x264 (ite $x248 $x272 (ite $x246 (= ?x228 (stack (top ?x79) ?x81)) $x272)))))
 (let (($x252 (= c_ center-to-left)))
 (let (($x253 (and $x252 $x249)))
 (let ((?x76 (left s_)))
 (let (($x254 ((_ is stack ) ?x76)))
 (let (($x255 (= c_ left-to-right)))
 (let (($x256 (and $x255 $x254)))
 (let (($x257 (= c_ left-to-center)))
 (let (($x258 (and $x257 $x254)))
 (ite $x258 $x272 (ite $x256 (= ?x228 (stack (top ?x76) ?x81)) (ite $x253 $x264 $x279))))))))))))))))))))))))))
(assert
 (= c___ c!2))
(assert
 (let ((?x232 (left s__)))
 (let ((?x146 (left s___)))
 (let (($x403 (= ?x146 ?x232)))
 (let ((?x240 (right s__)))
 (let ((?x370 (top ?x240)))
 (let ((?x371 (stack ?x370 ?x232)))
 (let (($x397 (= ?x146 ?x371)))
 (let (($x373 ((_ is stack ) ?x240)))
 (let (($x376 (= c__ right-to-left)))
 (let (($x377 (and $x376 $x373)))
 (let ((?x228 (center s__)))
 (let (($x378 ((_ is stack ) ?x228)))
 (let (($x379 (= c__ center-to-right)))
 (let (($x380 (and $x379 $x378)))
 (let ((?x366 (top ?x228)))
 (let ((?x367 (stack ?x366 ?x232)))
 (let (($x394 (= ?x146 ?x367)))
 (let (($x381 (= c__ center-to-left)))
 (let (($x382 (and $x381 $x378)))
 (let ((?x148 (rest ?x232)))
 (let (($x390 (= ?x146 ?x148)))
 (let (($x383 ((_ is stack ) ?x232)))
 (let (($x384 (= c__ left-to-right)))
 (let (($x385 (and $x384 $x383)))
 (let (($x386 (= c__ left-to-center)))
 (let (($x387 (and $x386 $x383)))
 (ite $x387 $x390 (ite $x385 $x390 (ite $x382 $x394 (ite $x380 $x403 (ite $x377 $x397 $x403))))))))))))))))))))))))))))))))
(assert
 (let ((?x240 (right s__)))
 (let ((?x149 (right s___)))
 (let (($x399 (= ?x149 ?x240)))
 (let ((?x369 (rest ?x240)))
 (let (($x396 (= ?x149 ?x369)))
 (let (($x373 ((_ is stack ) ?x240)))
 (let (($x374 (= c__ right-to-center)))
 (let (($x375 (and $x374 $x373)))
 (let (($x376 (= c__ right-to-left)))
 (let (($x377 (and $x376 $x373)))
 (let ((?x228 (center s__)))
 (let ((?x366 (top ?x228)))
 (let ((?x368 (stack ?x366 ?x240)))
 (let (($x395 (= ?x149 ?x368)))
 (let (($x378 ((_ is stack ) ?x228)))
 (let (($x379 (= c__ center-to-right)))
 (let (($x380 (and $x379 $x378)))
 (let (($x381 (= c__ center-to-left)))
 (let (($x382 (and $x381 $x378)))
 (let ((?x232 (left s__)))
 (let (($x383 ((_ is stack ) ?x232)))
 (let (($x384 (= c__ left-to-right)))
 (let (($x385 (and $x384 $x383)))
 (let (($x416 (ite $x385 $x399 (ite $x382 $x399 (ite $x380 $x395 (ite $x377 $x396 (ite $x375 $x396 $x399)))))))
 (let ((?x226 (top ?x232)))
 (let ((?x362 (stack ?x226 ?x240)))
 (let (($x391 (= ?x149 ?x362)))
 (let (($x386 (= c__ left-to-center)))
 (let (($x387 (and $x386 $x383)))
 (ite $x387 $x391 $x416)))))))))))))))))))))))))))))))
(assert
 (let ((?x228 (center s__)))
 (let ((?x363 (center s___)))
 (let (($x401 (= ?x363 ?x228)))
 (let ((?x240 (right s__)))
 (let ((?x370 (top ?x240)))
 (let ((?x372 (stack ?x370 ?x228)))
 (let (($x398 (= ?x363 ?x372)))
 (let (($x373 ((_ is stack ) ?x240)))
 (let (($x374 (= c__ right-to-center)))
 (let (($x375 (and $x374 $x373)))
 (let (($x376 (= c__ right-to-left)))
 (let (($x377 (and $x376 $x373)))
 (let ((?x365 (rest ?x228)))
 (let (($x393 (= ?x363 ?x365)))
 (let (($x378 ((_ is stack ) ?x228)))
 (let (($x379 (= c__ center-to-right)))
 (let (($x380 (and $x379 $x378)))
 (let (($x381 (= c__ center-to-left)))
 (let (($x382 (and $x381 $x378)))
 (let ((?x232 (left s__)))
 (let ((?x226 (top ?x232)))
 (let ((?x364 (stack ?x226 ?x228)))
 (let (($x392 (= ?x363 ?x364)))
 (let (($x383 ((_ is stack ) ?x232)))
 (let (($x384 (= c__ left-to-right)))
 (let (($x385 (and $x384 $x383)))
 (let (($x415 (ite $x385 $x392 (ite $x382 $x393 (ite $x380 $x393 (ite $x377 $x401 (ite $x375 $x398 $x401)))))))
 (let (($x386 (= c__ left-to-center)))
 (let (($x387 (and $x386 $x383)))
 (ite $x387 $x401 $x415)))))))))))))))))))))))))))))))
(assert
 (= c____ c!3))
(assert
 (let ((?x146 (left s___)))
 (let ((?x290 (left s____)))
 (let (($x541 (= ?x290 ?x146)))
 (let ((?x149 (right s___)))
 (let ((?x508 (top ?x149)))
 (let ((?x509 (stack ?x508 ?x146)))
 (let (($x535 (= ?x290 ?x509)))
 (let (($x511 ((_ is stack ) ?x149)))
 (let (($x514 (= c___ right-to-left)))
 (let (($x515 (and $x514 $x511)))
 (let ((?x363 (center s___)))
 (let (($x516 ((_ is stack ) ?x363)))
 (let (($x517 (= c___ center-to-right)))
 (let (($x518 (and $x517 $x516)))
 (let ((?x504 (top ?x363)))
 (let ((?x505 (stack ?x504 ?x146)))
 (let (($x532 (= ?x290 ?x505)))
 (let (($x519 (= c___ center-to-left)))
 (let (($x520 (and $x519 $x516)))
 (let ((?x334 (rest ?x146)))
 (let (($x528 (= ?x290 ?x334)))
 (let (($x521 ((_ is stack ) ?x146)))
 (let (($x522 (= c___ left-to-right)))
 (let (($x523 (and $x522 $x521)))
 (let (($x524 (= c___ left-to-center)))
 (let (($x525 (and $x524 $x521)))
 (ite $x525 $x528 (ite $x523 $x528 (ite $x520 $x532 (ite $x518 $x541 (ite $x515 $x535 $x541))))))))))))))))))))))))))))))))
(assert
 (let ((?x149 (right s___)))
 (let ((?x335 (right s____)))
 (let (($x537 (= ?x335 ?x149)))
 (let ((?x507 (rest ?x149)))
 (let (($x534 (= ?x335 ?x507)))
 (let (($x511 ((_ is stack ) ?x149)))
 (let (($x512 (= c___ right-to-center)))
 (let (($x513 (and $x512 $x511)))
 (let (($x514 (= c___ right-to-left)))
 (let (($x515 (and $x514 $x511)))
 (let ((?x363 (center s___)))
 (let ((?x504 (top ?x363)))
 (let ((?x506 (stack ?x504 ?x149)))
 (let (($x533 (= ?x335 ?x506)))
 (let (($x516 ((_ is stack ) ?x363)))
 (let (($x517 (= c___ center-to-right)))
 (let (($x518 (and $x517 $x516)))
 (let (($x519 (= c___ center-to-left)))
 (let (($x520 (and $x519 $x516)))
 (let ((?x146 (left s___)))
 (let (($x521 ((_ is stack ) ?x146)))
 (let (($x522 (= c___ left-to-right)))
 (let (($x523 (and $x522 $x521)))
 (let (($x554 (ite $x523 $x537 (ite $x520 $x537 (ite $x518 $x533 (ite $x515 $x534 (ite $x513 $x534 $x537)))))))
 (let ((?x361 (top ?x146)))
 (let ((?x500 (stack ?x361 ?x149)))
 (let (($x529 (= ?x335 ?x500)))
 (let (($x524 (= c___ left-to-center)))
 (let (($x525 (and $x524 $x521)))
 (ite $x525 $x529 $x554)))))))))))))))))))))))))))))))
(assert
 (let ((?x363 (center s___)))
 (let ((?x501 (center s____)))
 (let (($x539 (= ?x501 ?x363)))
 (let ((?x149 (right s___)))
 (let ((?x508 (top ?x149)))
 (let ((?x510 (stack ?x508 ?x363)))
 (let (($x536 (= ?x501 ?x510)))
 (let (($x511 ((_ is stack ) ?x149)))
 (let (($x512 (= c___ right-to-center)))
 (let (($x513 (and $x512 $x511)))
 (let (($x514 (= c___ right-to-left)))
 (let (($x515 (and $x514 $x511)))
 (let ((?x503 (rest ?x363)))
 (let (($x531 (= ?x501 ?x503)))
 (let (($x516 ((_ is stack ) ?x363)))
 (let (($x517 (= c___ center-to-right)))
 (let (($x518 (and $x517 $x516)))
 (let (($x519 (= c___ center-to-left)))
 (let (($x520 (and $x519 $x516)))
 (let ((?x146 (left s___)))
 (let ((?x361 (top ?x146)))
 (let ((?x502 (stack ?x361 ?x363)))
 (let (($x530 (= ?x501 ?x502)))
 (let (($x521 ((_ is stack ) ?x146)))
 (let (($x522 (= c___ left-to-right)))
 (let (($x523 (and $x522 $x521)))
 (let (($x553 (ite $x523 $x530 (ite $x520 $x531 (ite $x518 $x531 (ite $x515 $x539 (ite $x513 $x536 $x539)))))))
 (let (($x524 (= c___ left-to-center)))
 (let (($x525 (and $x524 $x521)))
 (ite $x525 $x539 $x553)))))))))))))))))))))))))))))))
(assert
 (= c_____ c!4))
(assert
 (let ((?x290 (left s____)))
 (let ((?x419 (left s_____)))
 (let (($x679 (= ?x419 ?x290)))
 (let ((?x335 (right s____)))
 (let ((?x646 (top ?x335)))
 (let ((?x647 (stack ?x646 ?x290)))
 (let (($x673 (= ?x419 ?x647)))
 (let (($x649 ((_ is stack ) ?x335)))
 (let (($x652 (= c____ right-to-left)))
 (let (($x653 (and $x652 $x649)))
 (let ((?x501 (center s____)))
 (let (($x654 ((_ is stack ) ?x501)))
 (let (($x655 (= c____ center-to-right)))
 (let (($x656 (and $x655 $x654)))
 (let ((?x642 (top ?x501)))
 (let ((?x643 (stack ?x642 ?x290)))
 (let (($x670 (= ?x419 ?x643)))
 (let (($x657 (= c____ center-to-left)))
 (let (($x658 (and $x657 $x654)))
 (let ((?x452 (rest ?x290)))
 (let (($x666 (= ?x419 ?x452)))
 (let (($x659 ((_ is stack ) ?x290)))
 (let (($x660 (= c____ left-to-right)))
 (let (($x661 (and $x660 $x659)))
 (let (($x662 (= c____ left-to-center)))
 (let (($x663 (and $x662 $x659)))
 (ite $x663 $x666 (ite $x661 $x666 (ite $x658 $x670 (ite $x656 $x679 (ite $x653 $x673 $x679))))))))))))))))))))))))))))))))
(assert
 (let ((?x335 (right s____)))
 (let ((?x453 (right s_____)))
 (let (($x675 (= ?x453 ?x335)))
 (let ((?x645 (rest ?x335)))
 (let (($x672 (= ?x453 ?x645)))
 (let (($x649 ((_ is stack ) ?x335)))
 (let (($x650 (= c____ right-to-center)))
 (let (($x651 (and $x650 $x649)))
 (let (($x652 (= c____ right-to-left)))
 (let (($x653 (and $x652 $x649)))
 (let ((?x501 (center s____)))
 (let ((?x642 (top ?x501)))
 (let ((?x644 (stack ?x642 ?x335)))
 (let (($x671 (= ?x453 ?x644)))
 (let (($x654 ((_ is stack ) ?x501)))
 (let (($x655 (= c____ center-to-right)))
 (let (($x656 (and $x655 $x654)))
 (let (($x657 (= c____ center-to-left)))
 (let (($x658 (and $x657 $x654)))
 (let ((?x290 (left s____)))
 (let (($x659 ((_ is stack ) ?x290)))
 (let (($x660 (= c____ left-to-right)))
 (let (($x661 (and $x660 $x659)))
 (let (($x692 (ite $x661 $x675 (ite $x658 $x675 (ite $x656 $x671 (ite $x653 $x672 (ite $x651 $x672 $x675)))))))
 (let ((?x499 (top ?x290)))
 (let ((?x638 (stack ?x499 ?x335)))
 (let (($x667 (= ?x453 ?x638)))
 (let (($x662 (= c____ left-to-center)))
 (let (($x663 (and $x662 $x659)))
 (ite $x663 $x667 $x692)))))))))))))))))))))))))))))))
(assert
 (let ((?x501 (center s____)))
 (let ((?x639 (center s_____)))
 (let (($x677 (= ?x639 ?x501)))
 (let ((?x335 (right s____)))
 (let ((?x646 (top ?x335)))
 (let ((?x648 (stack ?x646 ?x501)))
 (let (($x674 (= ?x639 ?x648)))
 (let (($x649 ((_ is stack ) ?x335)))
 (let (($x650 (= c____ right-to-center)))
 (let (($x651 (and $x650 $x649)))
 (let (($x652 (= c____ right-to-left)))
 (let (($x653 (and $x652 $x649)))
 (let ((?x641 (rest ?x501)))
 (let (($x669 (= ?x639 ?x641)))
 (let (($x654 ((_ is stack ) ?x501)))
 (let (($x655 (= c____ center-to-right)))
 (let (($x656 (and $x655 $x654)))
 (let (($x657 (= c____ center-to-left)))
 (let (($x658 (and $x657 $x654)))
 (let ((?x290 (left s____)))
 (let ((?x499 (top ?x290)))
 (let ((?x640 (stack ?x499 ?x501)))
 (let (($x668 (= ?x639 ?x640)))
 (let (($x659 ((_ is stack ) ?x290)))
 (let (($x660 (= c____ left-to-right)))
 (let (($x661 (and $x660 $x659)))
 (let (($x691 (ite $x661 $x668 (ite $x658 $x669 (ite $x656 $x669 (ite $x653 $x677 (ite $x651 $x674 $x677)))))))
 (let (($x662 (= c____ left-to-center)))
 (let (($x663 (and $x662 $x659)))
 (ite $x663 $x677 $x691)))))))))))))))))))))))))))))))
(assert
 (= c______ c!5))
(assert
 (let ((?x419 (left s_____)))
 (let ((?x557 (left s______)))
 (let (($x817 (= ?x557 ?x419)))
 (let ((?x453 (right s_____)))
 (let ((?x784 (top ?x453)))
 (let ((?x785 (stack ?x784 ?x419)))
 (let (($x811 (= ?x557 ?x785)))
 (let (($x787 ((_ is stack ) ?x453)))
 (let (($x790 (= c_____ right-to-left)))
 (let (($x791 (and $x790 $x787)))
 (let ((?x639 (center s_____)))
 (let (($x792 ((_ is stack ) ?x639)))
 (let (($x793 (= c_____ center-to-right)))
 (let (($x794 (and $x793 $x792)))
 (let ((?x780 (top ?x639)))
 (let ((?x781 (stack ?x780 ?x419)))
 (let (($x808 (= ?x557 ?x781)))
 (let (($x795 (= c_____ center-to-left)))
 (let (($x796 (and $x795 $x792)))
 (let ((?x590 (rest ?x419)))
 (let (($x804 (= ?x557 ?x590)))
 (let (($x797 ((_ is stack ) ?x419)))
 (let (($x798 (= c_____ left-to-right)))
 (let (($x799 (and $x798 $x797)))
 (let (($x800 (= c_____ left-to-center)))
 (let (($x801 (and $x800 $x797)))
 (ite $x801 $x804 (ite $x799 $x804 (ite $x796 $x808 (ite $x794 $x817 (ite $x791 $x811 $x817))))))))))))))))))))))))))))))))
(assert
 (let ((?x453 (right s_____)))
 (let ((?x591 (right s______)))
 (let (($x813 (= ?x591 ?x453)))
 (let ((?x783 (rest ?x453)))
 (let (($x810 (= ?x591 ?x783)))
 (let (($x787 ((_ is stack ) ?x453)))
 (let (($x788 (= c_____ right-to-center)))
 (let (($x789 (and $x788 $x787)))
 (let (($x790 (= c_____ right-to-left)))
 (let (($x791 (and $x790 $x787)))
 (let ((?x639 (center s_____)))
 (let ((?x780 (top ?x639)))
 (let ((?x782 (stack ?x780 ?x453)))
 (let (($x809 (= ?x591 ?x782)))
 (let (($x792 ((_ is stack ) ?x639)))
 (let (($x793 (= c_____ center-to-right)))
 (let (($x794 (and $x793 $x792)))
 (let (($x795 (= c_____ center-to-left)))
 (let (($x796 (and $x795 $x792)))
 (let ((?x419 (left s_____)))
 (let (($x797 ((_ is stack ) ?x419)))
 (let (($x798 (= c_____ left-to-right)))
 (let (($x799 (and $x798 $x797)))
 (let (($x830 (ite $x799 $x813 (ite $x796 $x813 (ite $x794 $x809 (ite $x791 $x810 (ite $x789 $x810 $x813)))))))
 (let ((?x637 (top ?x419)))
 (let ((?x776 (stack ?x637 ?x453)))
 (let (($x805 (= ?x591 ?x776)))
 (let (($x800 (= c_____ left-to-center)))
 (let (($x801 (and $x800 $x797)))
 (ite $x801 $x805 $x830)))))))))))))))))))))))))))))))
(assert
 (let ((?x639 (center s_____)))
 (let ((?x777 (center s______)))
 (let (($x815 (= ?x777 ?x639)))
 (let ((?x453 (right s_____)))
 (let ((?x784 (top ?x453)))
 (let ((?x786 (stack ?x784 ?x639)))
 (let (($x812 (= ?x777 ?x786)))
 (let (($x787 ((_ is stack ) ?x453)))
 (let (($x788 (= c_____ right-to-center)))
 (let (($x789 (and $x788 $x787)))
 (let (($x790 (= c_____ right-to-left)))
 (let (($x791 (and $x790 $x787)))
 (let ((?x779 (rest ?x639)))
 (let (($x807 (= ?x777 ?x779)))
 (let (($x792 ((_ is stack ) ?x639)))
 (let (($x793 (= c_____ center-to-right)))
 (let (($x794 (and $x793 $x792)))
 (let (($x795 (= c_____ center-to-left)))
 (let (($x796 (and $x795 $x792)))
 (let ((?x419 (left s_____)))
 (let ((?x637 (top ?x419)))
 (let ((?x778 (stack ?x637 ?x639)))
 (let (($x806 (= ?x777 ?x778)))
 (let (($x797 ((_ is stack ) ?x419)))
 (let (($x798 (= c_____ left-to-right)))
 (let (($x799 (and $x798 $x797)))
 (let (($x829 (ite $x799 $x806 (ite $x796 $x807 (ite $x794 $x807 (ite $x791 $x815 (ite $x789 $x812 $x815)))))))
 (let (($x800 (= c_____ left-to-center)))
 (let (($x801 (and $x800 $x797)))
 (ite $x801 $x815 $x829)))))))))))))))))))))))))))))))
(assert
 (= c_______ c!6))
(assert
 (let ((?x557 (left s______)))
 (let ((?x695 (left s_______)))
 (let (($x955 (= ?x695 ?x557)))
 (let ((?x591 (right s______)))
 (let ((?x922 (top ?x591)))
 (let ((?x923 (stack ?x922 ?x557)))
 (let (($x949 (= ?x695 ?x923)))
 (let (($x925 ((_ is stack ) ?x591)))
 (let (($x928 (= c______ right-to-left)))
 (let (($x929 (and $x928 $x925)))
 (let ((?x777 (center s______)))
 (let (($x930 ((_ is stack ) ?x777)))
 (let (($x931 (= c______ center-to-right)))
 (let (($x932 (and $x931 $x930)))
 (let ((?x918 (top ?x777)))
 (let ((?x919 (stack ?x918 ?x557)))
 (let (($x946 (= ?x695 ?x919)))
 (let (($x933 (= c______ center-to-left)))
 (let (($x934 (and $x933 $x930)))
 (let ((?x728 (rest ?x557)))
 (let (($x942 (= ?x695 ?x728)))
 (let (($x935 ((_ is stack ) ?x557)))
 (let (($x936 (= c______ left-to-right)))
 (let (($x937 (and $x936 $x935)))
 (let (($x938 (= c______ left-to-center)))
 (let (($x939 (and $x938 $x935)))
 (ite $x939 $x942 (ite $x937 $x942 (ite $x934 $x946 (ite $x932 $x955 (ite $x929 $x949 $x955))))))))))))))))))))))))))))))))
(assert
 (let ((?x591 (right s______)))
 (let ((?x729 (right s_______)))
 (let (($x951 (= ?x729 ?x591)))
 (let ((?x921 (rest ?x591)))
 (let (($x948 (= ?x729 ?x921)))
 (let (($x925 ((_ is stack ) ?x591)))
 (let (($x926 (= c______ right-to-center)))
 (let (($x927 (and $x926 $x925)))
 (let (($x928 (= c______ right-to-left)))
 (let (($x929 (and $x928 $x925)))
 (let ((?x777 (center s______)))
 (let ((?x918 (top ?x777)))
 (let ((?x920 (stack ?x918 ?x591)))
 (let (($x947 (= ?x729 ?x920)))
 (let (($x930 ((_ is stack ) ?x777)))
 (let (($x931 (= c______ center-to-right)))
 (let (($x932 (and $x931 $x930)))
 (let (($x933 (= c______ center-to-left)))
 (let (($x934 (and $x933 $x930)))
 (let ((?x557 (left s______)))
 (let (($x935 ((_ is stack ) ?x557)))
 (let (($x936 (= c______ left-to-right)))
 (let (($x937 (and $x936 $x935)))
 (let (($x968 (ite $x937 $x951 (ite $x934 $x951 (ite $x932 $x947 (ite $x929 $x948 (ite $x927 $x948 $x951)))))))
 (let ((?x775 (top ?x557)))
 (let ((?x914 (stack ?x775 ?x591)))
 (let (($x943 (= ?x729 ?x914)))
 (let (($x938 (= c______ left-to-center)))
 (let (($x939 (and $x938 $x935)))
 (ite $x939 $x943 $x968)))))))))))))))))))))))))))))))
(assert
 (let ((?x777 (center s______)))
 (let ((?x915 (center s_______)))
 (let (($x953 (= ?x915 ?x777)))
 (let ((?x591 (right s______)))
 (let ((?x922 (top ?x591)))
 (let ((?x924 (stack ?x922 ?x777)))
 (let (($x950 (= ?x915 ?x924)))
 (let (($x925 ((_ is stack ) ?x591)))
 (let (($x926 (= c______ right-to-center)))
 (let (($x927 (and $x926 $x925)))
 (let (($x928 (= c______ right-to-left)))
 (let (($x929 (and $x928 $x925)))
 (let ((?x917 (rest ?x777)))
 (let (($x945 (= ?x915 ?x917)))
 (let (($x930 ((_ is stack ) ?x777)))
 (let (($x931 (= c______ center-to-right)))
 (let (($x932 (and $x931 $x930)))
 (let (($x933 (= c______ center-to-left)))
 (let (($x934 (and $x933 $x930)))
 (let ((?x557 (left s______)))
 (let ((?x775 (top ?x557)))
 (let ((?x916 (stack ?x775 ?x777)))
 (let (($x944 (= ?x915 ?x916)))
 (let (($x935 ((_ is stack ) ?x557)))
 (let (($x936 (= c______ left-to-right)))
 (let (($x937 (and $x936 $x935)))
 (let (($x967 (ite $x937 $x944 (ite $x934 $x945 (ite $x932 $x945 (ite $x929 $x953 (ite $x927 $x950 $x953)))))))
 (let (($x938 (= c______ left-to-center)))
 (let (($x939 (and $x938 $x935)))
 (ite $x939 $x953 $x967)))))))))))))))))))))))))))))))
(assert
 (= c________ c!7))
(assert
 (let ((?x695 (left s_______)))
 (let ((?x833 (left s________)))
 (let (($x1093 (= ?x833 ?x695)))
 (let ((?x729 (right s_______)))
 (let ((?x1060 (top ?x729)))
 (let ((?x1061 (stack ?x1060 ?x695)))
 (let (($x1087 (= ?x833 ?x1061)))
 (let (($x1063 ((_ is stack ) ?x729)))
 (let (($x1066 (= c_______ right-to-left)))
 (let (($x1067 (and $x1066 $x1063)))
 (let ((?x915 (center s_______)))
 (let (($x1068 ((_ is stack ) ?x915)))
 (let (($x1069 (= c_______ center-to-right)))
 (let (($x1070 (and $x1069 $x1068)))
 (let ((?x1056 (top ?x915)))
 (let ((?x1057 (stack ?x1056 ?x695)))
 (let (($x1084 (= ?x833 ?x1057)))
 (let (($x1071 (= c_______ center-to-left)))
 (let (($x1072 (and $x1071 $x1068)))
 (let ((?x866 (rest ?x695)))
 (let (($x1080 (= ?x833 ?x866)))
 (let (($x1073 ((_ is stack ) ?x695)))
 (let (($x1074 (= c_______ left-to-right)))
 (let (($x1075 (and $x1074 $x1073)))
 (let (($x1076 (= c_______ left-to-center)))
 (let (($x1077 (and $x1076 $x1073)))
 (ite $x1077 $x1080 (ite $x1075 $x1080 (ite $x1072 $x1084 (ite $x1070 $x1093 (ite $x1067 $x1087 $x1093))))))))))))))))))))))))))))))))
(assert
 (let ((?x729 (right s_______)))
 (let ((?x867 (right s________)))
 (let (($x1089 (= ?x867 ?x729)))
 (let ((?x1059 (rest ?x729)))
 (let (($x1086 (= ?x867 ?x1059)))
 (let (($x1063 ((_ is stack ) ?x729)))
 (let (($x1064 (= c_______ right-to-center)))
 (let (($x1065 (and $x1064 $x1063)))
 (let (($x1066 (= c_______ right-to-left)))
 (let (($x1067 (and $x1066 $x1063)))
 (let ((?x915 (center s_______)))
 (let ((?x1056 (top ?x915)))
 (let ((?x1058 (stack ?x1056 ?x729)))
 (let (($x1085 (= ?x867 ?x1058)))
 (let (($x1068 ((_ is stack ) ?x915)))
 (let (($x1069 (= c_______ center-to-right)))
 (let (($x1070 (and $x1069 $x1068)))
 (let (($x1071 (= c_______ center-to-left)))
 (let (($x1072 (and $x1071 $x1068)))
 (let ((?x695 (left s_______)))
 (let (($x1073 ((_ is stack ) ?x695)))
 (let (($x1074 (= c_______ left-to-right)))
 (let (($x1075 (and $x1074 $x1073)))
 (let (($x1106 (ite $x1075 $x1089 (ite $x1072 $x1089 (ite $x1070 $x1085 (ite $x1067 $x1086 (ite $x1065 $x1086 $x1089)))))))
 (let ((?x913 (top ?x695)))
 (let ((?x1052 (stack ?x913 ?x729)))
 (let (($x1081 (= ?x867 ?x1052)))
 (let (($x1076 (= c_______ left-to-center)))
 (let (($x1077 (and $x1076 $x1073)))
 (ite $x1077 $x1081 $x1106)))))))))))))))))))))))))))))))
(assert
 (let ((?x915 (center s_______)))
 (let ((?x1053 (center s________)))
 (let (($x1091 (= ?x1053 ?x915)))
 (let ((?x729 (right s_______)))
 (let ((?x1060 (top ?x729)))
 (let ((?x1062 (stack ?x1060 ?x915)))
 (let (($x1088 (= ?x1053 ?x1062)))
 (let (($x1063 ((_ is stack ) ?x729)))
 (let (($x1064 (= c_______ right-to-center)))
 (let (($x1065 (and $x1064 $x1063)))
 (let (($x1066 (= c_______ right-to-left)))
 (let (($x1067 (and $x1066 $x1063)))
 (let ((?x1055 (rest ?x915)))
 (let (($x1083 (= ?x1053 ?x1055)))
 (let (($x1068 ((_ is stack ) ?x915)))
 (let (($x1069 (= c_______ center-to-right)))
 (let (($x1070 (and $x1069 $x1068)))
 (let (($x1071 (= c_______ center-to-left)))
 (let (($x1072 (and $x1071 $x1068)))
 (let ((?x695 (left s_______)))
 (let ((?x913 (top ?x695)))
 (let ((?x1054 (stack ?x913 ?x915)))
 (let (($x1082 (= ?x1053 ?x1054)))
 (let (($x1073 ((_ is stack ) ?x695)))
 (let (($x1074 (= c_______ left-to-right)))
 (let (($x1075 (and $x1074 $x1073)))
 (let (($x1105 (ite $x1075 $x1082 (ite $x1072 $x1083 (ite $x1070 $x1083 (ite $x1067 $x1091 (ite $x1065 $x1088 $x1091)))))))
 (let (($x1076 (= c_______ left-to-center)))
 (let (($x1077 (and $x1076 $x1073)))
 (ite $x1077 $x1091 $x1105)))))))))))))))))))))))))))))))
(assert
 (let ((?x52 (stack M (stack F empty))))
 (let ((?x867 (right s________)))
 (let (($x1109 (= ?x867 ?x52)))
 (let ((?x50 (stack B (stack C (stack I empty)))))
 (let ((?x1053 (center s________)))
 (let (($x1140 (= ?x1053 ?x50)))
 (let ((?x45 (stack H (stack L (stack A (stack J (stack E (stack N empty))))))))
 (let ((?x48 (stack G (stack D (stack K ?x45)))))
 (let ((?x833 (left s________)))
 (let (($x1141 (= ?x833 ?x48)))
 (let (($x1143 (not (and $x1141 $x1140 $x1109))))
 (not $x1143)))))))))))))
(check-sat)