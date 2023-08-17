; 
(set-info :status unknown)
(set-logic QF_DT)
(declare-datatypes ((Enum_A_B_C_D_E_F_G_H_I_J 0)) (((A) (B) (C) (D) (E) (F) (G) (H) (I) (J))))
 (declare-datatypes ((Tower 0)) (((stack (top Enum_A_B_C_D_E_F_G_H_I_J) (rest Tower)) (empty))))
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
(assert
 (= s_tmp s))
(assert
 (= c_tmp c))
(assert
 (let ((?x88 (left s_tmp_)))
 (= ?x88 empty)))
(assert
 (let ((?x23 (stack D empty)))
 (let ((?x22 (stack G ?x23)))
 (let ((?x24 (stack F ?x22)))
 (let ((?x25 (stack A ?x24)))
 (let ((?x26 (stack H ?x25)))
 (let ((?x27 (stack C ?x26)))
 (let ((?x28 (stack I ?x27)))
 (let ((?x91 (center s_tmp__)))
 (= ?x91 ?x28))))))))))
(assert
 (let ((?x29 (stack B empty)))
 (let ((?x30 (stack E ?x29)))
 (let ((?x31 (stack J ?x30)))
 (let ((?x94 (right s_tmp___)))
 (= ?x94 ?x31))))))
(assert
 (= c_tmp____ c!0))
(assert
 (= s_ s_tmp___))
(assert
 (= c_ c_tmp____))
(assert
 (let ((?x121 (left s_tmp__)))
 (= (left s_tmp___) ?x121)))
(assert
 (let ((?x91 (center s_tmp__)))
 (= (center s_tmp___) ?x91)))
(assert
 (let ((?x125 (center s_tmp_)))
 (= ?x125 (center s_tmp))))
(assert
 (let ((?x128 (right s_tmp_)))
 (= ?x128 (right s_tmp))))
(assert
 (let ((?x88 (left s_tmp_)))
 (let ((?x121 (left s_tmp__)))
 (= ?x121 ?x88))))
(assert
 (let ((?x128 (right s_tmp_)))
 (let ((?x132 (right s_tmp__)))
 (= ?x132 ?x128))))
(assert
 (= c__ c!1))
(assert
 (let ((?x65 (left s_)))
 (let ((?x206 (left s__)))
 (let (($x251 (= ?x206 ?x65)))
 (let ((?x68 (right s_)))
 (let (($x221 ((_ is stack ) ?x68)))
 (let (($x224 (= c_ right-to-left)))
 (let (($x225 (and $x224 $x221)))
 (let ((?x70 (center s_)))
 (let (($x226 ((_ is stack ) ?x70)))
 (let (($x227 (= c_ center-to-right)))
 (let (($x228 (and $x227 $x226)))
 (let (($x229 (= c_ center-to-left)))
 (let (($x230 (and $x229 $x226)))
 (let (($x260 (ite $x230 (= ?x206 (stack (top ?x70) ?x65)) (ite $x228 $x251 (ite $x225 (= ?x206 (stack (top ?x68) ?x65)) $x251)))))
 (let (($x238 (= ?x206 (rest ?x65))))
 (let (($x231 ((_ is stack ) ?x65)))
 (let (($x233 (and (= c_ left-to-right) $x231)))
 (let (($x235 (and (= c_ left-to-center) $x231)))
 (ite $x235 $x238 (ite $x233 $x238 $x260)))))))))))))))))))))
(assert
 (let ((?x68 (right s_)))
 (let ((?x208 (right s__)))
 (let (($x247 (= ?x208 ?x68)))
 (let (($x244 (= ?x208 (rest ?x68))))
 (let (($x221 ((_ is stack ) ?x68)))
 (let (($x222 (= c_ right-to-center)))
 (let (($x223 (and $x222 $x221)))
 (let (($x224 (= c_ right-to-left)))
 (let (($x225 (and $x224 $x221)))
 (let ((?x70 (center s_)))
 (let (($x226 ((_ is stack ) ?x70)))
 (let (($x227 (= c_ center-to-right)))
 (let (($x228 (and $x227 $x226)))
 (let (($x257 (ite $x228 (= ?x208 (stack (top ?x70) ?x68)) (ite $x225 $x244 (ite $x223 $x244 $x247)))))
 (let (($x229 (= c_ center-to-left)))
 (let (($x230 (and $x229 $x226)))
 (let ((?x65 (left s_)))
 (let (($x231 ((_ is stack ) ?x65)))
 (let (($x233 (and (= c_ left-to-right) $x231)))
 (let (($x235 (and (= c_ left-to-center) $x231)))
 (ite $x235 (= ?x208 (stack (top ?x65) ?x68)) (ite $x233 $x247 (ite $x230 $x247 $x257))))))))))))))))))))))))
(assert
 (let ((?x70 (center s_)))
 (let ((?x211 (center s__)))
 (let (($x249 (= ?x211 ?x70)))
 (let ((?x68 (right s_)))
 (let (($x221 ((_ is stack ) ?x68)))
 (let (($x222 (= c_ right-to-center)))
 (let (($x223 (and $x222 $x221)))
 (let (($x224 (= c_ right-to-left)))
 (let (($x225 (and $x224 $x221)))
 (let (($x241 (= ?x211 (rest ?x70))))
 (let (($x226 ((_ is stack ) ?x70)))
 (let (($x227 (= c_ center-to-right)))
 (let (($x228 (and $x227 $x226)))
 (let (($x256 (ite $x228 $x241 (ite $x225 $x249 (ite $x223 (= ?x211 (stack (top ?x68) ?x70)) $x249)))))
 (let (($x229 (= c_ center-to-left)))
 (let (($x230 (and $x229 $x226)))
 (let ((?x65 (left s_)))
 (let (($x231 ((_ is stack ) ?x65)))
 (let (($x233 (and (= c_ left-to-right) $x231)))
 (let (($x235 (and (= c_ left-to-center) $x231)))
 (ite $x235 $x249 (ite $x233 (= ?x211 (stack (top ?x65) ?x70)) (ite $x230 $x241 $x256))))))))))))))))))))))))
(assert
 (= c___ c!2))
(assert
 (let ((?x206 (left s__)))
 (let ((?x135 (left s___)))
 (let (($x361 (= ?x135 ?x206)))
 (let ((?x208 (right s__)))
 (let ((?x328 (top ?x208)))
 (let ((?x329 (stack ?x328 ?x206)))
 (let (($x355 (= ?x135 ?x329)))
 (let (($x331 ((_ is stack ) ?x208)))
 (let (($x334 (= c__ right-to-left)))
 (let (($x335 (and $x334 $x331)))
 (let ((?x211 (center s__)))
 (let (($x336 ((_ is stack ) ?x211)))
 (let (($x337 (= c__ center-to-right)))
 (let (($x338 (and $x337 $x336)))
 (let ((?x324 (top ?x211)))
 (let ((?x325 (stack ?x324 ?x206)))
 (let (($x352 (= ?x135 ?x325)))
 (let (($x339 (= c__ center-to-left)))
 (let (($x340 (and $x339 $x336)))
 (let ((?x137 (rest ?x206)))
 (let (($x348 (= ?x135 ?x137)))
 (let (($x341 ((_ is stack ) ?x206)))
 (let (($x342 (= c__ left-to-right)))
 (let (($x343 (and $x342 $x341)))
 (let (($x344 (= c__ left-to-center)))
 (let (($x345 (and $x344 $x341)))
 (ite $x345 $x348 (ite $x343 $x348 (ite $x340 $x352 (ite $x338 $x361 (ite $x335 $x355 $x361))))))))))))))))))))))))))))))))
(assert
 (let ((?x208 (right s__)))
 (let ((?x138 (right s___)))
 (let (($x357 (= ?x138 ?x208)))
 (let ((?x327 (rest ?x208)))
 (let (($x354 (= ?x138 ?x327)))
 (let (($x331 ((_ is stack ) ?x208)))
 (let (($x332 (= c__ right-to-center)))
 (let (($x333 (and $x332 $x331)))
 (let (($x334 (= c__ right-to-left)))
 (let (($x335 (and $x334 $x331)))
 (let ((?x211 (center s__)))
 (let ((?x324 (top ?x211)))
 (let ((?x326 (stack ?x324 ?x208)))
 (let (($x353 (= ?x138 ?x326)))
 (let (($x336 ((_ is stack ) ?x211)))
 (let (($x337 (= c__ center-to-right)))
 (let (($x338 (and $x337 $x336)))
 (let (($x339 (= c__ center-to-left)))
 (let (($x340 (and $x339 $x336)))
 (let ((?x206 (left s__)))
 (let (($x341 ((_ is stack ) ?x206)))
 (let (($x342 (= c__ left-to-right)))
 (let (($x343 (and $x342 $x341)))
 (let (($x374 (ite $x343 $x357 (ite $x340 $x357 (ite $x338 $x353 (ite $x335 $x354 (ite $x333 $x354 $x357)))))))
 (let ((?x203 (top ?x206)))
 (let ((?x320 (stack ?x203 ?x208)))
 (let (($x349 (= ?x138 ?x320)))
 (let (($x344 (= c__ left-to-center)))
 (let (($x345 (and $x344 $x341)))
 (ite $x345 $x349 $x374)))))))))))))))))))))))))))))))
(assert
 (let ((?x211 (center s__)))
 (let ((?x321 (center s___)))
 (let (($x359 (= ?x321 ?x211)))
 (let ((?x208 (right s__)))
 (let ((?x328 (top ?x208)))
 (let ((?x330 (stack ?x328 ?x211)))
 (let (($x356 (= ?x321 ?x330)))
 (let (($x331 ((_ is stack ) ?x208)))
 (let (($x332 (= c__ right-to-center)))
 (let (($x333 (and $x332 $x331)))
 (let (($x334 (= c__ right-to-left)))
 (let (($x335 (and $x334 $x331)))
 (let ((?x323 (rest ?x211)))
 (let (($x351 (= ?x321 ?x323)))
 (let (($x336 ((_ is stack ) ?x211)))
 (let (($x337 (= c__ center-to-right)))
 (let (($x338 (and $x337 $x336)))
 (let (($x339 (= c__ center-to-left)))
 (let (($x340 (and $x339 $x336)))
 (let ((?x206 (left s__)))
 (let ((?x203 (top ?x206)))
 (let ((?x322 (stack ?x203 ?x211)))
 (let (($x350 (= ?x321 ?x322)))
 (let (($x341 ((_ is stack ) ?x206)))
 (let (($x342 (= c__ left-to-right)))
 (let (($x343 (and $x342 $x341)))
 (let (($x373 (ite $x343 $x350 (ite $x340 $x351 (ite $x338 $x351 (ite $x335 $x359 (ite $x333 $x356 $x359)))))))
 (let (($x344 (= c__ left-to-center)))
 (let (($x345 (and $x344 $x341)))
 (ite $x345 $x359 $x373)))))))))))))))))))))))))))))))
(assert
 (= c____ c!3))
(assert
 (let ((?x135 (left s___)))
 (let ((?x267 (left s____)))
 (let (($x499 (= ?x267 ?x135)))
 (let ((?x138 (right s___)))
 (let ((?x466 (top ?x138)))
 (let ((?x467 (stack ?x466 ?x135)))
 (let (($x493 (= ?x267 ?x467)))
 (let (($x469 ((_ is stack ) ?x138)))
 (let (($x472 (= c___ right-to-left)))
 (let (($x473 (and $x472 $x469)))
 (let ((?x321 (center s___)))
 (let (($x474 ((_ is stack ) ?x321)))
 (let (($x475 (= c___ center-to-right)))
 (let (($x476 (and $x475 $x474)))
 (let ((?x462 (top ?x321)))
 (let ((?x463 (stack ?x462 ?x135)))
 (let (($x490 (= ?x267 ?x463)))
 (let (($x477 (= c___ center-to-left)))
 (let (($x478 (and $x477 $x474)))
 (let ((?x296 (rest ?x135)))
 (let (($x486 (= ?x267 ?x296)))
 (let (($x479 ((_ is stack ) ?x135)))
 (let (($x480 (= c___ left-to-right)))
 (let (($x481 (and $x480 $x479)))
 (let (($x482 (= c___ left-to-center)))
 (let (($x483 (and $x482 $x479)))
 (ite $x483 $x486 (ite $x481 $x486 (ite $x478 $x490 (ite $x476 $x499 (ite $x473 $x493 $x499))))))))))))))))))))))))))))))))
(assert
 (let ((?x138 (right s___)))
 (let ((?x297 (right s____)))
 (let (($x495 (= ?x297 ?x138)))
 (let ((?x465 (rest ?x138)))
 (let (($x492 (= ?x297 ?x465)))
 (let (($x469 ((_ is stack ) ?x138)))
 (let (($x470 (= c___ right-to-center)))
 (let (($x471 (and $x470 $x469)))
 (let (($x472 (= c___ right-to-left)))
 (let (($x473 (and $x472 $x469)))
 (let ((?x321 (center s___)))
 (let ((?x462 (top ?x321)))
 (let ((?x464 (stack ?x462 ?x138)))
 (let (($x491 (= ?x297 ?x464)))
 (let (($x474 ((_ is stack ) ?x321)))
 (let (($x475 (= c___ center-to-right)))
 (let (($x476 (and $x475 $x474)))
 (let (($x477 (= c___ center-to-left)))
 (let (($x478 (and $x477 $x474)))
 (let ((?x135 (left s___)))
 (let (($x479 ((_ is stack ) ?x135)))
 (let (($x480 (= c___ left-to-right)))
 (let (($x481 (and $x480 $x479)))
 (let (($x512 (ite $x481 $x495 (ite $x478 $x495 (ite $x476 $x491 (ite $x473 $x492 (ite $x471 $x492 $x495)))))))
 (let ((?x319 (top ?x135)))
 (let ((?x458 (stack ?x319 ?x138)))
 (let (($x487 (= ?x297 ?x458)))
 (let (($x482 (= c___ left-to-center)))
 (let (($x483 (and $x482 $x479)))
 (ite $x483 $x487 $x512)))))))))))))))))))))))))))))))
(assert
 (let ((?x321 (center s___)))
 (let ((?x459 (center s____)))
 (let (($x497 (= ?x459 ?x321)))
 (let ((?x138 (right s___)))
 (let ((?x466 (top ?x138)))
 (let ((?x468 (stack ?x466 ?x321)))
 (let (($x494 (= ?x459 ?x468)))
 (let (($x469 ((_ is stack ) ?x138)))
 (let (($x470 (= c___ right-to-center)))
 (let (($x471 (and $x470 $x469)))
 (let (($x472 (= c___ right-to-left)))
 (let (($x473 (and $x472 $x469)))
 (let ((?x461 (rest ?x321)))
 (let (($x489 (= ?x459 ?x461)))
 (let (($x474 ((_ is stack ) ?x321)))
 (let (($x475 (= c___ center-to-right)))
 (let (($x476 (and $x475 $x474)))
 (let (($x477 (= c___ center-to-left)))
 (let (($x478 (and $x477 $x474)))
 (let ((?x135 (left s___)))
 (let ((?x319 (top ?x135)))
 (let ((?x460 (stack ?x319 ?x321)))
 (let (($x488 (= ?x459 ?x460)))
 (let (($x479 ((_ is stack ) ?x135)))
 (let (($x480 (= c___ left-to-right)))
 (let (($x481 (and $x480 $x479)))
 (let (($x511 (ite $x481 $x488 (ite $x478 $x489 (ite $x476 $x489 (ite $x473 $x497 (ite $x471 $x494 $x497)))))))
 (let (($x482 (= c___ left-to-center)))
 (let (($x483 (and $x482 $x479)))
 (ite $x483 $x497 $x511)))))))))))))))))))))))))))))))
(assert
 (= c_____ c!4))
(assert
 (let ((?x267 (left s____)))
 (let ((?x377 (left s_____)))
 (let (($x637 (= ?x377 ?x267)))
 (let ((?x297 (right s____)))
 (let ((?x604 (top ?x297)))
 (let ((?x605 (stack ?x604 ?x267)))
 (let (($x631 (= ?x377 ?x605)))
 (let (($x607 ((_ is stack ) ?x297)))
 (let (($x610 (= c____ right-to-left)))
 (let (($x611 (and $x610 $x607)))
 (let ((?x459 (center s____)))
 (let (($x612 ((_ is stack ) ?x459)))
 (let (($x613 (= c____ center-to-right)))
 (let (($x614 (and $x613 $x612)))
 (let ((?x600 (top ?x459)))
 (let ((?x601 (stack ?x600 ?x267)))
 (let (($x628 (= ?x377 ?x601)))
 (let (($x615 (= c____ center-to-left)))
 (let (($x616 (and $x615 $x612)))
 (let ((?x410 (rest ?x267)))
 (let (($x624 (= ?x377 ?x410)))
 (let (($x617 ((_ is stack ) ?x267)))
 (let (($x618 (= c____ left-to-right)))
 (let (($x619 (and $x618 $x617)))
 (let (($x620 (= c____ left-to-center)))
 (let (($x621 (and $x620 $x617)))
 (ite $x621 $x624 (ite $x619 $x624 (ite $x616 $x628 (ite $x614 $x637 (ite $x611 $x631 $x637))))))))))))))))))))))))))))))))
(assert
 (let ((?x297 (right s____)))
 (let ((?x411 (right s_____)))
 (let (($x633 (= ?x411 ?x297)))
 (let ((?x603 (rest ?x297)))
 (let (($x630 (= ?x411 ?x603)))
 (let (($x607 ((_ is stack ) ?x297)))
 (let (($x608 (= c____ right-to-center)))
 (let (($x609 (and $x608 $x607)))
 (let (($x610 (= c____ right-to-left)))
 (let (($x611 (and $x610 $x607)))
 (let ((?x459 (center s____)))
 (let ((?x600 (top ?x459)))
 (let ((?x602 (stack ?x600 ?x297)))
 (let (($x629 (= ?x411 ?x602)))
 (let (($x612 ((_ is stack ) ?x459)))
 (let (($x613 (= c____ center-to-right)))
 (let (($x614 (and $x613 $x612)))
 (let (($x615 (= c____ center-to-left)))
 (let (($x616 (and $x615 $x612)))
 (let ((?x267 (left s____)))
 (let (($x617 ((_ is stack ) ?x267)))
 (let (($x618 (= c____ left-to-right)))
 (let (($x619 (and $x618 $x617)))
 (let (($x650 (ite $x619 $x633 (ite $x616 $x633 (ite $x614 $x629 (ite $x611 $x630 (ite $x609 $x630 $x633)))))))
 (let ((?x457 (top ?x267)))
 (let ((?x596 (stack ?x457 ?x297)))
 (let (($x625 (= ?x411 ?x596)))
 (let (($x620 (= c____ left-to-center)))
 (let (($x621 (and $x620 $x617)))
 (ite $x621 $x625 $x650)))))))))))))))))))))))))))))))
(assert
 (let ((?x459 (center s____)))
 (let ((?x597 (center s_____)))
 (let (($x635 (= ?x597 ?x459)))
 (let ((?x297 (right s____)))
 (let ((?x604 (top ?x297)))
 (let ((?x606 (stack ?x604 ?x459)))
 (let (($x632 (= ?x597 ?x606)))
 (let (($x607 ((_ is stack ) ?x297)))
 (let (($x608 (= c____ right-to-center)))
 (let (($x609 (and $x608 $x607)))
 (let (($x610 (= c____ right-to-left)))
 (let (($x611 (and $x610 $x607)))
 (let ((?x599 (rest ?x459)))
 (let (($x627 (= ?x597 ?x599)))
 (let (($x612 ((_ is stack ) ?x459)))
 (let (($x613 (= c____ center-to-right)))
 (let (($x614 (and $x613 $x612)))
 (let (($x615 (= c____ center-to-left)))
 (let (($x616 (and $x615 $x612)))
 (let ((?x267 (left s____)))
 (let ((?x457 (top ?x267)))
 (let ((?x598 (stack ?x457 ?x459)))
 (let (($x626 (= ?x597 ?x598)))
 (let (($x617 ((_ is stack ) ?x267)))
 (let (($x618 (= c____ left-to-right)))
 (let (($x619 (and $x618 $x617)))
 (let (($x649 (ite $x619 $x626 (ite $x616 $x627 (ite $x614 $x627 (ite $x611 $x635 (ite $x609 $x632 $x635)))))))
 (let (($x620 (= c____ left-to-center)))
 (let (($x621 (and $x620 $x617)))
 (ite $x621 $x635 $x649)))))))))))))))))))))))))))))))
(assert
 (= c______ c!5))
(assert
 (let ((?x377 (left s_____)))
 (let ((?x515 (left s______)))
 (let (($x775 (= ?x515 ?x377)))
 (let ((?x411 (right s_____)))
 (let ((?x742 (top ?x411)))
 (let ((?x743 (stack ?x742 ?x377)))
 (let (($x769 (= ?x515 ?x743)))
 (let (($x745 ((_ is stack ) ?x411)))
 (let (($x748 (= c_____ right-to-left)))
 (let (($x749 (and $x748 $x745)))
 (let ((?x597 (center s_____)))
 (let (($x750 ((_ is stack ) ?x597)))
 (let (($x751 (= c_____ center-to-right)))
 (let (($x752 (and $x751 $x750)))
 (let ((?x738 (top ?x597)))
 (let ((?x739 (stack ?x738 ?x377)))
 (let (($x766 (= ?x515 ?x739)))
 (let (($x753 (= c_____ center-to-left)))
 (let (($x754 (and $x753 $x750)))
 (let ((?x548 (rest ?x377)))
 (let (($x762 (= ?x515 ?x548)))
 (let (($x755 ((_ is stack ) ?x377)))
 (let (($x756 (= c_____ left-to-right)))
 (let (($x757 (and $x756 $x755)))
 (let (($x758 (= c_____ left-to-center)))
 (let (($x759 (and $x758 $x755)))
 (ite $x759 $x762 (ite $x757 $x762 (ite $x754 $x766 (ite $x752 $x775 (ite $x749 $x769 $x775))))))))))))))))))))))))))))))))
(assert
 (let ((?x411 (right s_____)))
 (let ((?x549 (right s______)))
 (let (($x771 (= ?x549 ?x411)))
 (let ((?x741 (rest ?x411)))
 (let (($x768 (= ?x549 ?x741)))
 (let (($x745 ((_ is stack ) ?x411)))
 (let (($x746 (= c_____ right-to-center)))
 (let (($x747 (and $x746 $x745)))
 (let (($x748 (= c_____ right-to-left)))
 (let (($x749 (and $x748 $x745)))
 (let ((?x597 (center s_____)))
 (let ((?x738 (top ?x597)))
 (let ((?x740 (stack ?x738 ?x411)))
 (let (($x767 (= ?x549 ?x740)))
 (let (($x750 ((_ is stack ) ?x597)))
 (let (($x751 (= c_____ center-to-right)))
 (let (($x752 (and $x751 $x750)))
 (let (($x753 (= c_____ center-to-left)))
 (let (($x754 (and $x753 $x750)))
 (let ((?x377 (left s_____)))
 (let (($x755 ((_ is stack ) ?x377)))
 (let (($x756 (= c_____ left-to-right)))
 (let (($x757 (and $x756 $x755)))
 (let (($x788 (ite $x757 $x771 (ite $x754 $x771 (ite $x752 $x767 (ite $x749 $x768 (ite $x747 $x768 $x771)))))))
 (let ((?x595 (top ?x377)))
 (let ((?x734 (stack ?x595 ?x411)))
 (let (($x763 (= ?x549 ?x734)))
 (let (($x758 (= c_____ left-to-center)))
 (let (($x759 (and $x758 $x755)))
 (ite $x759 $x763 $x788)))))))))))))))))))))))))))))))
(assert
 (let ((?x597 (center s_____)))
 (let ((?x735 (center s______)))
 (let (($x773 (= ?x735 ?x597)))
 (let ((?x411 (right s_____)))
 (let ((?x742 (top ?x411)))
 (let ((?x744 (stack ?x742 ?x597)))
 (let (($x770 (= ?x735 ?x744)))
 (let (($x745 ((_ is stack ) ?x411)))
 (let (($x746 (= c_____ right-to-center)))
 (let (($x747 (and $x746 $x745)))
 (let (($x748 (= c_____ right-to-left)))
 (let (($x749 (and $x748 $x745)))
 (let ((?x737 (rest ?x597)))
 (let (($x765 (= ?x735 ?x737)))
 (let (($x750 ((_ is stack ) ?x597)))
 (let (($x751 (= c_____ center-to-right)))
 (let (($x752 (and $x751 $x750)))
 (let (($x753 (= c_____ center-to-left)))
 (let (($x754 (and $x753 $x750)))
 (let ((?x377 (left s_____)))
 (let ((?x595 (top ?x377)))
 (let ((?x736 (stack ?x595 ?x597)))
 (let (($x764 (= ?x735 ?x736)))
 (let (($x755 ((_ is stack ) ?x377)))
 (let (($x756 (= c_____ left-to-right)))
 (let (($x757 (and $x756 $x755)))
 (let (($x787 (ite $x757 $x764 (ite $x754 $x765 (ite $x752 $x765 (ite $x749 $x773 (ite $x747 $x770 $x773)))))))
 (let (($x758 (= c_____ left-to-center)))
 (let (($x759 (and $x758 $x755)))
 (ite $x759 $x773 $x787)))))))))))))))))))))))))))))))
(assert
 (= c_______ c!6))
(assert
 (let ((?x515 (left s______)))
 (let ((?x653 (left s_______)))
 (let (($x913 (= ?x653 ?x515)))
 (let ((?x549 (right s______)))
 (let ((?x880 (top ?x549)))
 (let ((?x881 (stack ?x880 ?x515)))
 (let (($x907 (= ?x653 ?x881)))
 (let (($x883 ((_ is stack ) ?x549)))
 (let (($x886 (= c______ right-to-left)))
 (let (($x887 (and $x886 $x883)))
 (let ((?x735 (center s______)))
 (let (($x888 ((_ is stack ) ?x735)))
 (let (($x889 (= c______ center-to-right)))
 (let (($x890 (and $x889 $x888)))
 (let ((?x876 (top ?x735)))
 (let ((?x877 (stack ?x876 ?x515)))
 (let (($x904 (= ?x653 ?x877)))
 (let (($x891 (= c______ center-to-left)))
 (let (($x892 (and $x891 $x888)))
 (let ((?x686 (rest ?x515)))
 (let (($x900 (= ?x653 ?x686)))
 (let (($x893 ((_ is stack ) ?x515)))
 (let (($x894 (= c______ left-to-right)))
 (let (($x895 (and $x894 $x893)))
 (let (($x896 (= c______ left-to-center)))
 (let (($x897 (and $x896 $x893)))
 (ite $x897 $x900 (ite $x895 $x900 (ite $x892 $x904 (ite $x890 $x913 (ite $x887 $x907 $x913))))))))))))))))))))))))))))))))
(assert
 (let ((?x549 (right s______)))
 (let ((?x687 (right s_______)))
 (let (($x909 (= ?x687 ?x549)))
 (let ((?x879 (rest ?x549)))
 (let (($x906 (= ?x687 ?x879)))
 (let (($x883 ((_ is stack ) ?x549)))
 (let (($x884 (= c______ right-to-center)))
 (let (($x885 (and $x884 $x883)))
 (let (($x886 (= c______ right-to-left)))
 (let (($x887 (and $x886 $x883)))
 (let ((?x735 (center s______)))
 (let ((?x876 (top ?x735)))
 (let ((?x878 (stack ?x876 ?x549)))
 (let (($x905 (= ?x687 ?x878)))
 (let (($x888 ((_ is stack ) ?x735)))
 (let (($x889 (= c______ center-to-right)))
 (let (($x890 (and $x889 $x888)))
 (let (($x891 (= c______ center-to-left)))
 (let (($x892 (and $x891 $x888)))
 (let ((?x515 (left s______)))
 (let (($x893 ((_ is stack ) ?x515)))
 (let (($x894 (= c______ left-to-right)))
 (let (($x895 (and $x894 $x893)))
 (let (($x926 (ite $x895 $x909 (ite $x892 $x909 (ite $x890 $x905 (ite $x887 $x906 (ite $x885 $x906 $x909)))))))
 (let ((?x733 (top ?x515)))
 (let ((?x872 (stack ?x733 ?x549)))
 (let (($x901 (= ?x687 ?x872)))
 (let (($x896 (= c______ left-to-center)))
 (let (($x897 (and $x896 $x893)))
 (ite $x897 $x901 $x926)))))))))))))))))))))))))))))))
(assert
 (let ((?x735 (center s______)))
 (let ((?x873 (center s_______)))
 (let (($x911 (= ?x873 ?x735)))
 (let ((?x549 (right s______)))
 (let ((?x880 (top ?x549)))
 (let ((?x882 (stack ?x880 ?x735)))
 (let (($x908 (= ?x873 ?x882)))
 (let (($x883 ((_ is stack ) ?x549)))
 (let (($x884 (= c______ right-to-center)))
 (let (($x885 (and $x884 $x883)))
 (let (($x886 (= c______ right-to-left)))
 (let (($x887 (and $x886 $x883)))
 (let ((?x875 (rest ?x735)))
 (let (($x903 (= ?x873 ?x875)))
 (let (($x888 ((_ is stack ) ?x735)))
 (let (($x889 (= c______ center-to-right)))
 (let (($x890 (and $x889 $x888)))
 (let (($x891 (= c______ center-to-left)))
 (let (($x892 (and $x891 $x888)))
 (let ((?x515 (left s______)))
 (let ((?x733 (top ?x515)))
 (let ((?x874 (stack ?x733 ?x735)))
 (let (($x902 (= ?x873 ?x874)))
 (let (($x893 ((_ is stack ) ?x515)))
 (let (($x894 (= c______ left-to-right)))
 (let (($x895 (and $x894 $x893)))
 (let (($x925 (ite $x895 $x902 (ite $x892 $x903 (ite $x890 $x903 (ite $x887 $x911 (ite $x885 $x908 $x911)))))))
 (let (($x896 (= c______ left-to-center)))
 (let (($x897 (and $x896 $x893)))
 (ite $x897 $x911 $x925)))))))))))))))))))))))))))))))
(assert
 (= c________ c!7))
(assert
 (let ((?x653 (left s_______)))
 (let ((?x791 (left s________)))
 (let (($x1051 (= ?x791 ?x653)))
 (let ((?x687 (right s_______)))
 (let ((?x1018 (top ?x687)))
 (let ((?x1019 (stack ?x1018 ?x653)))
 (let (($x1045 (= ?x791 ?x1019)))
 (let (($x1021 ((_ is stack ) ?x687)))
 (let (($x1024 (= c_______ right-to-left)))
 (let (($x1025 (and $x1024 $x1021)))
 (let ((?x873 (center s_______)))
 (let (($x1026 ((_ is stack ) ?x873)))
 (let (($x1027 (= c_______ center-to-right)))
 (let (($x1028 (and $x1027 $x1026)))
 (let ((?x1014 (top ?x873)))
 (let ((?x1015 (stack ?x1014 ?x653)))
 (let (($x1042 (= ?x791 ?x1015)))
 (let (($x1029 (= c_______ center-to-left)))
 (let (($x1030 (and $x1029 $x1026)))
 (let ((?x824 (rest ?x653)))
 (let (($x1038 (= ?x791 ?x824)))
 (let (($x1031 ((_ is stack ) ?x653)))
 (let (($x1032 (= c_______ left-to-right)))
 (let (($x1033 (and $x1032 $x1031)))
 (let (($x1034 (= c_______ left-to-center)))
 (let (($x1035 (and $x1034 $x1031)))
 (ite $x1035 $x1038 (ite $x1033 $x1038 (ite $x1030 $x1042 (ite $x1028 $x1051 (ite $x1025 $x1045 $x1051))))))))))))))))))))))))))))))))
(assert
 (let ((?x687 (right s_______)))
 (let ((?x825 (right s________)))
 (let (($x1047 (= ?x825 ?x687)))
 (let ((?x1017 (rest ?x687)))
 (let (($x1044 (= ?x825 ?x1017)))
 (let (($x1021 ((_ is stack ) ?x687)))
 (let (($x1022 (= c_______ right-to-center)))
 (let (($x1023 (and $x1022 $x1021)))
 (let (($x1024 (= c_______ right-to-left)))
 (let (($x1025 (and $x1024 $x1021)))
 (let ((?x873 (center s_______)))
 (let ((?x1014 (top ?x873)))
 (let ((?x1016 (stack ?x1014 ?x687)))
 (let (($x1043 (= ?x825 ?x1016)))
 (let (($x1026 ((_ is stack ) ?x873)))
 (let (($x1027 (= c_______ center-to-right)))
 (let (($x1028 (and $x1027 $x1026)))
 (let (($x1029 (= c_______ center-to-left)))
 (let (($x1030 (and $x1029 $x1026)))
 (let ((?x653 (left s_______)))
 (let (($x1031 ((_ is stack ) ?x653)))
 (let (($x1032 (= c_______ left-to-right)))
 (let (($x1033 (and $x1032 $x1031)))
 (let (($x1064 (ite $x1033 $x1047 (ite $x1030 $x1047 (ite $x1028 $x1043 (ite $x1025 $x1044 (ite $x1023 $x1044 $x1047)))))))
 (let ((?x871 (top ?x653)))
 (let ((?x1010 (stack ?x871 ?x687)))
 (let (($x1039 (= ?x825 ?x1010)))
 (let (($x1034 (= c_______ left-to-center)))
 (let (($x1035 (and $x1034 $x1031)))
 (ite $x1035 $x1039 $x1064)))))))))))))))))))))))))))))))
(assert
 (let ((?x873 (center s_______)))
 (let ((?x1011 (center s________)))
 (let (($x1049 (= ?x1011 ?x873)))
 (let ((?x687 (right s_______)))
 (let ((?x1018 (top ?x687)))
 (let ((?x1020 (stack ?x1018 ?x873)))
 (let (($x1046 (= ?x1011 ?x1020)))
 (let (($x1021 ((_ is stack ) ?x687)))
 (let (($x1022 (= c_______ right-to-center)))
 (let (($x1023 (and $x1022 $x1021)))
 (let (($x1024 (= c_______ right-to-left)))
 (let (($x1025 (and $x1024 $x1021)))
 (let ((?x1013 (rest ?x873)))
 (let (($x1041 (= ?x1011 ?x1013)))
 (let (($x1026 ((_ is stack ) ?x873)))
 (let (($x1027 (= c_______ center-to-right)))
 (let (($x1028 (and $x1027 $x1026)))
 (let (($x1029 (= c_______ center-to-left)))
 (let (($x1030 (and $x1029 $x1026)))
 (let ((?x653 (left s_______)))
 (let ((?x871 (top ?x653)))
 (let ((?x1012 (stack ?x871 ?x873)))
 (let (($x1040 (= ?x1011 ?x1012)))
 (let (($x1031 ((_ is stack ) ?x653)))
 (let (($x1032 (= c_______ left-to-right)))
 (let (($x1033 (and $x1032 $x1031)))
 (let (($x1063 (ite $x1033 $x1040 (ite $x1030 $x1041 (ite $x1028 $x1041 (ite $x1025 $x1049 (ite $x1023 $x1046 $x1049)))))))
 (let (($x1034 (= c_______ left-to-center)))
 (let (($x1035 (and $x1034 $x1031)))
 (ite $x1035 $x1049 $x1063)))))))))))))))))))))))))))))))
(assert
 (= c_________ c!8))
(assert
 (let ((?x791 (left s________)))
 (let ((?x929 (left s_________)))
 (let (($x1189 (= ?x929 ?x791)))
 (let ((?x825 (right s________)))
 (let ((?x1156 (top ?x825)))
 (let ((?x1157 (stack ?x1156 ?x791)))
 (let (($x1183 (= ?x929 ?x1157)))
 (let (($x1159 ((_ is stack ) ?x825)))
 (let (($x1162 (= c________ right-to-left)))
 (let (($x1163 (and $x1162 $x1159)))
 (let ((?x1011 (center s________)))
 (let (($x1164 ((_ is stack ) ?x1011)))
 (let (($x1165 (= c________ center-to-right)))
 (let (($x1166 (and $x1165 $x1164)))
 (let ((?x1152 (top ?x1011)))
 (let ((?x1153 (stack ?x1152 ?x791)))
 (let (($x1180 (= ?x929 ?x1153)))
 (let (($x1167 (= c________ center-to-left)))
 (let (($x1168 (and $x1167 $x1164)))
 (let ((?x962 (rest ?x791)))
 (let (($x1176 (= ?x929 ?x962)))
 (let (($x1169 ((_ is stack ) ?x791)))
 (let (($x1170 (= c________ left-to-right)))
 (let (($x1171 (and $x1170 $x1169)))
 (let (($x1172 (= c________ left-to-center)))
 (let (($x1173 (and $x1172 $x1169)))
 (ite $x1173 $x1176 (ite $x1171 $x1176 (ite $x1168 $x1180 (ite $x1166 $x1189 (ite $x1163 $x1183 $x1189))))))))))))))))))))))))))))))))
(assert
 (let ((?x825 (right s________)))
 (let ((?x963 (right s_________)))
 (let (($x1185 (= ?x963 ?x825)))
 (let ((?x1155 (rest ?x825)))
 (let (($x1182 (= ?x963 ?x1155)))
 (let (($x1159 ((_ is stack ) ?x825)))
 (let (($x1160 (= c________ right-to-center)))
 (let (($x1161 (and $x1160 $x1159)))
 (let (($x1162 (= c________ right-to-left)))
 (let (($x1163 (and $x1162 $x1159)))
 (let ((?x1011 (center s________)))
 (let ((?x1152 (top ?x1011)))
 (let ((?x1154 (stack ?x1152 ?x825)))
 (let (($x1181 (= ?x963 ?x1154)))
 (let (($x1164 ((_ is stack ) ?x1011)))
 (let (($x1165 (= c________ center-to-right)))
 (let (($x1166 (and $x1165 $x1164)))
 (let (($x1167 (= c________ center-to-left)))
 (let (($x1168 (and $x1167 $x1164)))
 (let ((?x791 (left s________)))
 (let (($x1169 ((_ is stack ) ?x791)))
 (let (($x1170 (= c________ left-to-right)))
 (let (($x1171 (and $x1170 $x1169)))
 (let (($x1202 (ite $x1171 $x1185 (ite $x1168 $x1185 (ite $x1166 $x1181 (ite $x1163 $x1182 (ite $x1161 $x1182 $x1185)))))))
 (let ((?x1009 (top ?x791)))
 (let ((?x1148 (stack ?x1009 ?x825)))
 (let (($x1177 (= ?x963 ?x1148)))
 (let (($x1172 (= c________ left-to-center)))
 (let (($x1173 (and $x1172 $x1169)))
 (ite $x1173 $x1177 $x1202)))))))))))))))))))))))))))))))
(assert
 (let ((?x1011 (center s________)))
 (let ((?x1149 (center s_________)))
 (let (($x1187 (= ?x1149 ?x1011)))
 (let ((?x825 (right s________)))
 (let ((?x1156 (top ?x825)))
 (let ((?x1158 (stack ?x1156 ?x1011)))
 (let (($x1184 (= ?x1149 ?x1158)))
 (let (($x1159 ((_ is stack ) ?x825)))
 (let (($x1160 (= c________ right-to-center)))
 (let (($x1161 (and $x1160 $x1159)))
 (let (($x1162 (= c________ right-to-left)))
 (let (($x1163 (and $x1162 $x1159)))
 (let ((?x1151 (rest ?x1011)))
 (let (($x1179 (= ?x1149 ?x1151)))
 (let (($x1164 ((_ is stack ) ?x1011)))
 (let (($x1165 (= c________ center-to-right)))
 (let (($x1166 (and $x1165 $x1164)))
 (let (($x1167 (= c________ center-to-left)))
 (let (($x1168 (and $x1167 $x1164)))
 (let ((?x791 (left s________)))
 (let ((?x1009 (top ?x791)))
 (let ((?x1150 (stack ?x1009 ?x1011)))
 (let (($x1178 (= ?x1149 ?x1150)))
 (let (($x1169 ((_ is stack ) ?x791)))
 (let (($x1170 (= c________ left-to-right)))
 (let (($x1171 (and $x1170 $x1169)))
 (let (($x1201 (ite $x1171 $x1178 (ite $x1168 $x1179 (ite $x1166 $x1179 (ite $x1163 $x1187 (ite $x1161 $x1184 $x1187)))))))
 (let (($x1172 (= c________ left-to-center)))
 (let (($x1173 (and $x1172 $x1169)))
 (ite $x1173 $x1187 $x1201)))))))))))))))))))))))))))))))
(assert
 (let ((?x41 (stack J empty)))
 (let ((?x963 (right s_________)))
 (let (($x1205 (= ?x963 ?x41)))
 (let ((?x40 (stack I empty)))
 (let ((?x1149 (center s_________)))
 (let (($x1236 (= ?x1149 ?x40)))
 (let ((?x37 (stack F (stack B (stack D (stack G (stack H (stack A empty))))))))
 (let ((?x39 (stack E (stack C ?x37))))
 (let ((?x929 (left s_________)))
 (let (($x1237 (= ?x929 ?x39)))
 (let (($x1239 (not (and $x1237 $x1236 $x1205))))
 (not $x1239)))))))))))))
(check-sat)
