; 
(set-info :status unknown)
(set-logic QF_DT)
(declare-datatypes ((Enum_A_B_C 0)) (((A) (B) (C))))
 (declare-datatypes ((Tower 0)) (((stack (top Enum_A_B_C) (rest Tower)) (empty))))
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
(assert
 (= s_tmp s))
(assert
 (= c_tmp c))
(assert
 (let ((?x15 (stack A empty)))
 (let ((?x65 (left s_tmp_)))
 (= ?x65 ?x15))))
(assert
 (let ((?x68 (center s_tmp__)))
 (= ?x68 empty)))
(assert
 (let ((?x16 (stack B empty)))
 (let ((?x17 (stack C ?x16)))
 (let ((?x71 (right s_tmp___)))
 (= ?x71 ?x17)))))
(assert
 (= c_tmp____ c!0))
(assert
 (= s_ s_tmp___))
(assert
 (= c_ c_tmp____))
(assert
 (let ((?x98 (left s_tmp__)))
 (= (left s_tmp___) ?x98)))
(assert
 (let ((?x68 (center s_tmp__)))
 (= (center s_tmp___) ?x68)))
(assert
 (let ((?x102 (center s_tmp_)))
 (= ?x102 (center s_tmp))))
(assert
 (let ((?x105 (right s_tmp_)))
 (= ?x105 (right s_tmp))))
(assert
 (let ((?x65 (left s_tmp_)))
 (let ((?x98 (left s_tmp__)))
 (= ?x98 ?x65))))
(assert
 (let ((?x105 (right s_tmp_)))
 (let ((?x109 (right s_tmp__)))
 (= ?x109 ?x105))))
(assert
 (= c__ c!1))
(assert
 (let ((?x42 (left s_)))
 (let ((?x161 (left s__)))
 (let (($x206 (= ?x161 ?x42)))
 (let ((?x45 (right s_)))
 (let (($x176 ((_ is stack ) ?x45)))
 (let (($x179 (= c_ right-to-left)))
 (let (($x180 (and $x179 $x176)))
 (let ((?x47 (center s_)))
 (let (($x181 ((_ is stack ) ?x47)))
 (let (($x183 (and (= c_ center-to-right) $x181)))
 (let (($x185 (and (= c_ center-to-left) $x181)))
 (let (($x215 (ite $x185 (= ?x161 (stack (top ?x47) ?x42)) (ite $x183 $x206 (ite $x180 (= ?x161 (stack (top ?x45) ?x42)) $x206)))))
 (let (($x193 (= ?x161 (rest ?x42))))
 (let (($x186 ((_ is stack ) ?x42)))
 (let (($x187 (= c_ left-to-right)))
 (let (($x188 (and $x187 $x186)))
 (let (($x189 (= c_ left-to-center)))
 (let (($x190 (and $x189 $x186)))
 (ite $x190 $x193 (ite $x188 $x193 $x215)))))))))))))))))))))
(assert
 (let ((?x45 (right s_)))
 (let ((?x166 (right s__)))
 (let (($x202 (= ?x166 ?x45)))
 (let (($x199 (= ?x166 (rest ?x45))))
 (let (($x176 ((_ is stack ) ?x45)))
 (let (($x177 (= c_ right-to-center)))
 (let (($x178 (and $x177 $x176)))
 (let (($x179 (= c_ right-to-left)))
 (let (($x180 (and $x179 $x176)))
 (let ((?x47 (center s_)))
 (let (($x181 ((_ is stack ) ?x47)))
 (let (($x183 (and (= c_ center-to-right) $x181)))
 (let (($x212 (ite $x183 (= ?x166 (stack (top ?x47) ?x45)) (ite $x180 $x199 (ite $x178 $x199 $x202)))))
 (let (($x185 (and (= c_ center-to-left) $x181)))
 (let ((?x42 (left s_)))
 (let (($x186 ((_ is stack ) ?x42)))
 (let (($x187 (= c_ left-to-right)))
 (let (($x188 (and $x187 $x186)))
 (let (($x189 (= c_ left-to-center)))
 (let (($x190 (and $x189 $x186)))
 (ite $x190 (= ?x166 (stack (top ?x42) ?x45)) (ite $x188 $x202 (ite $x185 $x202 $x212))))))))))))))))))))))))
(assert
 (let ((?x47 (center s_)))
 (let ((?x164 (center s__)))
 (let (($x204 (= ?x164 ?x47)))
 (let ((?x45 (right s_)))
 (let (($x176 ((_ is stack ) ?x45)))
 (let (($x177 (= c_ right-to-center)))
 (let (($x178 (and $x177 $x176)))
 (let (($x179 (= c_ right-to-left)))
 (let (($x180 (and $x179 $x176)))
 (let (($x196 (= ?x164 (rest ?x47))))
 (let (($x181 ((_ is stack ) ?x47)))
 (let (($x183 (and (= c_ center-to-right) $x181)))
 (let (($x211 (ite $x183 $x196 (ite $x180 $x204 (ite $x178 (= ?x164 (stack (top ?x45) ?x47)) $x204)))))
 (let (($x185 (and (= c_ center-to-left) $x181)))
 (let ((?x42 (left s_)))
 (let (($x186 ((_ is stack ) ?x42)))
 (let (($x187 (= c_ left-to-right)))
 (let (($x188 (and $x187 $x186)))
 (let (($x189 (= c_ left-to-center)))
 (let (($x190 (and $x189 $x186)))
 (ite $x190 $x204 (ite $x188 (= ?x164 (stack (top ?x42) ?x47)) (ite $x185 $x196 $x211))))))))))))))))))))))))
(assert
 (= c___ c!2))
(assert
 (let ((?x161 (left s__)))
 (let ((?x114 (left s___)))
 (let (($x313 (= ?x114 ?x161)))
 (let ((?x166 (right s__)))
 (let ((?x280 (top ?x166)))
 (let ((?x281 (stack ?x280 ?x161)))
 (let (($x307 (= ?x114 ?x281)))
 (let (($x283 ((_ is stack ) ?x166)))
 (let (($x286 (= c__ right-to-left)))
 (let (($x287 (and $x286 $x283)))
 (let ((?x164 (center s__)))
 (let (($x288 ((_ is stack ) ?x164)))
 (let (($x289 (= c__ center-to-right)))
 (let (($x290 (and $x289 $x288)))
 (let ((?x276 (top ?x164)))
 (let ((?x277 (stack ?x276 ?x161)))
 (let (($x304 (= ?x114 ?x277)))
 (let (($x291 (= c__ center-to-left)))
 (let (($x292 (and $x291 $x288)))
 (let ((?x115 (rest ?x161)))
 (let (($x300 (= ?x114 ?x115)))
 (let (($x293 ((_ is stack ) ?x161)))
 (let (($x294 (= c__ left-to-right)))
 (let (($x295 (and $x294 $x293)))
 (let (($x296 (= c__ left-to-center)))
 (let (($x297 (and $x296 $x293)))
 (ite $x297 $x300 (ite $x295 $x300 (ite $x292 $x304 (ite $x290 $x313 (ite $x287 $x307 $x313))))))))))))))))))))))))))))))))
(assert
 (let ((?x166 (right s__)))
 (let ((?x158 (right s___)))
 (let (($x309 (= ?x158 ?x166)))
 (let ((?x279 (rest ?x166)))
 (let (($x306 (= ?x158 ?x279)))
 (let (($x283 ((_ is stack ) ?x166)))
 (let (($x284 (= c__ right-to-center)))
 (let (($x285 (and $x284 $x283)))
 (let (($x286 (= c__ right-to-left)))
 (let (($x287 (and $x286 $x283)))
 (let ((?x164 (center s__)))
 (let ((?x276 (top ?x164)))
 (let ((?x278 (stack ?x276 ?x166)))
 (let (($x305 (= ?x158 ?x278)))
 (let (($x288 ((_ is stack ) ?x164)))
 (let (($x289 (= c__ center-to-right)))
 (let (($x290 (and $x289 $x288)))
 (let (($x291 (= c__ center-to-left)))
 (let (($x292 (and $x291 $x288)))
 (let ((?x161 (left s__)))
 (let (($x293 ((_ is stack ) ?x161)))
 (let (($x294 (= c__ left-to-right)))
 (let (($x295 (and $x294 $x293)))
 (let (($x326 (ite $x295 $x309 (ite $x292 $x309 (ite $x290 $x305 (ite $x287 $x306 (ite $x285 $x306 $x309)))))))
 (let ((?x271 (top ?x161)))
 (let ((?x272 (stack ?x271 ?x166)))
 (let (($x301 (= ?x158 ?x272)))
 (let (($x296 (= c__ left-to-center)))
 (let (($x297 (and $x296 $x293)))
 (ite $x297 $x301 $x326)))))))))))))))))))))))))))))))
(assert
 (let ((?x164 (center s__)))
 (let ((?x273 (center s___)))
 (let (($x311 (= ?x273 ?x164)))
 (let ((?x166 (right s__)))
 (let ((?x280 (top ?x166)))
 (let ((?x282 (stack ?x280 ?x164)))
 (let (($x308 (= ?x273 ?x282)))
 (let (($x283 ((_ is stack ) ?x166)))
 (let (($x284 (= c__ right-to-center)))
 (let (($x285 (and $x284 $x283)))
 (let (($x286 (= c__ right-to-left)))
 (let (($x287 (and $x286 $x283)))
 (let ((?x275 (rest ?x164)))
 (let (($x303 (= ?x273 ?x275)))
 (let (($x288 ((_ is stack ) ?x164)))
 (let (($x289 (= c__ center-to-right)))
 (let (($x290 (and $x289 $x288)))
 (let (($x291 (= c__ center-to-left)))
 (let (($x292 (and $x291 $x288)))
 (let ((?x161 (left s__)))
 (let ((?x271 (top ?x161)))
 (let ((?x274 (stack ?x271 ?x164)))
 (let (($x302 (= ?x273 ?x274)))
 (let (($x293 ((_ is stack ) ?x161)))
 (let (($x294 (= c__ left-to-right)))
 (let (($x295 (and $x294 $x293)))
 (let (($x325 (ite $x295 $x302 (ite $x292 $x303 (ite $x290 $x303 (ite $x287 $x311 (ite $x285 $x308 $x311)))))))
 (let (($x296 (= c__ left-to-center)))
 (let (($x297 (and $x296 $x293)))
 (ite $x297 $x311 $x325)))))))))))))))))))))))))))))))
(assert
 (= c____ c!3))
(assert
 (let ((?x114 (left s___)))
 (let ((?x251 (left s____)))
 (let (($x452 (= ?x251 ?x114)))
 (let ((?x158 (right s___)))
 (let ((?x419 (top ?x158)))
 (let ((?x420 (stack ?x419 ?x114)))
 (let (($x446 (= ?x251 ?x420)))
 (let (($x422 ((_ is stack ) ?x158)))
 (let (($x425 (= c___ right-to-left)))
 (let (($x426 (and $x425 $x422)))
 (let ((?x273 (center s___)))
 (let (($x427 ((_ is stack ) ?x273)))
 (let (($x428 (= c___ center-to-right)))
 (let (($x429 (and $x428 $x427)))
 (let ((?x415 (top ?x273)))
 (let ((?x416 (stack ?x415 ?x114)))
 (let (($x443 (= ?x251 ?x416)))
 (let (($x430 (= c___ center-to-left)))
 (let (($x431 (and $x430 $x427)))
 (let ((?x252 (rest ?x114)))
 (let (($x439 (= ?x251 ?x252)))
 (let (($x432 ((_ is stack ) ?x114)))
 (let (($x433 (= c___ left-to-right)))
 (let (($x434 (and $x433 $x432)))
 (let (($x435 (= c___ left-to-center)))
 (let (($x436 (and $x435 $x432)))
 (ite $x436 $x439 (ite $x434 $x439 (ite $x431 $x443 (ite $x429 $x452 (ite $x426 $x446 $x452))))))))))))))))))))))))))))))))
(assert
 (let ((?x158 (right s___)))
 (let ((?x270 (right s____)))
 (let (($x448 (= ?x270 ?x158)))
 (let ((?x418 (rest ?x158)))
 (let (($x445 (= ?x270 ?x418)))
 (let (($x422 ((_ is stack ) ?x158)))
 (let (($x423 (= c___ right-to-center)))
 (let (($x424 (and $x423 $x422)))
 (let (($x425 (= c___ right-to-left)))
 (let (($x426 (and $x425 $x422)))
 (let ((?x273 (center s___)))
 (let ((?x415 (top ?x273)))
 (let ((?x417 (stack ?x415 ?x158)))
 (let (($x444 (= ?x270 ?x417)))
 (let (($x427 ((_ is stack ) ?x273)))
 (let (($x428 (= c___ center-to-right)))
 (let (($x429 (and $x428 $x427)))
 (let (($x430 (= c___ center-to-left)))
 (let (($x431 (and $x430 $x427)))
 (let ((?x114 (left s___)))
 (let (($x432 ((_ is stack ) ?x114)))
 (let (($x433 (= c___ left-to-right)))
 (let (($x434 (and $x433 $x432)))
 (let (($x465 (ite $x434 $x448 (ite $x431 $x448 (ite $x429 $x444 (ite $x426 $x445 (ite $x424 $x445 $x448)))))))
 (let ((?x410 (top ?x114)))
 (let ((?x411 (stack ?x410 ?x158)))
 (let (($x440 (= ?x270 ?x411)))
 (let (($x435 (= c___ left-to-center)))
 (let (($x436 (and $x435 $x432)))
 (ite $x436 $x440 $x465)))))))))))))))))))))))))))))))
(assert
 (let ((?x273 (center s___)))
 (let ((?x412 (center s____)))
 (let (($x450 (= ?x412 ?x273)))
 (let ((?x158 (right s___)))
 (let ((?x419 (top ?x158)))
 (let ((?x421 (stack ?x419 ?x273)))
 (let (($x447 (= ?x412 ?x421)))
 (let (($x422 ((_ is stack ) ?x158)))
 (let (($x423 (= c___ right-to-center)))
 (let (($x424 (and $x423 $x422)))
 (let (($x425 (= c___ right-to-left)))
 (let (($x426 (and $x425 $x422)))
 (let ((?x414 (rest ?x273)))
 (let (($x442 (= ?x412 ?x414)))
 (let (($x427 ((_ is stack ) ?x273)))
 (let (($x428 (= c___ center-to-right)))
 (let (($x429 (and $x428 $x427)))
 (let (($x430 (= c___ center-to-left)))
 (let (($x431 (and $x430 $x427)))
 (let ((?x114 (left s___)))
 (let ((?x410 (top ?x114)))
 (let ((?x413 (stack ?x410 ?x273)))
 (let (($x441 (= ?x412 ?x413)))
 (let (($x432 ((_ is stack ) ?x114)))
 (let (($x433 (= c___ left-to-right)))
 (let (($x434 (and $x433 $x432)))
 (let (($x464 (ite $x434 $x441 (ite $x431 $x442 (ite $x429 $x442 (ite $x426 $x450 (ite $x424 $x447 $x450)))))))
 (let (($x435 (= c___ left-to-center)))
 (let (($x436 (and $x435 $x432)))
 (ite $x436 $x450 $x464)))))))))))))))))))))))))))))))
(assert
 (= c_____ c!4))
(assert
 (let ((?x251 (left s____)))
 (let ((?x329 (left s_____)))
 (let (($x590 (= ?x329 ?x251)))
 (let ((?x270 (right s____)))
 (let ((?x557 (top ?x270)))
 (let ((?x558 (stack ?x557 ?x251)))
 (let (($x584 (= ?x329 ?x558)))
 (let (($x560 ((_ is stack ) ?x270)))
 (let (($x563 (= c____ right-to-left)))
 (let (($x564 (and $x563 $x560)))
 (let ((?x412 (center s____)))
 (let (($x565 ((_ is stack ) ?x412)))
 (let (($x566 (= c____ center-to-right)))
 (let (($x567 (and $x566 $x565)))
 (let ((?x553 (top ?x412)))
 (let ((?x554 (stack ?x553 ?x251)))
 (let (($x581 (= ?x329 ?x554)))
 (let (($x568 (= c____ center-to-left)))
 (let (($x569 (and $x568 $x565)))
 (let ((?x362 (rest ?x251)))
 (let (($x577 (= ?x329 ?x362)))
 (let (($x570 ((_ is stack ) ?x251)))
 (let (($x571 (= c____ left-to-right)))
 (let (($x572 (and $x571 $x570)))
 (let (($x573 (= c____ left-to-center)))
 (let (($x574 (and $x573 $x570)))
 (ite $x574 $x577 (ite $x572 $x577 (ite $x569 $x581 (ite $x567 $x590 (ite $x564 $x584 $x590))))))))))))))))))))))))))))))))
(assert
 (let ((?x270 (right s____)))
 (let ((?x363 (right s_____)))
 (let (($x586 (= ?x363 ?x270)))
 (let ((?x556 (rest ?x270)))
 (let (($x583 (= ?x363 ?x556)))
 (let (($x560 ((_ is stack ) ?x270)))
 (let (($x561 (= c____ right-to-center)))
 (let (($x562 (and $x561 $x560)))
 (let (($x563 (= c____ right-to-left)))
 (let (($x564 (and $x563 $x560)))
 (let ((?x412 (center s____)))
 (let ((?x553 (top ?x412)))
 (let ((?x555 (stack ?x553 ?x270)))
 (let (($x582 (= ?x363 ?x555)))
 (let (($x565 ((_ is stack ) ?x412)))
 (let (($x566 (= c____ center-to-right)))
 (let (($x567 (and $x566 $x565)))
 (let (($x568 (= c____ center-to-left)))
 (let (($x569 (and $x568 $x565)))
 (let ((?x251 (left s____)))
 (let (($x570 ((_ is stack ) ?x251)))
 (let (($x571 (= c____ left-to-right)))
 (let (($x572 (and $x571 $x570)))
 (let (($x603 (ite $x572 $x586 (ite $x569 $x586 (ite $x567 $x582 (ite $x564 $x583 (ite $x562 $x583 $x586)))))))
 (let ((?x409 (top ?x251)))
 (let ((?x549 (stack ?x409 ?x270)))
 (let (($x578 (= ?x363 ?x549)))
 (let (($x573 (= c____ left-to-center)))
 (let (($x574 (and $x573 $x570)))
 (ite $x574 $x578 $x603)))))))))))))))))))))))))))))))
(assert
 (let ((?x412 (center s____)))
 (let ((?x550 (center s_____)))
 (let (($x588 (= ?x550 ?x412)))
 (let ((?x270 (right s____)))
 (let ((?x557 (top ?x270)))
 (let ((?x559 (stack ?x557 ?x412)))
 (let (($x585 (= ?x550 ?x559)))
 (let (($x560 ((_ is stack ) ?x270)))
 (let (($x561 (= c____ right-to-center)))
 (let (($x562 (and $x561 $x560)))
 (let (($x563 (= c____ right-to-left)))
 (let (($x564 (and $x563 $x560)))
 (let ((?x552 (rest ?x412)))
 (let (($x580 (= ?x550 ?x552)))
 (let (($x565 ((_ is stack ) ?x412)))
 (let (($x566 (= c____ center-to-right)))
 (let (($x567 (and $x566 $x565)))
 (let (($x568 (= c____ center-to-left)))
 (let (($x569 (and $x568 $x565)))
 (let ((?x251 (left s____)))
 (let ((?x409 (top ?x251)))
 (let ((?x551 (stack ?x409 ?x412)))
 (let (($x579 (= ?x550 ?x551)))
 (let (($x570 ((_ is stack ) ?x251)))
 (let (($x571 (= c____ left-to-right)))
 (let (($x572 (and $x571 $x570)))
 (let (($x602 (ite $x572 $x579 (ite $x569 $x580 (ite $x567 $x580 (ite $x564 $x588 (ite $x562 $x585 $x588)))))))
 (let (($x573 (= c____ left-to-center)))
 (let (($x574 (and $x573 $x570)))
 (ite $x574 $x588 $x602)))))))))))))))))))))))))))))))
(assert
 (let ((?x15 (stack A empty)))
 (let ((?x18 (stack C ?x15)))
 (let ((?x363 (right s_____)))
 (let (($x606 (= ?x363 ?x18)))
 (let ((?x550 (center s_____)))
 (let (($x637 (= ?x550 empty)))
 (let ((?x16 (stack B empty)))
 (let ((?x329 (left s_____)))
 (let (($x638 (= ?x329 ?x16)))
 (let (($x640 (not (and $x638 $x637 $x606))))
 (not $x640))))))))))))
(check-sat)