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
(assert
 (= s_tmp s))
(assert
 (= c_tmp c))
(assert
 (let ((?x37 (stack W empty)))
 (let ((?x38 (stack L ?x37)))
 (let ((?x39 (stack N ?x38)))
 (let ((?x40 (stack Y ?x39)))
 (let ((?x41 (stack J ?x40)))
 (let ((?x42 (stack H ?x41)))
 (let ((?x43 (stack B ?x42)))
 (let ((?x44 (stack D ?x43)))
 (let ((?x45 (stack U ?x44)))
 (let ((?x46 (stack F ?x45)))
 (let ((?x47 (stack A ?x46)))
 (let ((?x48 (stack G ?x47)))
 (let ((?x49 (stack S ?x48)))
 (let ((?x50 (stack R ?x49)))
 (let ((?x51 (stack E ?x50)))
 (let ((?x52 (stack T ?x51)))
 (let ((?x53 (stack C ?x52)))
 (let ((?x54 (stack V ?x53)))
 (let ((?x55 (stack X ?x54)))
 (let ((?x56 (stack P ?x55)))
 (let ((?x57 (stack M ?x56)))
 (let ((?x58 (stack I ?x57)))
 (let ((?x59 (stack O ?x58)))
 (let ((?x60 (stack Q ?x59)))
 (let ((?x61 (stack K ?x60)))
 (let ((?x133 (left s_tmp_)))
 (= ?x133 ?x61))))))))))))))))))))))))))))
(assert
 (let ((?x136 (center s_tmp__)))
 (= ?x136 empty)))
(assert
 (let ((?x139 (right s_tmp___)))
 (= ?x139 empty)))
(assert
 (= c_tmp____ c!0))
(assert
 (= s_ s_tmp___))
(assert
 (= c_ c_tmp____))
(assert
 (let ((?x166 (left s_tmp__)))
 (= (left s_tmp___) ?x166)))
(assert
 (let ((?x136 (center s_tmp__)))
 (= (center s_tmp___) ?x136)))
(assert
 (let ((?x170 (center s_tmp_)))
 (= ?x170 (center s_tmp))))
(assert
 (let ((?x173 (right s_tmp_)))
 (= ?x173 (right s_tmp))))
(assert
 (let ((?x133 (left s_tmp_)))
 (let ((?x166 (left s_tmp__)))
 (= ?x166 ?x133))))
(assert
 (let ((?x173 (right s_tmp_)))
 (let ((?x177 (right s_tmp__)))
 (= ?x177 ?x173))))
(assert
 (let ((?x82 (stack M (stack J (stack H (stack G (stack F (stack E empty))))))))
 (let ((?x86 (stack W (stack S (stack R (stack Q ?x82))))))
 (let ((?x113 (right s_)))
 (let ((?x76 (stack O (stack P (stack D (stack B (stack U (stack T empty))))))))
 (let ((?x115 (center s_)))
 (let ((?x62 (stack K empty)))
 (let ((?x63 (stack I ?x62)))
 (let ((?x64 (stack L ?x63)))
 (let ((?x65 (stack N ?x64)))
 (let ((?x66 (stack X ?x65)))
 (let ((?x67 (stack A ?x66)))
 (let ((?x68 (stack V ?x67)))
 (let ((?x69 (stack C ?x68)))
 (let ((?x70 (stack Y ?x69)))
 (let ((?x110 (left s_)))
 (let (($x183 (not (and (= ?x110 ?x70) (= ?x115 ?x76) (= ?x113 ?x86)))))
 (not $x183))))))))))))))))))
(check-sat)