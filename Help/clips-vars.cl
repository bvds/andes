;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; clips-vars -- functions for mapping CLIPS variable names to Andes2 KB
;;               quantity expressions.
;;
;; This is needed to handle the ids sent as arguments in the check-answer API
;; from Andes1 problem files. The id was of the form Answer-CLIPSVAR where 
;; the CLIPSVAR part was the solution variable denoting the relevant sought.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :user)

; split-sym - Split out separator-char-delimited fields from structured symbol 
; Default separator is underscore.
; Returns: list of objects read from fields, expected to be symbols or numbers.
; Ex: (split-sym 'FOO_BAR_DURING_1_2) => (FOO BAR DURING 1 2)
; Notes: Empty fields will cause Lisp error if present.
;        Generally useful, could be moved to utils.
(defun split-sym (item &optional (sep #\_))
   (let* ((itemstr (format NIL "~A" item)) ; may get number in recursive call
          (pos (position sep itemstr)))
	(if pos (cons (read-from-string (subseq itemstr 0 pos))
				  (split-sym (read-from-string 
				  	(subseq itemstr (1+ pos))) sep))
	  (list item))))
	  
; convert CLIPS body name to Andes2 KB body term
(defun cbody (name)
   ; for compound body names, split and form (compound a b)
   (if (find #\$ (string name) :test #'equal)
      `(compound ,@(sort (mapcar #'fix-body-name (split-sym name #\$)) 
                         #'expr<))
    ; may need to convert hyphens to underscores in body names 
    (fix-body-name name)))

; Convert list of CLIPS time spec args into KB time term
; For convenience, arg list may include trailiing choice pt tags 
; from variable name. They will be ignored if present
(defun ctime (args)
  (cond ((numberp (first args)) ; time point number
              (first args)) 
        ((equal (first args) 'during) ; (during pt1 pt2 [tags])
	       `(during ,(second args) ,(third args))) 
	(T (get-default-time))))  ; Assume it's Ex* = problem time

; Convert CLIPS variable name to Andes2 quantity expression
(defun clips-var->quant (var)
  (let ((args (split-sym var)))
   (case (first args)
     (dist   `(distance ,(cbody (second args))
                 :time ,(ctime (cddr args))))
     (height `(height ,(cbody (second args)) ; third arg = ref point
                 :time ,(ctime (cdddr args)))) 
     (ke     `(kinetic-energy ,(cbody (second args))
                 :time ,(ctime (cddr args))))
     (mag     `(mag ,(clips-vector (rest args))))
     (mass   `(mass ,(cbody (second args))))
     (ntwd   `(net-work ,(cbody (second args))
                 :time ,(ctime (cddr args))))
     (num    `(num-forces ,(cbody (fourth args))
                  :time 1)) ; no time in CLIPS var, assume default time OK
     (phi     `(dir ,(clips-vector (rest args))))
     (speed  `(speed ,(cbody (second args))
                 :time ,(ctime (cddr args))))
     (the    `(duration (during ,(fifth args) ,(sixth args))))
     (theta   `(dir ,(clips-vector (rest args))))
     (twd    `(work ,(cbody (second args)) ,(cbody (third args))
                 :time ,(ctime (cdddr args))))
     (xc     (vector-compo (clips-vector (rest args)) '(axis x 0)))
     (yc     (vector-compo (clips-vector (rest args)) '(axis y 0)))
     (zc     (vector-compo (clips-vector (rest args)) '(axis z 0)))
     (otherwise (error "Unrecognized CLIPS var: ~A~%" var))
   )))

; Convert CLIPS vector-specifying arg list to vector expression
(defun clips-vector (args)
  ; Because 'T has special meaning in case statement:
  (if (eql (first args) T)     
         `(force ,(cbody (second args)) ,(cbody (third args)) TENSION
	             :time ,(ctime (cdddr args)))
  ; else:
   (case (first args)
    (A     `(accel ,(cbody (second args))
               :time ,(ctime (cddr args))))
    (ALPHA `(ang-accel ,(cbody (second args))
               :time ,(ctime (cddr args))))
    (CF	   `(force ,(cbody (second args)) ,(cbody (third args)) 
                 ; contact force now NORMAL in some prbs, APPLIED in others
                  ,(if (member (problem-name *cp*) '(EXDT3A EXDT14A EXDT15A)) 
		     'NORMAL 'APPLIED)
	       :time ,(ctime (cdddr args))))
    (DISP  `(displacement ,(cbody (second args))
               :time ,(ctime (cddr args))))
    (F     `(force ,(cbody (second args)) ,(cbody (third args)) 
		   ; should be kinetic friction, but wrong in EXS8A 
                   ,(if (eql (problem-name *cp*) 'EXS8A) 'STATIC-FRICTION
		      'KINETIC-FRICTION)
	       :time ,(ctime (cdddr args))))
    (GF    `(force ,(cbody (second args)) ,(cbody (third args)) APPLIED
	       :time ,(ctime (cdddr args))))
    (L     `(ang-momentum ,(cbody (second args))
               :time ,(ctime (cddr args))))
    (N     `(force ,(cbody (second args)) ,(cbody (third args)) NORMAL
	       :time ,(ctime (cdddr args))))
    (NF    `(net-force ,(cbody (second args))
               :time ,(ctime (cddr args))))
    (NTOR  `(net-torque ,(cbody (second args)) ,(cbody (third args))
               :time ,(ctime (cdddr args))))
    (OMEGA `(ang-velocity ,(cbody (second args))
               :time ,(ctime (cddr args))))
    (SF    `(force ,(cbody (second args)) ,(cbody (third args)) STATIC-FRICTION
	       :time ,(ctime (cdddr args))))
    (THETA `(ang-displacement ,(cbody (second args))
               :time ,(ctime (cddr args))))
    (V     `(velocity ,(cbody (second args)) 
               :time ,(ctime (cddr args))))
    (W     `(force ,(cbody (second args)) ,(cbody (third args)) WEIGHT
	       :time ,(ctime (cdddr args))))
    (otherwise (error "Unrecognized CLIPS vector abbrev: ~A~%" (first args)))
  )))

#| 
; For reference  and testing, following lists all variables used for 
; sought answers in Andes1 CLIPS problems:
(defvar *clips-answer-vars* '(
DIST_BULLET_DURING_1_2_B3-1
DIST_CAR_DURING_2_3_B3-1
DIST_MOTORCYCLE_DURING_1_3_B1-1
DIST_SOUND_DURING_2_3_B1-1
HEIGHT_BLOCK_UNCP-SPRING_3_B1-1
KE_BLOCK_1_B1-1
KE_BLOCK_2_B1-1
MAG_A_BLOCK1$BLOCK2_EXDT3A_B1-1 MAG_A_BLOCK2_EXDT3A_B1-2
MAG_A_MAN_EXDT13A_B1-1
MAG_A_MOTORBOAT_DURING_1_3_B1-1
MAG_A_MOTORCYCLE_DURING_1_2_B1-1
MAG_A_SOAPBOX_DURING_1_2_B1-1
MAG_A_SUPERTANKER_DURING_1_2_B1-1
MAG_ALPHA_FLYWHEEL_EXTOR5A_B1-1
MAG_ALPHA_PLATE_EXTOR6A_B1-1
MAG_ALPHA_WHEEL_DURING_1_2_B1-1
MAG_CF_AIR-BAG_DRIVER_DURING_1_2_B1-1	; now applied
MAG_CF_BLOCK2_BLOCK1_EXDT3A_B1-2	; now normal
MAG_CF_BOX_CRATE_EXDT14A_B3-1		; now normal
MAG_CF_CRATE_BOX_EXDT15A_B3-1		; now normal
MAG_CF_PIER_CARRIER_DURING_1_2_B1-1	; now applied
MAG_CF_VEST_BULLET_DURING_1_2_B1-1	; now applied
MAG_DISP_BLOCK2_DURING_1_2_B1-1
MAG_DISP_JOGGER_DURING_1_7_B1-1
MAG_DISP_SUPERTANKER_DURING_1_2_B1-1
MAG_F_CART_GROUND_EXDT10A_B1-1
MAG_F_LOVESEAT_FLOOR_EXS8A_B1-1
MAG_GF_CRATE$BOX_MID_EXDT14A_B1-1
MAG_GF_CRATE_MID_EXDT14A_B1-2 MAG_GF_CRATE$BOX_MID_EXDT14A_B1-1
MAG_GF_L-END_UNSPECIFIED_EXTOR7A_B1-1
MAG_GF_TL-CORN_UNSPECIFIED_EXTOR4A_B1-1
MAG_L_BAR_EXMOMR1B_B1-1
MAG_L_HOOP_EXMOMR2A_B1-1
MAG_L_METERSTICK_EXMOMR1A_B1-1
MAG_N_BLOCK_PLANE_EXS4A_B1-1
MAG_N_BOOK_PACKAGE_EXS3A_B4-1
MAG_N_BOX_PLANE_EXDT15A_B1-1
MAG_N_CART_GROUND_EXDT10A_B1-1
MAG_N_LOVESEAT_FLOOR_EXS8A_B1-1
MAG_N_PACKAGE$BOOK_TABLE_EXS3A_B1-1
MAG_N_PACKAGE_TABLE_EXS3A_B1-2 MAG_N_PACKAGE$BOOK_TABLE_EXS3A_B1-1
MAG_NF_BULLET_DURING_1_2_B3-1
MAG_NF_CARRIER_DURING_1_2_B3-1
MAG_NF_DRIVER_DURING_1_2_B1-1
MAG_NF_DRIVER_DURING_1_2_B3-1
MAG_NF_F-14_DURING_1_2_B1-1
MAG_NF_TROOPER_DURING_1_2_B1-1
MAG_NTOR_PLATE_CM_EXTOR3A_B1-1
MAG_OMEGA_DISK1$DISK2_2_B1-1
MAG_OMEGA_MAN-WEIGHT-SYSTEM_2_B1-1
MAG_OMEGA_PULLEY2_EXKR6A_B1-1
MAG_OMEGA_WHEEL_2_B1-1
MAG_SF_BLOCK_PLANE_EXDT4A_B1-1
MAG_T_AIRPLANE_S2_EXS2A_B1-1
MAG_T_ASTRONAUT_ROPE_EXS1F_B1-1
MAG_T_BLOCK_LINE_EXDR4A_B1-1
MAG_T_BLOCK_STRING_EXS1A_B1-1
MAG_T_BLOCK_STRING_EXS4A_B1-1
MAG_T_BUNGEE-JUMPER_BUNGEE_EXS1C_B1-1
MAG_T_CLIMBER_SAFETY-LINE_EXS1B_B1-1
MAG_T_KNOT_C1_EXS7A_B1-1
MAG_T_KNOT1_S2_EXS5A_B5-1
MAG_T_KNOT1_S3_EXS5A_B5-1
MAG_T_KNOT2_S1_EXS5A_B3-1
MAG_T_MIDSHIPMAN_CABLE_EXS1E_B1-1
MAG_T_PACKAGE_STRING_DURING_1_2_B1-1
MAG_T_SEAL_HARNESS_EXS1D_B1-1
MAG_THETA_WHEEL_DURING_1_2_B1-1
MAG_THETA_WHEEL_DURING_1_2_B1-1
MAG_THETA_WHEEL_DURING_1_2_B1-1
MAG_THETA_WHEEL_DURING_1_2_B1-1
MAG_V_BALL_1_B1-1
MAG_V_BALL_DURING_1_2_B1-1
MAG_V_BALL_DURING_1_2_B1-1
MAG_V_BALL_EXDR2A_B1-1
MAG_V_BALL1_DURING_2_3_B1-1
MAG_V_BALL1_DURING_2_3_B1-1
MAG_V_BEE_DURING_1_2_B1-1
MAG_V_BLOCK_2_B1-1
MAG_V_BLOCK_EXDR3A_B1-1
MAG_V_BLOCK_EXDR6A_B1-1
MAG_V_BLOCK_EXDR7A_B1-1
MAG_V_CANNONBALL_1_B1-1
MAG_V_CAR_2_B1-1
MAG_V_CAR_2_B1-1
MAG_V_CAR_2_B1-1
MAG_V_CAR_EXDR1A_B1-1
MAG_V_CAR_EXDR9A_B1-1
MAG_V_CHILD_DURING_2_3_B1-1
MAG_V_HAILSTONE_2_B1-1
MAG_V_HAILSTONE_2_B1-1
MAG_V_KANGAROO_1_B1-1
MAG_V_KID_DURING_2_3_B1-1
MAG_V_POINT_EXKR7A_B1-1
MAG_V_PROBE_DURING_2_3_B1-1
MAG_V_SKATER1$SKATER2_DURING_2_3_B1-1
MAG_V_TELESCOPE_DURING_2_3_B1-1
MAG_W_BLOCKB_EARTH_EXS6A_B1-1
MAG_W_W2_EARTH_EXS5A_B1-1
MASS_AIRPLANE_EXS2A_B1-1
MASS_BLOCK_EXDR7A_B1-1
NTWD_CART_DURING_1_2_B1-1
NTWD_CART_DURING_1_2_B1-1
NUM_FORCES_ON_BLOCK2
PHI_L_BAR_EXMOMR1B_B1-1
PHI_L_HOOP_EXMOMR2A_B1-1
PHI_L_METERSTICK_EXMOMR1A_B1-1
SPEED_AIRCRAFT_DURING_1_2_B1-1
SPEED_BEE_DURING_1_2_B1-1
THE_TIME_AT_DURING_1_2
THE_TIME_AT_DURING_1_2
THE_TIME_AT_DURING_1_2
THE_TIME_AT_DURING_1_2
THE_TIME_AT_DURING_1_3
THE_TIME_AT_DURING_2_3
THETA_V_BALL1_DURING_2_3_B1-1
THETA_V_BALL1_DURING_2_3_B1-1
TWD_CART_EARTH_DURING_1_2_B1-1
TWD_CART_GIRL_DURING_1_2_B1-1
TWD_CRATE_MAN_DURING_1_2_B1-1
TWD_SUITCASE_STRAP_DURING_1_2_B1-1
TWD_TOURIST_ELEVATOR_DURING_1_2_B1-1
XC_A_MAN_EXDT12A_B1-1_A2-1
XC_A_PARTICLE_DURING_1_2_B1-1_A2-1
XC_DISP_BALL_DURING_1_2_B1-1_A2-1
XC_DISP_SHOE_DURING_1_3_B1-1_A2-1
XC_V_CANNONBALL_2_B1-1_A2-1
XC_V_FLARE_2_B1-1_A2-1
XC_V_PARTICLE_1_B1-1_A2-1
XC_V_PARTICLE_2_B1-1_A2-1
YC_A_MAN_EXDT12A_B1-1_A2-1
YC_A_PARTICLE_DURING_1_2_B1-1_A2-1
YC_DISP_BULLET_DURING_1_2_B1-1_A2-1
YC_DISP_FIRECRACKER_DURING_1_2_B1-1_A2-1
YC_V_CANNONBALL_2_B1-1_A2-1
YC_V_FLARE_2_B1-1_A2-1
YC_V_PARTICLE_1_B1-1_A2-1
YC_V_PARTICLE_2_B1-1_A2-1
ZC_NTOR_BAR_L-END_EXTOR1A_B1-1
ZC_NTOR_BAR_L-END_EXTOR1B_B1-1
ZC_NTOR_BAR_L-END_EXTOR2A_B1-1
))
|#
