; KINEMATICS
(sdd ?b (during ?t1 ?t2))	; = average speed def.
(avg-velocity ?b (during ?t1 ?t2))
    ; 1D CONSTANT ACCELERATION
    (compo-free lk-no-s ?xyz ?rot (lk ?b (during ?t1 ?t2)) ; = avg-accel def.
    (compo-free lk-no-t ?xyz ?rot (lk ?b (during ?t1 ?t2))
    (compo-free lk-no-vf ?xyz ?rot (lk ?b (during ?t1 ?t2))
    (compo-free lk-no-a ?xyz ?rot (lk ?b (during ?t1 ?t2))
    ; FREELY FALLING BODIES
    (free-fall-accel ?b ?t) ; = g
    (std-constant g)
    ; UNIFORM CIRCULAR MOTION
    (centripetal-accel ?b ?t)
; MISCELLANEOUS 
(equals (?quant1 ?quant2)) ; known equivalence, e.g dist. out = dist. back
(sum-times ?t0 ?t1 ?t2)    ; t02 = t01 + t12
; NEWTONS LAWS
    (compo-free NFL ?xyz ?rot (NL ?b ?t)) ; when accel is zero
    (compo-free NSL ?xyz ?rot (NL ?b ?t)) ; with sum of forces
    (compo-free NSL-net ?xyz ?rot (NL-net ?b ?t)) ; with net force
    (NTL (?b1 ?b2) ?force-type ?t)
; FORCE LAWS
(wt-law ?b ?t)
(kinetic-friction ?b ?surface ?t)
(static-friction ?b ?surface ?t) ; at max
(num-forces ?b ?t)		 ; silly, num-forces = <count of the forces>
; CONNECTED BODIES
(connected-accels ?b1 ?b2 ?t)
(connected-velocities ?b1 ?b2 ?t)
(tensions-equal ?string (?b1 ?b2) ?t)   ; or should this go under force laws?
; COMPOUND BODIES 
(mass-compound ?b1 ?b2 ...)   ; b_i = bodies making up compound
(kine-compound ?vec-type ?bi (?b1 ?b2 ...) ?t) ; part, whole same kinematics
(force-compound ?type ?agent (?b1 ?b2 ...) ?t)
; WORK
(work ?b ?agent (during ?t1 ?t2))     ; by a single force
(net-work ?b (during ?t1 ?t2))        ; by all forces
; CONSERVATION OF ENERGY
(cons-energy ?b ?t1 ?t2)
; LINEAR MOMENTUM
(cons-linmom (?b1 ?b2 ...) (during ?t1 ?t2))
(cons-ke-elastic (?b1 ?b2 ...) (during ?t1 ?t2)) ; KEi=KEf for elastic coll.
; ROTATIONAL KINEMATICS
(ang-sdd ?b ?t)
(linear-vel ?pt ?t)
    ; CONSTANT ANGULAR ACCELERATION
    (compo-free rk-no-s z 0 (rk-no-s ?b (during ?t1 ?t2)))
    (compo-free rk-no-vf z 0 (rk-no-vf ?b (during ?t1 ?t2)))
    (compo-free rk-no-t z 0 (rk-no-t ?b (during ?t1 ?t2)))
; MOMENT OF INERTIA
(I-rod-cm ?b ?t)
(I-rod-end ?b ?t)
(I-hoop-cm ?b ?t)
(I-disk-cm ?b ?t)
(I-rect-cm ?b ?t)
(I-compound (?b1 ?b2 ...) ?t)
; ANGULAR MOMENTUM
(ang-momentum ?b ?t)
(cons-angmom (?b1 ?b2 ...) (during ?t1 ?t2))
; ROTATIONAL DYNAMICS (TORQUE)
(net-torque-zc ?b ?axis ?t)
(mag-torque ?b ?axis (force ?pt ?agent ?type) ?t)
(NL-rot ?b ?axis ?t)
