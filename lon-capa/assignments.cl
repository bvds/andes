
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                Temporary holder for class-specific assignments
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *guerra-assigned* 
'("Andes assginments for St. Anselm"
(
("Chapter 1: Vectors" (VEC1AY VEC1C VEC3B))
("Chapter 2: Static Forces" (S1C S7B))
("Chapter 3: Rotational Equilibrium" (DR2A DR8A))
("Chapter 4: Gravity" (DT12A S11A))
("Chapter 5: Electric forces and Fields" (Coul1B Coul2A FOR1B FOR2A FOR10A))
("Chapter 6: Magnetic Forces" (MAG2A MAG3B MAG4A))
("Chapter 7: Fluid Forces" (Fluids1 Fluids9 Fluids11))
("Chapter 8: Four Forces of Nature (no ANDES problems))" ())
("Chapter 9: Kinematics" (KT1A KT3A KT6A KT8B KGRAPH2 KGRAPH3 KGRAPH5 KGRAPH7))
("Chapter 10: F=ma" (DT1A DT6B DT1C ELEC3B))
("Chapter 11: Friction & Inclines" (DT6C DT7A DT11B DT10A))
("Chapter 12: Parabolic Motion" (KT11B KT13A KT12A))
("Chapter 13: CIRCULAR MOTION" (ROTS3A ROTS8A ROTS6B MAG5A))
("Chapter 14: Rotational Motion" (KR2B KR4A KR7A DR6A))
("Chapter 15: Simple Harmonic Motion" (OSC3 OSC5 OSC8))
("Chapter 16: Waves" (WAVE1 WAVE5 WAVE6))
("Chapter 17: Sound Waves" (WAVE2 WAVE3 WAVE8))
("Chapter 18: Energy" (E2A E4B E5A E7B E9A))
("Chapter 19: Momentum" (IMP1 LMOM2A LMOM4A))
("Chapter 20: Angular Momentum" (MOMR2A MOMR3A))
("Chapter 21: Fluid Dynamics" (FLUIDS2 FLUIDS3 FLUIDS4 FLUIDS5))
("Chapter 22: Thermodynamics; No ANDES Questions" ())
("Chapter 23: Electric Circuits" (EQRES2A EQRES3A EQRES5A EQRES7A EQRES8A    
                    KIR2A KIR4A EPOW3))
("Chapter 24: Capacitance" (CAP2A CAP4A CAP6B CAP9A CAP9B))
("Chapter 25: Magnetic Fields" (MAG1A MAG6A MAG6C MAG8A FARA2A FARA4B FARA3A FARA5A FARA6B FARA10B FARA11A))
("Chapter 27: Geometric Optics" (Lens1A LENS2B LENS3A REF2C))
("Chapter 28: Physical Optics" (WAVE19 INT1A INT1B INT1D))
)))


(defvar *blackwood-assigned* 
'("Andes assignments for Blackwood"
(
("Vectors" (Vec1ay vec1b vec1d vec1e vec2a vec2c vec3a vec3b vec4a vec4b vec9 mot1 mot2))
("Kinematics Graphs" (kgraph1 kgraph2 kgraph3 kgraph5b kgraph5c kgraph6 kgraph8 kgraph11 kgraph13))
("Translational Kinematics" (kt1a kt1b kt2a kt3a kt4a kt5a kt6a kt8a kt9a kt9b kt9c kt10a kt10c kt11a kt11b kt11c kt13a kt13d))
("Free Body Diagrams" (fbd1b fbd2a fbd3a fbd4a fbd6a fbd8 fbd9 fbd11))
("Statics" (s1b s1c s1d s1f s2a s2b s4a s5a s6a s10a s11a s12a))
("Translations Dynamics" (dq1 dt1a dt1b dt1c dt2a dt3a dt3c dt4a dt6c dt7a dt9a dt12a dt13a dt13b dt16))
("Circular Motion" (rots1a rots3a rots4a rots7a))
("Work and Entergy" (Weq2 weq4 weq5 e1a e1b e2b e3a e4b e5a e5b e6a e9b e11a we1a we3a))
("Power" (pow1a pow1b pow3a pow5c))
("Linear Momentum" (lmom1a lmom2a imp1 imp2 imp3a cm3 roc1))
("Rotational Kinematics" (kr8 kr9 kr1a kr3b kr7a))
("Rotational Dynamics" (dr2a dr3a dr8a))
("Fluids" (fluids1 fluids9 fluids11 fluids12))
("Oscillations" (osc3 osc5 osc6 osc7 osc8))
("Waves" (wave1 wave2 wave3 wave10 wave12 wave24))
("Electric Field" (charge1a charge1b charge2 coul1a coul3 ediag1 efield1b efield1d efield2 efield3 efield4a efield4b for1a for7b elec1a elec1d gauss1 gauss4))
("Electric Potential" (epot1a epot2 pot1b pot3b))
("Resistance" (eqres1a eqres1c eqres1d eqres1e eqres4a eqres5a))
("Capacitance" (eqcap1a eqcap6a cap1b cap6a cap6b cap9b))
("DC Circuits" (kir1a kir1b kir3a kir4a epow1 epow3 epow4)) 
("Magnetic Field" (mag1b mag1c mag2a magtor1a mag6a mag7 mag8a))
("Electromagnetic Induction" (fara1a fara2a fara4a fara5c fara6a fara10a fara10b))
("Electromatics Waves" (emwave1 wave19 emwave3a))
("Optics" (mirror1 mirror2 mirror4 lens1a lens2b lens5a lens5b ref1 ref2a ref2b ref3a ref3b ref4a ref4b ref5a))
)))

(defvar *usna-fall-2007* 
'("Andes assignments for USNA, fall semester"
(
("Homework 1" (vec1b vec1d vec2a vec2b vec2d))
("Homework 2" (vec3b vec3c vec4a vec5a kt7a))
("Homework 3" (vec6a vec6c vec6d vec8a kt6b kt8a))
("Homework 4" (vec3a vec4b vec5b vec8b kt10a kgraph1))
("Homework 5" (kt3a kt5a kt9a kt9b kt13c kgraph3 kgraph4))
("Homework 6" (kt11a kt11b kt12a kgraph6 kgraph8 relvel1a relvel2a))
("Homework 7" (fbd1a fbd1b fbd2a fbd3a fbd6a))
("Homework 8" (s1b s1f s2b s2e s3c))
("Homework 9" (s4b s6a s11a dt1a dt2a dt4a))
("Homework 10" (dt13a dt14b dt11a dt9a))
("Homework 11" (rots1a rots3a rots4a))
("Homework 12" (rots6a rots6c rots7a))
("Homework 13" (dt13b dt4b dt6a))
("Homework 14" (dt7b dt12a dt14a))
("Homework 15" (e1a e1b e2a e2b e2c))
("Homework 16" (e8a e8b e9b e10a))
("Homework 17" (e4a e4b))
("Homework 18" (e5a e6a e7a))
("Homework 19" (pow1a pow3a pow4a pow5a pow6a))
("Homework 20" (lmom1a lmom2a lmom2b))
("Homework 21" (imp1 imp2 imp3a pgraph1 pgraph2 pgraph3))
("Homework 22" (lmom3a lmom4a))
("Homework 23" (lmom5 lmom6 lmom7))
("Homework 24" (cm2 cm3 roc1 roc2 roc4))
("Homework 25" (kr1a kr1b kr2b kr3a))
("Homework 26" (kr4a kr6a kr7a dr2a dr2b))
("Homework 27" (dr5a dr6a dr8a))
("Homework 28" (momr1a momr2a momr3a momr4a))
("Homework 29" (rots8a rots8b))
("Homework 30" (fluids1 fluids8 fluids9))
("Homework 31" (fluids11 fluids12 fluids14 fluids15))
("Homework 32" (fluids2 fluids3 fluids4 fluids5))
("Homework 33" (osc1 osc2 osc3 osc4))
("Homework 34" (osc5 osc6 osc7))
("Homework 35" (wave1 wave2 wave3 wave4))
("Homework 36" (wave8 wave9))
("Homework 37" (wave10 wave11 wave12))
("Homework 38" (wave5 wave6 wave15 wave16))
("Homework 39" (wave13 wave14 wave17 wave18))
)))


(defvar *usna-spring-2008* 
'("Andes assignments for USNA, spring semester"
(
("Charges; Charging; Coulomb's Law" (Charge1b charge2 Coul1b coul1c coul2a))
("The Electric Field" 
 (Efield1a efield1b efield1c efield1d efield1e efield3 efield4a))
("Motion of Charged Particles in Electric Field" 
 (For1a for1c for4a for7a for9a elec3b))
("Electric Flux; Gauss's Law for Electric Fields" 
 (Gauss1 gauss3 gauss4 gauss5))
("Applications of Gauss' Law" (Gauss8 gauss9))
("Potential Difference and Electric Potential" 
 (Epot1a epot1b epot2 pot1b pot2a))
("Potential Due to Point Charges E from V" (Pot3b pot6 pot8))
("Capacitance and Capacitors" (Cap2b cap4a cap5a cap6a cap6b))
("Equivalent capacitors" 
 (Eqcap1a eqcap1b eqcap1c eqcap6a))
("Capacitors with Dielectrics; Dipoles" (Cap9a cap9b dip1a dip1b))
("Current and Resistance" (Epow1 epow2 epow3 epow4))
("Emf Combinations of Resistors" 
 (Eqres1a eqres1b eqres1e eqres5a eqres7a eqres7b eqres8a eqres8b))
("Kirchhoff's Rules" (Kir1a kir2a kir3a kir4a kir5a))
("RC Circuits" (RC1a rc2a rc3a rc4a rc5a))
("Magnetic Fields; Forces on Conductors" (Mag1a mag2a mag3b mag4a mag5a ))
("Magnetic Forces on Moving Charges" 
 (Magdip1 magdip2 magtor1a magtor1b magtor1c))
("Biot-Savart Law; Forces - Parallel Conductors" 
 (Mag6a mag6b mag7 mag8a mag12))
("Amp&egrave;re's Law and applications" (Amp1))
("Magnetic Flux; Gauss's Law; Amp&egrave;re's Law" 
 (Fara1a fara1b fara2a fara2b fara3a fara3b))
("Faraday's Law of Induction" (Fara4a fara4b fara5a fara5b fara5c))
("Motional emf; Lenz's Law" 
 (Fara6a fara6b fara7a fara7b fara7c fara7d fara8a))
("Induced emf and Induced Electric Fields" (Fara9 fara11a))
("Self Inductance; RL Circuits" (Ind1a ind3b ind4))
("Magnetic Energy Storage; Mutual Inductance" (LR1b lr2a lr3a lr3b))
("LC and RLC Circuits" (LC1a lc2a lrc1a lrc2a))
("Ampere's Law;Maxwell's Eqs; EM Waves" 
 (Emwave1 emwave3a emwave4 emwave5 wave19))
("Geometric Optics; Reflection" (Ref1 ref2a ref2b ref2c))
("Refraction; Prisms; Total Internal Reflection" (Ref3b))
("Images formed by Mirrors" (Mirror1 mirror2 mirror3 mirror4))
("Images formed by Refracting Surfaces" (Lens1a lens1b lens2a lens2b lens3a))
("Thin Lenses" (Lens3b lens4a lens4b lens5a lens5b))
("Double-Slit Interference" (Int1a int1b int1c int1d))
("Thin Film Interference" (Int2a int2b))
("Diffraction by Small Apertures" (Ref4a ref4b ref5a ref5b))
)))
