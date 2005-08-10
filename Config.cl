;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Config.cl
;;; Collin Lynch
;;; Collinl@pitt.edu
;;;
;;; This file contains startup and initialization code for Andes
;;; and should be modifed to change runtime conditions only.
;;;
;;; NOTE:: Comments in this file are preceeded by a semicolon
;;;   or enclosed by the hash-pipe braces ("#|" and "|#").
;;;   If you wish to add your own comments please follow my
;;;   example here and preceed each line of comments with 
;;;   a pair or more of semicolons ";;".
;;;
;;; If any problem arises please e-mail me.


;;; --------------------------------------------------------
;;; Setting Score Weights.  
;;; This function will be called at runtime to set the weights 
;;; for each of the registered runtime tests.  If a weight is 
;;; zero the test's score value will not be used to compute the 
;;; total score.  If it is more or less than zero it will be used.  
;;;
;;; The total score is computed as a linear function.  The current
;;; value of each test is multiplied by it's weight and the results
;;; are summed to produce the total score.  This value will be
;;; submitted as a percentage (value between 0 and 100) to the
;;; workbench.  The actual output of the function is a value 
;;; between 0 and 1.  It will be multiplied by 100 and truncated
;;; to the nearest whole number to produce the result.  
;;;
;;; The tests themselves can be classified into two groups.  The first
;;; set consist of relational statistics such as the number of 
;;; correct entries made over the total number of entries made.  These
;;; tests range in value from 0 to 1.  
;;;
;;; The incremental or statistical tests are tests that count some value
;;; such as the number of times that the student called for procedural
;;; (next-step) help.  These tests range in value from 0 to some 
;;; unspecified number.  
;;;
;;; In the code below I have included all of the tests defined.  I have
;;; grouped the tests into two sections.  The first section contains all
;;; of the relational tests.  These are the ones that I recommend you use
;;; the second section includes all of the incremental tests.  They have
;;; been assigned weights of 0.  You may experiment with adding them 
;;; weights but I would recommend that you avoid doing so as the resulting
;;; totals will be somewhat nonsensical.  
;;;
;;; If you explicitly wish to use one of them in computing the total please
;;; e-mail me and I will see what I can do to threshold the value for you.
;;;
;;; The code below this comment block is a single function call with 
;;; interspersed comments.  The comments are preceeded by semicolons.
;;; The Function name is on the first line after this block and should
;;; remain unchanged.  
;;;
;;; The arguments are lists of the form (<TestName> <Weight> <CreditType>) 
;;; with thefirst argument being preceeded by an extra apostrophie and 
;;; parenthesis and the last line being followed by an extra pair of 
;;; paramtheses.  
;;;
;;; In order to modify the test weights all that you need to do is change
;;; the second and third values in each list.  The next time that Andes 
;;; is loaded it will use those weights for computation.
;;;
;;; At runtime the total weight of the "credit" tests will be scaled so 
;;; that the student can score between 0 and 100 simply by satisfying 
;;; all of them.  Those tests labelled "Extra" will be added to the score
;;; above and beyond the 100% potential.  The Scores labelled "Debit" 
;;; will be subtracted from the score making negative scores possible. 
(set-runtime-test-weights
 '(
   ;; =================================================================
   ;; The problems in this section are the ratio problems and should be
   ;; used for the student's score.  All of them range in value from
   ;; 0 to 1.
   
   ;; ---------------------------------------------------------------
   ;; The answer entry subscore is a ratio of the number of correct
   ;; answer entries that the student has made relative to the 
   ;; number of answer entries that they need to make.  This test 
   ;; will only be used on problems that seek a numerical answer 
   ;; (most of them).  On the Fbd-only problems where the students
   ;; must select a check-box the mc-answer test below will be used.
   ;;
   ;; On fbd-only problems this test will be treated as if its weight
   ;; was 0.
   (Answer_Entry_Subscore 0.20 Credit)
   
   ;; --------------------------------------------------------------
   ;; The MC-Answer-entry subscore is a ratio of the number of correct
   ;; multiple-choice answer entries that the student has made relative
   ;; to the number of multiple choice answer entries that they need 
   ;; to make.  
   ;;
   ;; The multiple choice answer entries are used on the fbd-only 
   ;; problems alone where the students must select a checkbox.  On
   ;; all other problems this will be treated as if the assinged weight
   ;; was 0.  
   ;;
   ;; I recommend that you assign the same weight to this test and to
   ;; the one above.  
   (MC_Answer_Entry_Subscore 0.20 Credit)
   
   ;; ----------------------------------------------------------------
   ;; The equation subscore retuerns a ratio of the number of 
   ;; major equations that the student has written in any form
   ;; (even combined with other equations) relative to the number
   ;; of major equations that they need to write overall.  
   ;;
   ;; NOTE::  If the student has entered a correct answer or answers
   ;;  to the problem then this number will be 1 as a correct answer
   ;;  is treated as a combination of all the equations.  
   ;;
   ;; NOTE:: This score (like many below) is dependent upon the 
   ;;  solution that the student is pursuing.  If they ar pursuing a
   ;;  solution with 7 major equations then the score is n/7.  If 
   ;;  we determine that they have shifted to a solution that has 
   ;;  6 entries then the score will be m/6.  Where n and m are the
   ;;  number of equations in the solution that the student has used.
   ;;  
   ;;  Andes assumes that the student is working on whatever solution
   ;;  gives them the best score overall.
   (Equation_entry_Subscore 0.20 Credit)
   
   ;; ---------------------------------------------------------------
   ;; The explicit equation entry subscore tracks the number of major
   ;; equations that the student has written explicitly (no 
   ;; substitutions of any kind) over the number of major equations 
   ;; that they need for their current solution.  This score too is 
   ;; solution dependent.  
   (Explicit_Equation_Entry_Subscore 0.15 Credit)
   
   ;; ---------------------------------------------------------------
   ;; The given-equation-entry subscore tracks the number of necessary 
   ;; given equations (an assignment statement for a given value) that 
   ;; the student has used in any form (including substitutions).  Like
   ;; the Equation_Entry_Subscore above, a correct answer will set this
   ;; to 1.
   (Given_Equation_Entry_Subscore 0.10 Credit)
   
   ;; ----------------------------------------------------------------
   ;; The explicit given-equation-entry-subscore tracks the number of 
   ;; necessary given equations (an assignment statement for a given
   ;; value) that the student has written explicitly (no substitutions).
   ;;
   ;; This value is similar to the Explicit_Equation_Entry_Subscore.
   (Explicit_Given_Equation_entry_Subscore 0.05 Credit)
   
   ;; ----------------------------------------------------------------
   ;; The Body_Entry_Subscore is used to track the number of necessary
   ;; body entries that the student has made.  This too is solution
   ;; dependent.
   (Body_Entry_Subscore 0.10 Credit)
   
   ;; -----------------------------------------------------------------
   ;; The Axis-Entry-Subscore tracks the number of necessary axis entries 
   ;; that the student has made.  This too is solution dependent.
   (Axis_Entry_Subscore 0.10 Credit)
   
   ;; -----------------------------------------------------------------
   ;; NSH_BO_V_NSH Records the number of times that the student called
   ;; next-step-help and went to a bottom-out hint over the number of
   ;; times that they called Next-step-help overall.  
   (NSH_BO_V_NSH 0 Nil)
   
   ;; -----------------------------------------------------------------
   ;; WWH_BO_V_WWH records a ratio of the number of times that the 
   ;; student called for what's wrong-help and went to the bottom-out
   ;; hint relative to the number of times that they called WWH overall.
   (WWH_BO_V_WWH 0 Nil)
   
   ;; -----------------------------------------------------------------
   ;; WWO_BO_V_WWO records a ratio of the number of times the student
   ;; called What's wrong help on an object or the answer box (don't 
   ;; ask) and wen't to a bottom-out-hint relative to the number of 
   ;; times that they caled WWO overall.
   (WWO_BO_V_WWO 0 Nil)
   
   ;; -----------------------------------------------------------------
   ;; WWE_BO_V_WWE records the number of times that the student asked
   ;; for what's wrong help on an equation and bottomed-out relative
   ;; to the number of times that they asked for what's wrong help
   ;; overall.
   (WWE_BO_V_WWE 0 Nil)
   
   
   ;; -----------------------------------------------------------------
   ;; Correct_Entries_V_Entries records the ratio between the number
   ;; of correct entries that the student made while working on this
   ;; problem and the number of entries overall.  
   ;; 
   ;; NOTE:: This is not solution dependent.
   (Correct_Entries_V_Entries 0.05 Credit)
   
   ;; -----------------------------------------------------------------
   ;; Correct_EQ_Entries_V_EQ_Entries records the ratio between the 
   ;; number of correct equation entries that the student made while 
   ;; working on this problem and the number of entries overall.  
   ;; 
   ;; NOTE:: This is not solution dependent.
   (Correct_EQ_Entries_V_EQ_Entries 0 Nil)
   
   ;; -----------------------------------------------------------------
   ;; Correct_NonEQ_Entries_V_NonEQ_Entries records the ratio between 
   ;; the number of correct non-equation entries that the student made 
   ;; while working on this problem and the number of entries overall.  
   ;; 
   ;; NOTE:: This is not solution dependent.
   (Correct_NonEQ_Entries_V_NonEQ_Entries 0 Nil)
   
   ;; -----------------------------------------------------------------
   ;; Correct_Answer_Entries_V_Answer_Entries records the ratio between 
   ;; the number of correct answer entries that the student made 
   ;; while working on this problem and the number of answer entries 
   ;; overall.  
   ;; 
   ;; NOTE:: This is not solution dependent.
   (Correct_Answer_Entries_V_Answer_Entries 0.05 Credit)
   
   
   
   
   
   ;;; =================================================================
   ;;; The code in this section are incremental counts and increment to
   ;;; keep track of a number.  They are not necessary for use in the
   ;;; Total and only one of them is used (NSH_BO_CALL_COUNT).
   
   ;; --------------------------------------------------------------
   ;; The NSH-call-count records the number of time that the
   ;; student called for Next-Step-Help (Procedural Help).
   (NSH_Call_Count 0 Nil)
   
   ;; --------------------------------------------------------------
   ;; The NSH-BO-call-count records the number of time that the
   ;; student called for Next-Step-Help (Procedural Help) and followed
   ;; the hint sequence to a bottom-out hint.
   (NSH_BO_Call_Count -0.05 Debit)
   
   ;; --------------------------------------------------------------
   ;; WWH_Call_Count records the number of times that the student
   ;; asked for what's wrong help on any incorrect entry.
   (WWH_Call_Count 0 Nil)
   
   ;; --------------------------------------------------------------
   ;; WWH_BO_Call_Count records the number of times that the student
   ;; asked for what's wrong help on any incorrect entry and followed
   ;; the help to a bottom-out-hint.
   (WWH_BO_Call_Count -0.05 Debit)
   
   ;; --------------------------------------------------------------
   ;; WWO_Call_Count records the number of times that the student
   ;; asked for what's wrong help on a non-equation entry or an 
   ;; answer box (don't ask).  
   (WWO_Call_Count 0 Nil)

   ;; --------------------------------------------------------------
   ;; WWO_BO_Call_Count records the number of times that the student
   ;; asked for what's wrong help on a non-equation entry or an 
   ;; answer box (don't ask) and followed it to the bottom-out-hint.  
   (WWO_BO_Call_Count 0 Nil)
   
   ;; --------------------------------------------------------------
   ;; WWE_Call_Count records the number of times that the student
   ;; asked for what's wrong help on an equation.  
   (WWE_Call_Count 0 Nil)
   
   ;; --------------------------------------------------------------
   ;; WWE_BO_Call_Count records the number of times that the student
   ;; asked for what's wrong help on an equation and followed the 
   ;; hint sequence to the bottom-out-hint.
   (WWE_BO_Call_Count 0 Nil)
   
   ;; --------------------------------------------------------------
   ;; Unsol_Help_count records the number of times that the studnet
   ;; made some incorrect entry and we gave them an unsolicited help
   ;; message such as "check your signs" or "missing units on number."
   (Unsol_Help_count 0 Nil)
   
   ;; -------------------------------------------------------------
   ;; Num_Entries records the total number of entries (of any type)
   ;; that the student made while working on this problem.
   (Num_Entries 0 Nil)
   
   ;; -------------------------------------------------------------
   ;; Num_Correct_Entries records the total number of correct entries
   ;; that the student made while working on this problem.
   (Num_Correct_Entries 0 Nil)
    
   ;; -------------------------------------------------------------
   ;; Num_Incorrect_Entries records the total number of incorrect 
   ;; entries that the student made while working on this problem.
   (Num_Incorrect_Entries 0 Nil)
   
   ;; -------------------------------------------------------------
   ;; Num_Eq_Entries records the total number of equation entries 
   ;; that the student made while working on this problem.
   (Num_Eq_Entries 0 Nil)
   
   ;; -------------------------------------------------------------
   ;; Num_NonEq_Entries records the total number of non-equation 
   ;; entries that the student made while working on this problem.
   (Num_NonEq_Entries 0 Nil)

   ;; -------------------------------------------------------------
   ;; Num_Answer_Entries records the total number of answer entries
   ;; (correct or incorrect) that the student made while working on 
   ;; this problem.
   (Num_Answer_Entries 0 Nil)
   
   ;; --------------------------------------------------------------
   ;; Num_Algebra_calls counts the number of algebra calls that the
   ;; the student made.  This reflects how much use they made of
   ;; the calculate, and solve-for tools.
   (Num_Algebra_calls 0 Nil)
   
   ;; --------------------------------------------------------------
   ;; Num_Deletions reflects the number of deletions that the student
   ;; made while working on the problem.  
   (Num_Deletions 0 Nil)
   
   
   ;;; ==============================================================
   ;;; Time Tests
   ;;; The tests below accumulate the total time that the problem
   ;;; was open or some other value.  They will return a time in 
   ;;; seconds when used at runtime and should probably not be used
   ;;; to compute the total test value.
   
   ;; ---------------------------------------------------------------
   ;; Total_Open_Time records the total amount of time that the 
   ;; student had the problem open.
   (Total_Open_Time 0 Nil)
   
   ;; ---------------------------------------------------------------
   ;; Total_Non_Pause_Open_Time reflects the amount of time that the
   ;; student was "working" on the problem that is there was a pause
   ;; of less than **testing-pause-time-threshold** between entries.
   (Total_Non_Pause_Open_Time 0 Nil)
   
   ;; ---------------------------------------------------------------
   ;; Total_Pause_Open_Time reflects the amount of time that the 
   ;; student was "paused" when working that is, the total amount 
   ;; of time between entries where the time was greater than
   ;; **testing-Pause-Time-Threshold**.
   (Total_Pause_Open_Time 0 Nil)))



;;;; ===================================================================
;;;; Testing pause time threshold
;;;; This Value sets the amount of time between entries that we 
;;;; consider a "pause".  At present it is set at 3 minutes and is
;;;; used soley in the time tests listed above.  In order to change it
;;;; you should just replace the existing integer values with your
;;;; own choices, save the file and load Andes.

(setq **Testing-Pause-Time-Threshold** 
  (make-htime 
   ;; Change the integer values on the lines below. 
   :Sec 0
   :Min 3
   :Hour 0
   ))

; disable constraint loss filter
(setq **Filter-Constraint-losses** NIL)
