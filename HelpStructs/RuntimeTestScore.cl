#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RuntimeTestScore.cl
;; Collin Lynch
;; 10/3/2003
;;
;; This file contains the rt-Val, rt-score and rt-Solset classes and 
;; their individual subclasses.  The rt-Val class is an abstract 
;; superclass of the rt-score and rt-solset classes.  It defines 
;; several basic methods that should be used on all of the subsequent
;; classes.  
;;
;; The rt-score class defines individual rt-test-scores.  The root 
;; class here is also abstract and should be followed by several
;; individual classes.  The rt-solset class is used to combine
;; several individual solution values by set type for late use.
;;
;; This file first defines the abstract class and its methods.  
;; It then defines the rt-score class and its subclasses.  It 
;; then defines the rt-solset classes.
;;
;; All of the class sections also include utility functions that
;; can be used to process them.


;; The code in this section is used to define the runtime test result 
;; types.  The purpose of this typing is to make it easy for the runtime 
;; test code to deal with the different types of results.  This will make
;; it possible for the values to be stored and loaded seamlessly.  It
;; will also make it easy to hang specialized functions off of each type
;; so that they can be used at runtime.
;;
;; For the purpose of this section I have used CLOS to define the score
;; objects as it will be necessary to amend several different functions 
;; to account for each new type.  For that reason the rt-scores are 
;; defined as clos objects with their own methods.  The root class is
;; defined immediately below.  
;;
;; Each rt-score should contain the following methods:
;;;;;   rt-score-value <rt-score>: returns the raw value.
;;   get-rt-score-value <rt-score>: returns the raw value.
;;   set-rt-score-value <rt-score> Value: returns the raw value.
;;   rt-score->float <rt-score>:  
;;     Returns an appropriate floating point value for the score.
;;   rt-score->store-format <rt-score>:
;;     Returns a list form of the score that can be safely stored
;;     in the student.dat file and then be realoaded later.
;;   rt-score-update <rt-score> Value:
;;      Updates the value in score appropriately based upon the 
;;      Value argument and the type of the score.
;;   rt-score-load-stored-val <rt-score> StoredVal:
;;      Set's the specified rt-score to be the (appropriately)
;;      processed form of the StoredVal.  Depending upon the 
;;      type this may involve reading from strings or other 
;;      formatting changes or it may be the val itself.
;;   rt-score extern:  A mapping to a lisp type that can be mapped
;;      by the database code appropriately.  This code is used to 
;;      specify the external format for database storage.
;;   rt-score-dbform:  A mapping of an rt-val to an appropriate
;;      database form.  The type of this form should be the same
;;      as the external type.  
;;
;;;;   rt-score-stored-formp <rt-score> <form>:
;;;;     Return t iff this is an appropriate form for this score type.
;;;;   rt-score-load-stored-form <rt-score> <form>:  
;;;;     When called with an appropriate form this will generate and
;;;;     return a new instance of the specified class from the 
;;;;     loaded form.
;;     
|#

;;;; =================================================================
;;;; General utilities.

;;; --------------------------------------------
;;; Storage root.
;;; This root function is used as the basis of
;;; the storage formatting.  It should be called
;;; with a rt-score subclass object and an 
;;; appropriately formatted value.  It will then
;;; format the value appropriately for storage.
;;;
;;; The storage form is:
;;;   (rt-score <subclass-type> <Formatted-value>)
;;;
;;; This code makes use of the class-of and 
;;; class-name functions to ensure that even if
;;; a subclass is supplied to the formatting
;;; function it will be assigned the approrpiate
;;; name.  Doing it this way ensures that each
;;; new class need not create a new instance of
;;; the rt-score->store-format function.
;;;
;;; This format will be written out and read in
;;; using lisp's read function so the formatted
;;; value is assumed to have been formatted in 
;;; some way that is read-safe and can be reread
;;; using the rt-score-load-stored-val function.
(defun format-rt-val-for-storage (Obj value)
  (list 'rt-val (class-name (class-of Obj)) Value))




;;;; =================================================================
;;;; Abstract root classes
;;;; The abstract root class here is used to encapsualte the same
;;;; input-output methods for all of the subclasses.  This makes it 
;;;; feasible to read-write with less effort.


;;; -------------------------------------------------------------
;;; Class Definition.

(defclass rt-val ()
  ((Value :accessor rt-val-value :initarg :Value)))

;;; Given an rt-val generate a lisp writable and safely readable
;;; form of it that can be recognized by the reader function at 
;;; a later date.  
;;;
;;; The store format is a lisp-readable format of the class that
;;; can be safely stored-to/read-from the student.dat file or 
;;; other locations.  
;;;
;;; The stored-format should be generated using the 
(defmethod rt-val->store-format ((Val rt-val))
  "Format the runtime test score to a storage format."
  (format-rt-val-for-storage 
   Val (rt-val-value Val)))

;;; The rt-val<-stored-val is used to load a stored value 
;;; appropriately into the rt-val class.  It should be 
;;; called at runtime on an instance of the specified val
;;; type and with an appropriate StoredVal.  It will then
;;; load the data appropriately into the instance.
(defmethod rt-val<-stored-val ((Val rt-val) StoredVal)
  "Set the RT Score's stored value to be the supplied value."
  (setf (rt-val-value Val) StoredVal))


;;; ---------------------------------------------------------
;;; rt-val Utility functions.

;;; Type test
;;; Given an object return t iff it is an instance 
;;; of rt-val or a subclass of it.
;;;
;;; CLOS (unlike structs) does not define a <type>-p 
;;; function or other syntacitc assistants.  Therefore
;;; I have defined this function here.  It will return t
;;; if an rt-score or a subclass of it is supplied 
;;; as the argument.  

(defun rt-val-p (Obj)
  "Return t if obj is an rt-val or descendent."
  (typep Obj (find-class 'rt-val)))

;;; ---------------------------------------------------
;;; Interface funcs.
;;; General Storage Func.
;;; This function is a general one used to output rt-vals.
;;; This is here more for syntacic sugar than for anything
;;; else.  It calls the method function for storage and 
;;; returns the results.  

(defun pack-rt-val (Val)
  (rt-val->store-format Val))


;;; General Read func.
;;; This function is not just here for syntacic sugar.
;;; Given a lisp list of the form (rt-val <type> <Val>)
;;; this code will read it appropriately returning an
;;; instance of the specified class with the value loaded
;;;
;;; This function should be called on the entire list.

(defun unpack-rt-val (ValLst)
  (let ((Obj (make-instance (nth 1 ValLst))))
    (when (not (rt-val-p Obj))
      (error "Non rt-val obj passed to read-stored-rt-val"))
    (rt-val<-stored-val Obj (nth 2 ValLst))
    Obj))


;;; map-rt-val->float
;;; In order to give the students a score at runtime we need to map all 
;;; of the runtime-test-scores to a floating point number so that they
;;; can be used to formulate a weighted runtime score.  This function
;;; serves as a wrapper on the runtime test val types.  
;;;
;;; This code takes in an rt-val and an index argument.  It will then 
;;; test the code type.  If this is an rt-score then it will return 
;;; the result of calling rt-score->float.  If this is an rt-solset
;;; then it will return the results of calling rt-solset->float with
;;; the supplied index value.  If the Object is neither of these then
;;; it will throw an error.  
;;;
;;; The purpose of this wrapper function is to facilitate the easy
;;; calculation of score values without worrying about the immediate
;;; type.  It will be called by the code in runtime-test at runtime.
(defun map-rt-val->float (Val &optional (Index 0))
  "Map the runtime val supplied to a float."
  (cond
   ((rt-score-p Val) (rt-score->float Val))
   ((rt-solset-p Val) (rt-solset-score->float Val :Index Index))
   (t (error "ERROR:: Unrecognized type ~a supplied to map-rt-val->float"
	     (type-of Val)))))
      

;;; The "string" format of the val is used to display the 
;;; contents of the Val to the user.  This is done for 
;;; informational not computational purposes so the stored 
;;; format should be something that can be easily printed 
;;; by the andes2 Workbench.  The out form typically 
;;; appears on the printouts.
(defmethod map-rt-val->display-form ((Val rt-val) &optional (Index 0))
  "Return the rt-val in a \"displayable\" form."
  (cond
   ((rt-score-p Val) (rt-score->display-form Val))
   ((rt-solset-p Val) (rt-solset->display-form Val :Index Index))
   (t (error "ERROR:: Unrecognized type ~a supplied to map-rt-val->float"
	     (type-of Val)))))



;;;; External types.
;;;; Given a an rt-val classname map it to the appropriate external
;;;; type for storage.  The external type is specified by the class
;;;; itself and should be used for external storage.
;;;;
;;;; This works by instantiating the class and then calling the 
;;;; rt-val->externtype method.  This could probably be done 
;;;; better when I have the time.  

(defun map-rt-val-classname->externtype (Classname)
  "Get the external type of the class."
  (let ((inst (make-instance Classname)))
    (when inst
      (if (not (rt-val-p Inst))
	  (error "Non rt-val passed to map-rt-val-classname->externtype.")
	(if (rt-score-p Inst) 
	    (get-rt-score-externtype Inst)
	  (get-rt-solset-externtype Inst))))))



;;; Given an arbitrary tuntime test val map it to the 
;;; appropriate external type.  In this case the index,
;;; if necessary will be used.  



;;;; ======================================================
;;;; rt-score
;;;; The rt-score class is intended to represent single
;;;; score values.  IOt defines methods for setting and
;;;; retrieving the class values.  It also defines an 
;;;; update method that is used to handle the score value
;;;; in an appropriate way.  
;;;;
;;;; The general methods that are included are:
;;;;  (Inherited from rt-val)
;;;;   rt-val-Value:  Return the score's value.
;;;;   rt-val->store-format:  Format the value for storage.
;;;;   rt-val<-stored-val:  Load a stored val appropriately
;;;;     into an instance of this class.
;;;;
;;;;  (New)
;;;;   get-rt-score-value:  Returns the current value.
;;;;   set-rt-score-value:  Set the rt-score-value 
;;;;    woth some form of typechecking in the process.
;;;;   update-rt-score-value:  Given a value opdate the
;;;;    score's internal value in some way to represent it.
;;;;   rt-score->float:  Convert the score into a 
;;;;    floating point number in some way.
;;;;
;;;; The rt-score class also has an abstract subclass 
;;;; rt-sum-score that should be used for summable (numeric-ish)
;;;; values such as numbers and times.  It too is defined here.
;;;; In addition to all of the above it also defines the 
;;;; following methods:
;;;;  inc-rt-sum-score-value:  Increment the value by the supplied
;;;;   amount or some standard predefined amount.
;;;;  dec-rt-sum-score-value:  Decrement the value by some supplied
;;;;   amount or a prefedined standard value.  


;;; ------------------------------------------------------------------
;;; Root Class
;;; This class provides the basic shared methods that will be exteded
;;; by the other specialized classes below.  It should not be used 
;;; by itself.  It is only presnet to provide a model for other 
;;; classes.

;;; This function is used to generate new instances of the rt-score
;;; specifically.  It is not a method but parallels to it should
;;; be generated for the other subclasses below.  
(defun make-rt-score (Val)
  "Make a new runtime-test-score class with the specified val."
  (make-instance 'rt-score :Value Val))


(defclass rt-score (rt-val) ())

;;; Obtain the current score's value.  This is used 
;;; for retreival but not for setting any scores.
(defmethod get-rt-score-value ((Score rt-score))
  "Get the runtime test score value." 
  (rt-val-value Score))

;;; Set the specified scores value (destructively) to 
;;; be the supplied Val.
(defmethod set-rt-score-value ((Score rt-score) Val)
  "Set the runtime test score value."
  (setf (rt-val-value Score) Val))

;;; Given a function that takes the rt-score-val as an
;;; argument and returns a value of the appropriate type
;;; this method will funcall the function with the 
;;; current value as an argument and set the value to
;;; be the result.
(defmethod update-rt-score-value ((Score rt-score) Func)
  "Update the score in some set way."
  (setf (rt-val-value Score) 
    (funcall Func (rt-val-Value Score))))


;;; Translate the supplied value into a floating point number.
;;; The purpose of this function is to translate the value for
;;; use in computing the student's runtime score.  
;;; 
;;; The supplied index argument is ignored here.  it exists so
;;; that the later solset score can be converted appropriately.
(defmethod rt-score->float ((Score rt-score))
  "Translate the current value to a floating point Num. (Default)"
  9999999999)


;;; The "display" format of the score is used to display the 
;;; contents of the score to the user.  This is done for 
;;; informational not computational purposes so the stored 
;;; format should be something that can be easily printed 
;;; by the andes2 Workbench.  The out form typically 
;;; appears on the printouts.
(defmethod rt-score->display-form ((Val rt-score))
  "Return the rt-score in a \"displayable\" form."
  (format Nil "~a" (rt-val-value Val)))



;;; External types 
;;; Obtain the external type of the rt-score and map it to that.

(defmethod get-rt-score-externtype ((Score rt-score))
  "Get the external type of the score."
  'number)


;;; Map the rt-score to the appropriate external type.
(defmethod rt-score->externtype ((Score rt-score))
  "Map the score to the external type."
  999999)





;;; rt-score Type test
;;; CLOS (unlike structs) does not define a <type>-p 
;;; function or other syntacitc assistants.  Therefore
;;; I have defined this function here.  It will return t
;;; if an rt-score or a subclass of it is supplied 
;;; as the argument.  

(defun rt-score-p (Obj)
  "Return t if obj is an rt-score."
  (typep Obj (find-class 'rt-score)))








;;; ---------------------------------------------------------------
;;; sum class.
;;; The rt-sum-score class reflects a score that can be updated
;;; in some incrementing and decrementing way.  In addition to 
;;; the standard functions supported by rt-score classes it also
;;; supports the two increment and decrement functions.  
;;;
;;; This class should not be instantiated directly.  I have only
;;; included it here as a model.  
;;;
;;; This does not define its own externtype info.

;;; This function is used to generate new instances of the 
;;; rt-sum-score class specifically.  It is not a method 
;;; but parallels to it should be generated for the other 
;;; subclasses below.  
(defun make-rt-sum-score (Val)
  "Make a new rt-sum-score class with the specified val."
  (make-instance 'rt-sum-score :Value (list Val)))


(defclass rt-sum-score (rt-score) ())

;;;  inc-rt-sum-score:  "increments" the value by some default
;;;    amount or the supplied argument.  For this abstract class
;;;    the value will be incremented by Nil leaving it as-is.
(defmethod inc-rt-sum-score-value ((Score rt-sum-score) &optional (Amt 0))
  "Increment the score by the supplied value or the default."
  (setf (rt-val-value Score) 
    (list (rt-val-value Score) Amt)))

;;;  rt-sum-score-dec:  "decrements" the value by some default
;;;    amount or the optional argument.  
(defmethod dec-rt-sum-score-value ((Score rt-sum-score) &optional (Amt 0))
  "Decrement the score's current value."
  (declare (ignore Amt))
  (setf (rt-val-value Score) 
    (car (rt-val-value Score))))


;;; rt-sum-score Type test
;;; CLOS (unlike structs) does not define a <type>-p 
;;; function or other syntacitc assistants.  Therefore
;;; I have defined this function here.  It will return t
;;; if an rt-score or a subclass of it is supplied 
;;; as the argument.  

(defun rt-sum-score-p (Obj)
  "Return t if obj is an rt-score."
  (typep Obj (find-class 'rt-sum-score)))





;;; ==================================================================
;;; Numeric score types.
;;; The classes in this section are designed to store and maintain
;;; numeric values.  The root classes are used to support numeric
;;; tests and will be used at runtime for those purposes.

;;; ------------------------------------------------------------------
;;; runtime-test-num-score
;;; This runtime test score class is used to handle numeric values.
;;; The update function will replace the current value with the 
;;; newly updated value.  
;;;
;;; Because the initial runtime-score type has a numeric external type
;;; this one will not define its own form.

(defun make-rt-num-score (Val)
  "Make a new runtime-test-score class with the specified val."
  (if (not (numberp Val))
      (error "Non # supplied to rt-int-score constructor: ~a" Val)
    (make-instance 'rt-num-score :Value Val)))


;; statt here ensuring appropriate values returned.

(defclass rt-num-score (rt-score) ())

(defmethod set-rt-score-value ((Score rt-num-score) Val)
  "Set the runtime test score value."
  (if (not (numberp Val))
      (error "Non # supplied to rt-num-score setfunc: ~a" Val)
    (setf (rt-val-value Score) Val)))

(defmethod rt-score->float ((Score rt-num-score))
  "Translate the current value to a floating point Num. (Default)"
  (rt-val-value Score))

(defmethod rt-val<-stored-val ((Score rt-num-score) StoredVal)
  "Set the RT Score's stored value to be the supplied value."
  (if (not (numberp StoredVal))
      (error "Non num supplied to rt-num-score setfunc: ~a" StoredVal)
    (setf (rt-val-value Score) StoredVal)))





;;; -------------------------------------------------------------------
;;; runtime-test-sum-num-score 
;;; This runtime test class is used to keep a count of a numeric value
;;; it implements the inc and dec functions and should be used for 
;;; any value that needs to be treated as a counter.

(defun make-rt-num-sum-score (Val)
  "Make a new runtime-test-score class with the specified val."
  (if (not (numberp Val))
      (error "Non # supplied to rt-int-score constructor: ~a" Val)
    (make-instance 'rt-num-sum-score :Value Val)))


(defclass rt-num-sum-score (rt-num-score) ())

;;; Increment the current value by the default amount
;;; or the argument value (if specified).
(defmethod inc-rt-sum-score-value ((Score rt-num-sum-score) &optional (Amt 1))
  "Increment the number by the specified val."
  (if (not (numberp Amt))
      (error "Non-Numeric value supplied to rt-num-sum-score-inc.")
    (setf (rt-val-value Score) (+ (rt-val-value Score) Amt))))


;;; Increment the current value by the default amount
;;; or the argument value (if specified).
(defmethod dec-rt-sum-score-value ((Score rt-num-sum-score) &optional (Amt 1))
  "Decrement the number by the specified val."
  (if (not (numberp Amt))
      (error "Non-Numeric value supplied to rt-num-sum-score-inc.")
    (setf (rt-val-value Score) (- (rt-val-value Score) Amt))))







;;; ------------------------------------------------------------------
;;; runtime-test-int-score
;;; This runtime test score is used to score integer values.  Here 
;;; the update function will replace the current value with the 
;;; newly supplied value.  This should be used for storing a number
;;; of visible components or other values.  Counts can be better 
;;; maintained by the rt-int-sum-score class below. 

(defun make-rt-int-score (Val)
  "Make a new runtime-test-score class with the specified val."
  (if (not (integerp Val))
      (error "Non Int supplied to rt-int-score constructor: ~a" Val)
    (make-instance 'rt-int-score :Value Val)))


(defclass rt-int-score (rt-num-score) ())

(defmethod set-rt-score-value ((Score rt-int-score) Val)
  "Set the runtime test score value."
  (if (not (integerp Val))
      (error "Non Int supplied to rt-int-score setfunc: ~a" Val)
    (setf (rt-val-value Score) Val)))

(defmethod update-rt-score-value ((Score rt-int-score) Func)
  "Update the score by setting it to the func's result."
  (let ((NewVal (funcall Func (rt-val-value Score))))
    (if (not (integerp NewVal))
	(error "Non Int supplied to rt-int-score setfunc: ~a" NewVal)
      (setf (rt-val-value Score) NewVal))))

(defmethod rt-val<-stored-val ((Score rt-int-score) StoredVal)
  "Set the RT Score's stored value to be the supplied value."
  (if (not (integerp StoredVal))
      (error "Non Int supplied to rt-int-score setfunc: ~a" StoredVal)
    (setf (rt-val-value Score) StoredVal)))


(defmethod get-rt-score-externtype ((Score rt-int-score))
  "Get the external type of the rt-score."
  'integer)


(defmethod rt-score->externtype ((Score rt-int-score))
  "Map to an external form."
  (rt-val-value Score))



;;; ------------------------------------------------------------------
;;; runtime-test-int-sum-score
;;; This class also stores a runtime sum but it maintains a runtime
;;; sum of the value.  When the update function is called it will
;;; add the newly supplied value to the current value.  
;;;
;;; This can be best used for keeping track of counts and other values.
;;; This class has an additional pair of functions inc and dec to deal
;;; with that task.

(defun make-rt-int-sum-score (&optional (Val 0))
  "Make a new runtime-test-score class with the specified val."
  (if (not (integerp Val))
      (error "Non Int supplied to rt-int-sum-score constructor: ~a" Val)
    (make-instance 'rt-int-sum-score :Value Val)))


(defclass rt-int-sum-score (rt-int-score rt-num-sum-score) ())

(defmethod inc-rt-sum-score-value ((Score rt-int-sum-score) &optional (Amt 1))
  "Increment the rt-score-value."
  (if (not (integerp Amt))
      (error "Non-Integer value supplied to rt-int-sum-score-inc.")
    (setf (rt-val-value Score)
      (+ (rt-val-value Score) Amt))))

(defmethod dec-rt-sum-score ((Score rt-int-sum-score) &optional (Amt 1))
  "Increment the rt-score-value."
  (if (not (integerp Amt))
      (error "Non-Integer value supplied to rt-int-sum-score-inc.")
   (setf (rt-val-value Score)
     (- (rt-val-value Score) Amt))))





;;; ------------------------------------------------------------------
;;; runtime-test-float-score
;;; This runtime-test-object is used to handle floating-point values.
;;; At runtime it will be used to handle any non-integer numeric
;;; values.  The update function for this class will replace the
;;; current value with the next value.

(defun make-rt-float-score (Val)
  "Make a new runtime-test-score class with the specified val."
  (if (not (floatp Val))
      (error "Non float supplied to rt-float-score constructor: ~a" Val)
    (make-instance 'rt-float-score :Value Val)))



(defclass rt-float-score (rt-num-score) ())

(defmethod set-rt-score-value ((Score rt-float-score) Val)
  "Set the runtime test score value."
  (if (not (floatp Val))
      (error "Non float supplied to rt-float-score setfunc: ~a" Val)
    (setf (rt-val-value Score) Val)))

(defmethod update-rt-score-value ((Score rt-float-score) Func)
  "Update the score by setting NewValue to it."
  (let ((NewVal (funcall Func (rt-val-value Score))))
    (if (not (floatp NewVal))
	(error "Non Float supplied to rt-float-score setfunc: ~a" NewVal)
      (setf (rt-val-value Score) NewVal))))

(defmethod rt-val<-stored-val ((Score rt-float-score) StoredVal)
  "Set the RT Score's stored value to be the supplied value."
  (if (not (floatp StoredVal))
      (error "Non Int supplied to rt-float-score setfunc: ~a" StoredVal)
    (setf (rt-val-value Score) StoredVal)))


(defmethod get-rt-score-externtype ((Score rt-float-score))
  "Get the external type of the score."
  'float)

(defmethod rt-score->externtype ((Score rt-float-score))
  "Map the score to the external type."
  (rt-val-value Score))



;;; ------------------------------------------------------------------
;;; runtime-test-float-sum-score
;;; Floating point sum socre.  This class should be used to track
;;; floating point units that are updated by addition.  

(defun make-rt-float-sum-score (Val)
  "Make a new runtime-test-score class with the specified val."
  (if (not (floatp Val))
      (error "Non float supplied to rt-float-sum-score constructor: ~a" Val)
    (make-instance 'rt-float-sum-score :Value Val)))


(defclass rt-float-sum-score (rt-float-score rt-num-sum-score) ())

(defmethod inc-rt-sum-score-value ((Score rt-float-sum-score) &optional (Amt 1.0))
  "Increment the rt-score-value."
  (if (not (floatp Amt))
      (error "Non-Float value supplied to rt-float-sum-score-inc.")
    (setf (rt-val-value Score)
      (+ (rt-val-value Score) Amt))))

(defmethod dec-rt-sum-score-value ((Score rt-float-sum-score) &optional (Amt 1.0))
  "Increment the rt-score-value."
  (if (not (floatp Amt))
      (error "Non-Float value supplied to rt-float-sum-score-inc.")
   (setf (rt-val-value Score)
     (- (rt-val-value Score) Amt))))
 






;;;; ==================================================================
;;;; Htime scores.
;;;; The score classes in this section are used to handle Htime
;;;; results and should be loaded/used at runtime to keep track of
;;;; the running time or store set time values.

;;; ------------------------------------------------------------------
;;; runtime-test-htime-score
;;; This runtime test score is used to track an htime value.  

(defun make-rt-htime-score (Val)
  "Make a new runtime-test-score class with the specified val."
  (if (not (htime-p Val))
      (error "Non htime supplied to rt-htime-score constructor: ~a" Val)
    (make-instance 'rt-htime-score :Value (copy-striucture Val))))



(defclass rt-htime-score (rt-score) ())

(defmethod set-rt-score-value ((Score rt-htime-score) Val)
  "Set the runtime test score value."
  (if (not (htime-p Val))
      (error "Non htime supplied to rt-htime-score constructor: ~a" Val)
    (setf (rt-val-value Score) (copy-structure Val))))

(defmethod update-rt-score-value ((Score rt-htime-score) Func)
  "Update the score by adding NewValue to it."
  (let ((Val (funcall Func (rt-val-value Score))))
    (if (not (htime-p Val))
	(error "Non htime supplied to rt-htime-score constructor: ~a" Val)
      (setf (rt-val-value Score) (copy-structure Val)))))

(defmethod rt-score->float ((Score rt-htime-score))
  "Translate the current value to a floating point Num. (Default)"
  (convert-htime->secs (rt-val-value Score)))



(defmethod rt-val->store-format ((Score rt-htime-score))
  "Format the runtime test score to a storage format."
  (format-rt-val-for-storage
   Score (format Nil "~a" (rt-val-value Score))))

(defmethod rt-val<-stored-val ((Score rt-htime-score) StoredVal)
  "Set the RT Score's stored value to be the supplied value."
  (if (not (stringp StoredVal))
      (error "Non string supplied to rt-htime-score setfunc: ~a" StoredVal)
    (setf (rt-val-value Score) (parse-htime (copy-seq StoredVal)))))


(defmethod get-rt-score-externtype ((Score rt-Htime-score))
  "Get the lisp external type of the score."
  'htime)

(defmethod rt-score->externtype ((Score rt-htime-score))
  "Convert the score to an external type."
  (rt-val-value Score))



;;; ------------------------------------------------------------------
;;; runtime-test-htime-sum-score
;;; This runtime test score is used to track an htime value.  

(defun make-rt-htime-sum-score (&optional (Val (make-htime)))
  "Make a new runtime-test-score class with the specified val."
  (if (not (htime-p Val))
      (error "Non htime supplied to rt-htime-score constructor: ~a" Val)
    (make-instance 'rt-htime-sum-score :Value (copy-structure Val))))


(defclass rt-htime-sum-score (rt-htime-score rt-sum-score) ())

(defmethod inc-rt-sum-score-value ((Score rt-htime-sum-score) 
				   &optional (Val (make-htime :sec 1)))
  "Increment the htime score val."
  (if (not (htime-p Val))
      (error "Non htime supplied to rt-htime-score inc: ~a" Val)
    (setf (rt-val-value Score)
      (add-htimes (rt-val-value Score) Val))))

(defmethod dec-rt-sum-score-value ((Score rt-htime-sum-score) 
				   &optional (Val (make-htime :sec 1)))
  "Decrement the htime score val."
  (if (not (htime-p Val))
      (error "Non htime supplied to rt-htime-score dec: ~a" Val)
    (setf (rt-val-value Score)
      (sub-htimes (rt-val-value Score) Val))))






;;;; ==================================================================
;;;; Fract type
;;;; The runtime test fract type is used to handle ratios of values.
;;;; Its value will be a list of two numbers.  The summation form(s)
;;;; will be incrementing and decrementing the numerator only.  The
;;;; update will be explicitly setting the Numerator.

;;; Utility function for determing whether or not a 
;;; given item is an appropriate fract list.
(defun fract-list-p (Lst)
  (and (listp Lst) (= 2 (length Lst))
       (numberp (car Lst)) (numberp (cadr Lst))))




;;; ------------------------------------------------------------------
;;; runtime-test-Fract
;;; A fractional score that represents a rate or ratio.  The update
;;; function affects only the numerator in the list.  Mapping it to
;;; a floating point value will return a safe division of the numerator
;;; by the denominator.

(defun make-rt-fract-score (Num Denom)
  "Make a new runtime-test-score class with the specified val."
  (if (not (and (numberp Num) (numberp Denom)))
      (error "Non fract list supplied to rt-fract-score constructor: ~a ~a" Num Denom)
    (make-instance 'rt-fract-score :Value (list Num Denom))))


(defclass rt-fract-score (rt-score) ())

(defmethod set-rt-score-value ((Score rt-fract-score) Val)
  "Set the runtime test score value."
  (if (not (fract-list-p Val))
      (error "Non fract-list supplied to rt-fract-score set-value: ~a" Val)
    (setf (rt-val-value Score) (copy-tree Val))))


(defmethod update-rt-score-value ((Score rt-fract-score) Func)
  "Set the runtime test score value."
  (let ((Val (funcall Func (rt-val-value Score))))
    (if (not (fract-list-p Val))
	(error "Non number supplied to rt-fract-score update-value: ~a" Val)
      (setf (rt-val-value Score) (copy-tree Val)))))

;; May be resurrected as necessary.
;;; This update function does not take in a static value.  Rather it
;;; taken in a test function that will be executed with the current
;;; value as an argument and which is assumed to return a single number.
;;; the numerator of the result will be updated to that value (iff
;;; it is less than or equal to the denominator).
;;(defmethod rt-score-update ((Score rt-fract-score) Test)
;;  "Update the score by funcalling test and storing the result."
;;  (setf (nth 0 (rt-score-value Score))
;;    (let ((Result (funcall Test (rt-score-value Score))))
;;      (when (> Result (second (rt-score-value Score)))
;;	(error "Fractional result greater than denominator."))
;;      Result)))


;;; Perform a save divide returning 0 if either the numerator or
;;; denominator is 0 or returning the floating point val otherwize.
(defmethod rt-score->float ((Score rt-fract-score))
  "Translate the current value to a floating point Num. (Default)"
  (coerce 
   (let ((Val (rt-val-value score)))
     (if (or (= 0 (first Val)) (= 0 (second Val))) 0
       (/ (first Val) (second Val))))
   'float))
  

;;; Load in the stored value.
(defmethod rt-val<-stored-val ((Score rt-fract-score) StoredVal)
  "Set the RT Score's stored value to be the supplied value."
  (if (not (fract-list-p StoredVal))
      (error "Non fract-list supplied to rt-fract-score setfunc: ~a" StoredVal)
    (setf (rt-val-value Score) (copy-tree StoredVal))))


(defmethod rt-score->display-form ((Score rt-fract-score))
  "Return a display form of the val."
  (format Nil "~a/~a" 
	  (nth 0 (rt-val-value Score))
	  (nth 1 (rt-val-value Score))))



(defmethod get-rt-score-externtype ((Score rt-fract-score))
  "Get the external type of the rt-score."
  'float)


;;; Is this appropriate?
(defmethod rt-score->externtype ((Score rt-fract-score))
  "Map the score to the appropriate external type."
  (coerce (rt-score->float Score) 'float))





;;; ------------------------------------------------------------------
;;; runtime-test-Fract-sum
;;; A fractional score that represents a rate or ratio.  The update
;;; function affects only the numerator in the list.  Mapping it to
;;; a floating point value will return a safe division of the numerator
;;; by the denominator.
;;;
;;; Here the increment and decrement functions will update the numerator
;;; and denominator by adding each of the supplied integers to them or 
;;; subtracting each of the supplied integers from them.  The values should
;;; be supplied in a list of length 2.


(defun make-rt-fract-sum-score (Num Denom)
  "Make a new runtime-test-score class with the specified val."
  (if (not (and (numberp Num) (numberp Denom)))
      (error "Non fract list supplied to rt-fract-score constructor: ~a ~a" Num Denom)
    (make-instance 'rt-fract-sum-score :Value (list Num Denom))))


(defclass rt-fract-sum-score (rt-fract-score rt-sum-score) ())

(defmethod inc-rt-sum-score-value ((Score rt-fract-sum-score) &optional (Val '(1 1)))
  "Increment the htime score val."
  (when (not (fract-list-p Val))
    (error "Non num supplied to inc-rt-htime-sum-score-value: ~a" Val))
  (setf (nth 0 (rt-val-value Score))
    (+ (nth 0 (rt-val-value Score)) (nth 0 Val)))
  (setf (nth 1 (rt-val-value Score))
    (+ (nth 1 (rt-val-value Score)) (nth 1 Val))))

(defmethod dec-rt-sum-score-value ((Score rt-fract-sum-score) &optional (Val '(1 1)))
  "Decrement the htime score val."
  (when (not (fract-list-p Val))
    (error "Non num supplied to dec-rt-htime-sum-score-value: ~a" Val))
  (setf (nth 0 (rt-val-value Score))
    (- (nth 0 (rt-val-value Score)) (nth 0 Val)))
  (setf (nth 1 (rt-val-value Score))
    (- (nth 1 (rt-val-value Score)) (nth 1 Val))))







;;; ------------------------------------------------------------------
;;; Fract1 score
;;; The fract1 score operates like the fract score save that, when it 
;;; is mapped to a floating point number the value is 1 if the result
;;; of the division is 0.  This is a dummy value for score reflection.
;;;
;;; Most of the methods for this class are defined above.

(defun make-rt-fract1-score (Num Denom)
  "Make a new runtime-test-score class with the specified val."
  (if (not (and (numberp Num) (numberp Denom)))
      (error "Non fract list supplied to rt-fract-score constructor: ~a ~a" Num Denom)
    (make-instance 'rt-fract1-score :Value (list Num Denom))))


(defclass rt-fract1-score (rt-fract-score) ())


;;; Perform a save divide returning 1 if the denominator and numerator
;;; are zero or the result of numerator/denominator otherwize.
(defmethod rt-score->float ((Score rt-fract1-score))
  "Translate the current value to a floating point Num. (Default)"
  (coerce 
   (let ((Val (rt-val-value score)))
     (cond ((and (= 0 (first Val)) (= 0 (second Val))) 1)
	   ((or (= 0 (first Val)) (= 0 (second Val))) 0)
	   (t (/ (first Val) (second Val)))))
   'float))


;;; Return a display val but with the caveat that, if the denominator
;;; of the fract is 0 then the result value will be 1/1 not n/0
(defmethod rt-score->display-form ((Score rt-fract1-score))
  "Return a display form of the val."
  (if (= 0 (nth 1 (rt-val-value Score))) "1/1"
    (format Nil "~a/~a" 
	    (nth 0 (rt-val-value Score))
	    (nth 1 (rt-val-value Score)))))





;;; ----------------------------------------------------------
;;; Fract1-sum-score 
;;; The fract1 sum score behaves like the fract-sum-score
;;; save for the change in behavior when it is mapped to 
;;; a floating point number.  Therefore it inherits directly
;;; from rt-fract1-score and rt-fract-sum-score.

(defun make-rt-fract1-sum-score (Num Denom)
  "Make a new runtime-test-score class with the specified val."
  (if (not (and (numberp Num) (numberp Denom)))
      (error "Non fract list supplied to rt-fract-score constructor: ~a ~a" Num Denom)
    (make-instance 'rt-fract1-sum-score :Value (list Num Denom))))

(defclass rt-fract1-sum-score (rt-fract1-score rt-fract-sum-score) ())





;;; ------------------------------------------------------------------
;;; fract-num class.
;;; The standard runtime test fract and fract1 classes define the 
;;; the update increment and decrement functions s.t. the user is 
;;; updating both the numerator and the denominator.  This code
;;; defines a fract class that can be used to update only the 
;;; numerator (once the initial denominator is set).  The purpose
;;; of this is to facilitate ratios of entries that do not need 
;;; to have both numerator and denominator updated at runtime.

(defun make-rt-fract-num-score (Num Denom)
  "Make a new runtime-test-score class with the specified val."
  (if (not (and (numberp Num) (numberp Denom)))
      (error "Non fract list supplied to rt-fract-score constructor: ~a ~a" Num Denom)
    (make-instance 'rt-fract-num-score :Value (list Num Denom))))


(defclass rt-fract-num-score (rt-fract-score) ())

;;; Update the numerator in the score. Leave the denominator untouched.  
(defmethod update-rt-score-value ((Score rt-fract-num-score) Func)
  "Set the runtime test score's numerator value.."
  (let ((Val (funcall Func (nth 0 (rt-val-value Score)))))
    (if (not (numberp Val))
	(error "Non number supplied to rt-fract-score update-value: ~a" Val)
      (setf (nth 0 (rt-val-value Score)) Val))))





;;; ------------------------------------------------------------
;;; rt-fract-num-sum score
;;; The rt-fract num sum score operates like the fract sum score
;;; save that it updates soley the numerator on the inc and dec
;;; functions.
;;;
;;; The Increment and decrement functions will update the numerator but 
;;; not the denominator.  If it proves necessary at a later date the 
;;; inc function may be set to throw an error if someone attempts to 
;;; set the numerator greater than the denominator.

(defun make-rt-fract-num-sum-score (Num Denom)
  "Make a new runtime-test-score class with the specified val."
  (if (not (and (numberp Num) (numberp Denom)))
      (error "Non fract list supplied to rt-fract-score constructor: ~a ~a" Num Denom)
    (make-instance 'rt-fract-num-sum-score :Value (list Num Denom))))


(defclass rt-fract-num-sum-score (rt-fract-num-score rt-fract-sum-score) ())

(defmethod inc-rt-sum-score-value ((Score rt-fract-num-sum-score) &optional (Val 1))
  "Increment the htime score val."
  (if (not (numberp Val))
      (error "Non num supplied to inc-rt-htime-sum-score-value: ~a" Val)
    (setf (nth 0 (rt-val-value Score))
      (+ (nth 0 (rt-val-value Score)) Val))))

(defmethod dec-rt-sum-score-value ((Score rt-fract-num-sum-score) &optional (Val 1))
  "Decrement the htime score val."
  (if (not (numberp Val))
      (error "Non num supplied to dec-rt-htime-sum-score-value: ~a" Val)
    (setf (nth 0 (rt-val-value Score))
      (- (nth 0 (rt-val-value Score)) Val))))






;;; ------------------------------------------------------------------
;;; fract1-num class.
;;; The standard runtime test fract and fract1 classes define the 
;;; the update increment and decrement functions s.t. the user is 
;;; updating both the numerator and the denominator.  This code
;;; defines a fract class that can be used to update only the 
;;; numerator (once the initial denominator is set).  The purpose
;;; of this is to facilitate ratios of entries that do not need 
;;; to have both numerator and denominator updated at runtime.

(defun make-rt-fract1-num-score (Num Denom)
  "Make a new runtime-test-score class with the specified val."
  (if (not (and (numberp Num) (numberp Denom)))
      (error "Non fract list supplied to rt-fract-score constructor: ~a ~a" Num Denom)
    (make-instance 'rt-fract1-num-score :Value (list Num Denom))))


(defclass rt-fract1-num-score (rt-fract1-score) ())

;;; Update the numerator in the score. Leave the denominator untouched.  
(defmethod update-rt-score-value ((Score rt-fract1-num-score) Func)
  "Set the runtime test score's numerator value.."
  (let ((Val (funcall Func (rt-val-value Score))))
    (if (not (numberp Val))
	(error "Non number supplied to rt-fract-score update-value: ~a" Val)
      (setf (nth 0 (rt-val-value Score)) Val))))





;;; ------------------------------------------------------------
;;; rt-fract-num-sum score
;;; The rt-fract num sum score operates like the fract sum score
;;; save that it updates soley the numerator on the inc and dec
;;; functions.
;;;
;;; The Increment and decrement functions will update the numerator but 
;;; not the denominator.  If it proves necessary at a later date the 
;;; inc function may be set to throw an error if someone attempts to 
;;; set the numerator greater than the denominator.

(defun make-rt-fract1-num-sum-score (Num Denom)
  "Make a new runtime-test-score class with the specified val."
  (if (not (and (numberp Num) (numberp Denom)))
      (error "Non fract list supplied to rt-fract-score constructor: ~a ~a" Num Denom)
    (make-instance 'rt-fract1-num-sum-score :Value (list Num Denom))))


(defclass rt-fract1-num-sum-score (rt-fract1-num-score rt-fract-sum-score) ())

(defmethod inc-rt-sum-score-value ((Score rt-fract1-num-sum-score) &optional (Val 1))
  "Increment the htime score val."
  (if (not (numberp Val))
      (error "Non num supplied to inc-rt-htime-sum-score-value: ~a" Val)
    (setf (nth 0 (rt-val-value Score))
      (+ (nth 0 (rt-val-value Score)) Val))))

(defmethod dec-rt-sum-score-value ((Score rt-fract1-num-sum-score) &optional (Val 1))
  "Decrement the htime score val."
  (if (not (numberp Val))
      (error "Non num supplied to dec-rt-htime-sum-score-value: ~a" Val)
    (setf (nth 0 (rt-val-value Score))
      (- (nth 0 (rt-val-value Score)) Val))))




;;;; ==================================================================
;;;; Solution Sets.
;;;; The Solution sets are used to manage several parallel score values.
;;;; They share storage and retrival code with the rt-score values and
;;;; are therefore descended from the rt-val class.  However they do not
;;;; make use of the same accessors as the rt-score functions and so they
;;;; will be accessed differently at runtime.  The code in this section
;;;; will define access to them at runtime.  
;;;;
;;;; The general solset type can be used to store any rt-score types.  
;;;; the specialized types are used to provide rt-score specific set 
;;;; types for later use.  These are used so that the type of the set
;;;; can be set a-priori.  
;;;;
;;;; These types do not differ over sum sets versus non-sum-sets what 
;;;; they track is the fundamental types of the values be they store
;;;; therefore the solsets are: num, int, float, htime, and fract.
;;;;
;;;; At present I have only defined the specialized type for fract
;;;; solsets so that the scores will be maintained by that.  The others
;;;; can be added later to the heirarchy if they prove necessary.  

;;;; --------------------------------------------------------------------
;;;; Util funcs.

;;;; The solution sets result type store several different runtime-test
;;;; scores (of the same type) one for each solution set.  It's value 
;;;; is therefore a list of types.  
;;;;
;;;; in addition to the classic functions below there is also an iterator
;;;; that will evaluate a given function for each element in the list 
;;;; defined by an index.  Look to the bottom for its definition.

;;; Return t if the supplied list is a list of runtime tests
;;; all of the same type. Return nil otherwize.  
(defun rt-solset-Set-p (Set)
  "Return ti if this is a list of rt-scores of the same type."
  (and Set (rt-score-p (car Set))
       (set-type-testp (type-of (car Set)) (cdr Set))))


;;; Return t if the elements in the set 
;;; are not of the specified type.
(defun set-type-testp (Type Set)
  "Return t iff the elements of test are of the same type or nil."
  (cond ((null Set) t)
	((= 1 (length Set)) (equalp Type (type-of (car Set))))
	(t (and (equalp Type (type-of (car Set)))
		(set-type-testp Type (cdr Set))))))


;;; Test to see if this object is a Solution Set.
(defun rt-solset-p (Obj)
  "Return t if obj is an rt-score."
  (typep Obj (find-class 'rt-solset)))


;;;; -------------------------------------------------------------------
;;;; SolSet Class.

;;; This takes in a valis rt-solset-Set and produces
;;; a rt-solset-score struct that can then be processed
;;; at runtime.
(defun make-rt-solset (Set)
  "Make the runtime test solset score."
  (if (not (rt-solset-set-p Set))
      (error "Non Solset supplied to make-rt-solset-score.")
    (make-instance 'rt-solset :Value Set)))


(defclass rt-solset (rt-val) ())


(defmethod get-rt-solset-size ((Set rt-solset))
  "Get the size of the rt-solset."
  (length (rt-val-value Set)))

(defmethod set-rt-solset-value ((Score rt-solset) Set)
  "Set the rt-score value to be the supplied values."
  (if (not (rt-solset-set-p Set))
      (error "Non Solset supplied to make-rt-solset.")
    (setf (rt-val-value Score) Set)))

;;; Update the specified rt-score in the rt-solset with
;;; the supplied value by recursively calling the update
;;; function.  In this case Func should be a function
;;; suitable for use in an update.
(defmethod update-rt-solset-val ((S rt-solset) &key (Index 0) (Func #'identity))
  "Update the specified Solution in the index."
  (update-rt-score-value (nth Index (rt-val-value S)) Func))



;;; The map-update-rt-solset-function takes in a function suitable
;;; for use in an rt-score update.  It then recursively calls the 
;;; update procedure with that function on each item in the set.
(defmethod update-rt-solset-map ((S rt-solset) Func)
  (setf (rt-val-value S)
    (mapcar Func (rt-val-value S))))


;;; Identify the type of the rt-scores that are presnt 
;;; in this solset.  
(defmethod get-rt-solset-score-type ((S rt-solset))
  (type-of (car (rt-val-value S))))


;;; Converting the solset score to a floating point number
;;; necessittes knowing which of the individual scores 
;;; need to be converted and then recursively doing so.
;;; Therefore, the optinal index argument if supplied 
;;; will be used to specify which of the individual 
;;; elements to return.  
(defmethod rt-solset-score->float ((S rt-solset) &key (Index 0))
  "Recursively convert the score elt to a float."
  (rt-score->float (nth Index (rt-val-value S))))



;;; Generating an appropriate store format for the scores is
;;; also a recursive process.  The resulting format is a 
;;; list of store-formats for each of the input scores.
(defmethod rt-val->store-format ((Score rt-solset))
  "Format the SolSet score values."
  (format-rt-val-for-storage
   Score (mapcar #'rt-val->store-format (rt-val-value Score))))

;;; Taking in the stiored val of an rt-solset is a recurisive 
;;; process. This code will iterate over the list of stored
;;; values and call unpack-rt-val on each one to generate 
;;; the val before setting that value to the current 
;;; rt-val-value.
(defmethod rt-val<-stored-val ((Val rt-solset) StoredVal)
  (setf (rt-val-value Val)
    (mapcar #'unpack-rt-val StoredVal)))


;;; The "display" format of the val is used to display the 
;;; contents of a single score within the solset to the user.
;;; This is used by Andes to generate printouts or to provide 
;;; other displays.  This code will recursively generate the
;;; score display form of the solset by calling the function
;;; on the rt-score within its storage specified by the index.
(defmethod rt-solset->display-form ((Set rt-val) &key (Index 0))
  "Return the rt-val in a \"displayable\" form."
  (rt-score->display-form (nth Index (rt-val-value Set))))



;;; The External types for solsets vary.  Ideally it should be the 
;;; externtype of the code within it.  The purpose of this is to
;;; store the values themselves on a row-by-row basis.  If it becomes
;;; necessary I may change this so that the values are stored as 
;;; a list but database processing makes this more efficient in 
;;; this way.
;;;
;;; If the set value is nil then this will return the dummy value of 
;;; 'string.
(defmethod get-rt-solset-externtype ((Set rt-solset))
  "Get the rt-solset external type."
  (if (not (slot-boundp Set 'value)) 'string
    (get-rt-score-externtype (car (rt-val-value Set)))))


;;; When mapping a solset to an external type we don't want to 
;;; encapsulate the entire solset value but rather the particular
;;; index value that was supplied.  This code will therefore 
;;; return the externtype of the indexed rt-score.
(defmethod rt-solset->externtype ((Set rt-solset) &key (Index 0))
  "Get the externtype of the indexed rt-score."
  (rt-score->externtype (nth Index (rt-val-value Set))))


;;;; ------------------------------------------------------------------
;;;; rt-fract-solset
;;;; The rt-fract-solset is specialized for storing solution sets.  It 
;;;; will specify an external format of float for storage.  


(defun make-rt-fract-solset (Set)
  "Generate an RT-Fract-Solset."
  (if (or (not (rt-solset-set-p Set))
	  (not (set-type-testp 'rt-fract-score Set)))
      (error "Incorrect solset type supplied to make-rt-fract-solset.")
    (make-instance 'rt-fract-solset :Value Set)))

(defclass rt-fract-solset (rt-solset) ())


;;; Since we know a-priori that this set will only contain fracts the 
;;; externtype info is much easier to handle.  In this case we will
;;; just return the a-prior float type.  Because the code will still
;;; work appropriately I will not subclass rt-solset->externtype.
(defmethod get-rt-solset-externtype ((Set rt-fract-solset))
  "Get the external type of the rt-fract-solset."
  'float)


;;;; ------------------------------------------------------------------
;;;; RT-Num-Solset
;;;; An rt-solset that stores only rt-num-scores and their descendents.
;;;; The purpose of this is to facilitate external storage.  




