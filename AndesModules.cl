#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AndesModules.cl
;; Collin Lynch
;; 2/7/2001
;;
;; This file defines the Andes module system.  The purpose of this 
;; file is to facilitate file grouping and loading but at a
;; less complex level than the defsystem interface.  
;;
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; just so .lsp works like .cl ... Lynn is lazy and subject to habits
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq system:*load-search-list*
  '(:first
    #.(make-pathname :type "efasl")
    #.(make-pathname :type "fasl")
    #.(make-pathname :type "cl")
    #.(make-pathname :type "lisp")
    #.(make-pathname :type "lsp")))


;;==========================================================================
;; Basic structure

(defstruct Andes-Module
  
  Name       ;;Unique Andes-Module Name
  Files      ;;Files to load.
  
  Compiles   ;;Files that can be compiled.
  
  Requires   ;;Modules that are required to be present.
  
  Path       ;;Loading path for this module relative to the *Base-Path*
  
  Specs      ;;String describing specifications.

  compile-code ;; Code that will be executed at compile time.
  load-code    ;; Code that will be executed at load time.
  
  Switches   ;;A list of switch parameters to be displayed by (sw).
  )



;;=========================================================================
;; Storage parameters.

(defparameter *Loaded-Andes-Modules* () "Currently active packages.")

;; Set the path by os type.
#+MSWINDOWS (defparameter *Base-Andes-Module-Path* "C:/andes2/" "The base path for this system.")
#+LINUX (defparameter *Base-Andes-Module-Path* "./" "The base path for this system.")


;;===========================================================================
;; Functions 

(defun rsa ()
  "Reload the modules from source."
  (ra t))

(defun ra (&optional (Source nil))
  "Reload the entire system."
  (load-andes-modules Source))

(defun rca ()
  "Recompile and load the system."
  (compile-andes-modules)
  (load-andes-modules))


(defun setup-Andes-Module-system (Base)
  "Setup the base Andes-Module system."
  
  (setq *Loaded-Andes-Modules* nil)
  (setq *Base-Andes-Module-Path* Base))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define-andes-module
;; This is the main loading function for andes modules. 
;; When called this function will test for dependencies and if satisfied 
;; generate the module and store it.

(defmacro define-Andes-Module (Name &key (Path nil) (Files nil) (compiles nil) 
					 (Requires nil) (Specifications nil) 
					 (Compile-Code nil) (Load-Code Nil) 
					 (Switches nil))
  "Define a package and load it."

  (loop for Req in Requires
      when (not (find Req *Loaded-Andes-Modules*
		      :test #'Equalp
		      :key #'Andes-Module-Name))
      do (error "Required Andes2 Module ~A is not loaded.~%" Req))
  
  (let ((M (make-Andes-Module :Name Name
			      :Path Path
			      :Files Files
			      :compiles Compiles
			      :requires Requires
			      :Specs Specifications
			      :compile-code compile-code
			      :load-code load-code
			      :Switches Switches)))
    
    
    (when (not (listp compiles))
      (setf (Andes-Module-Compiles M) Files))
    
    ;; Before appending the new module we want to remove any
    ;; matching modules (match by name) from the loaded set.
    ;; This will alert the user to any unloading that occurs.
    (dolist (Module *Loaded-Andes-Modules*)
      (when (equalp (andes-module-name Module) 
		    (andes-module-name M))
	(format t "Removing duplicate module:~%~a~%" Module)
	(setq *Loaded-Andes-Modules* 
	  (remove Module *Loaded-Andes-Modules*))))

    (setq *Loaded-Andes-Modules* (append *Loaded-Andes-Modules* (list M)))
    
    M))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Access functions.

(defun get-Andes-Module (Andes-Module-Name)
  "Get the named Andes-Module form the list if present."
  
  (find Andes-Module-Name *Loaded-Andes-Modules* :key #'Andes-Module-Name))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loading Functions.

(defun load-Andes-Modules (&optional (Load-Source nil))
  "Load the current contents of *Loaded-Andes-Modules*."
  (dolist (M *Loaded-Andes-Modules*)
    (format t "------------------------------~%  Loading Module ~a~%"
	    (andes-module-Name M))
    (Load-Andes-Module M Load-Source)))


(defun load-named-Andes-Module (Andes-Module-Name &optional (load-source nil))
  "Load the named Andes-Module with source potentially."
  (load-Andes-Module (get-Andes-Module Andes-Module-Name) load-Source))


(defun Load-Andes-Module (M &optional (Load-Source nil))
  "Load the specified Andes-Module."
  (dolist (F (Andes-Module-Files M))
    (let ((ext ""))
      ;; force load of source .cl if that was specified coming in
      ;; OR the file in question is not in the module's "compiles" list.
      (when (or Load-Source 
	        (not (member F (Andes-Module-Compiles M) :test #'string-equal)))
	(setf ext ".cl"))
      (load-relative-file *Base-Andes-module-Path* (Andes-Module-Path M) F ext)))
  
  (dolist (n (Andes-Module-Load-code M))
    (if (cdr n)
	(funcall (car n) (cdr n))
      (funcall (car n)))))


    

(defun Load-relative-file (Base Relative File &optional (Ext ""))
  "Load the specified file relative to base, recompiling as needed"
  ;; makes and loads compiled unless .cl not specified
  (let ((src) (FileSpec (concatenate 'string Base Relative File Ext))
        (Fasl (concatenate 'string Base Relative File ".fasl")))
    
    ;; Test for the type of the source file and set it if it is present
    ;; if not then raise a cerror regarding it and move on.
    (cond ((probe-file (concatenate 'string Base Relative File ".cl"))
	   (setq src (concatenate 'string Base Relative File ".cl")))
	  ((probe-file (concatenate 'string Base Relative File ".lsp"))
	   (setq src (concatenate 'string Base Relative File ".lsp")))
	  (t (cerror "Carry on without loading it" 
		     "Source file ~A not found." src)))
    
    
    (cond ((probe-file (pathname FileSpec)) ;; if found with specified ext
	   (load (pathname FileSpec)))	    ;; load what caller requested (.cl)

	   ((probe-file fasl)               ;; fasl and cl already exists
	    (when (file-older-p fasl src) 
	      (format T "~&; Recompiling ~A because it is out of date~%" src)
	      (compile-file src))
	    (load fasl))
	  
 	  (T ;(format T "~&; Compiling ~A~%" src)
	     (compile-file src)
	     (load fasl)))))

    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compilation functions.

(defun compile-Andes-Modules ()
  "Compile the currently loaded Andes-Modules."
  (dolist (M *Loaded-Andes-Modules*)
    (compile-Andes-Module M)))


(defun compile-named-Andes-Module (Andes-Module-Name)
  "Compile the named Andes-Module."
  (compile-Andes-Module (get-Andes-Module Andes-Module-Name)))
    

(defun compile-Andes-Module (M)
  "Compile-the specified Andes-Module."
  (if (Andes-Module-compiles M)
      (dolist (F (Andes-Module-Compiles M))
	(compile-file (pathname (concatenate 'string 
				  *Base-Andes-module-Path* (Andes-Module-Path M) F ".cl")))))
  (dolist (n (Andes-Module-Compile-code M))
    (if (cdr n)
	(funcall (car n) (cdr n))
      (funcall (car n)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Switch info.

(defun sw ()
  (switches))

(defun switches ()
  "Obain a list of Andes-Module switches."
  (let (tmp)
    (dolist (S (loop for M in *Loaded-Andes-Modules*
		   when (Andes-Module-Switches M)
		   append it))
      
      (setq tmp (handler-case (list S (symbol-value S) (documentation S 'variable))
		  (unbound-variable (v) (list S "UNBOUND" ()))))
      (format t "~A~16,T= ~A~22,T ~A~%" (car Tmp) (cadr Tmp) (caddr Tmp)))))



;;=====================================================================
;; Provision

(provide 'Andes-Module-System)
