#|==================================================================
;; TestSet.cl
;; Collin Lynch
;; 3/17/2003
;;
;; This file defines the testset struct an orderd struct used to 
;; collect multiple tests into a single item and to execute or 
;; combine them as necessary.
;; 
;; The purpose of this framework is to make it easier for new types
;; of tests to be added to and removed from the HelpDriver without 
;; invalidating the underlying system or necessitating major alterations.
;;
;; The Testset struct was not designed for either ease of use or any 
;; other features that is why its structure is somewhat less than 
;; ideal in parts.  
;;
;; A testset contains the following fields:
;;  Name:  
;;   An atomic name for the test type that can be used as a table name
;;   or as part of a filename.
;;
;;  Tests:  
;;   A list of test predicates to be passed to the executor.  These tests
;;   are assumed to be listed as atoms that can be funcalled and can be
;;   printed to the database as a string of 64 characters or less.  These
;;   Tests will be used to generate a corresponding result set at runtime.
;; 
;;  Executor:  
;;   A funcallable function name or lambda expression that takes 4 args:
;;    Tests: the set of tests to be executed at runtime this will be the
;;      list stored in the tests field.
;;    Command: The current CMD in the run to be evaluated as necessary.
;;    Stack:  The current execution struct to be evaluated as necessary.
;;    &optional
;;      Results:  A list of the same length as TESTS s.t. for each TEST(N)
;;       Results(N) is the result value of that test.  These results are
;;       all expected to be of the same type and length as the specifiers.
;; 
;;  ResultType:  
;;   An atomic type specifier that will be used when storing the results
;;   to a database.  later on I will list the legal types here.
;;
;;  ExternalType
;;   This is an atomic type specifier that is compatible with the lisp
;;   external type mechanism.  It will be used when we are fetching
;;   Results using select commands for internal use.
;;
;;  ResultCombiner:  
;;   A funcallable function name or lambda expression that can combine
;;   two corresponding Result lists.  This should be a func of 2 args.
;;
;;   The restriction on the Resultcombiner is this:
;;    It must be valid to combine any sequence of work using this.  That
;;    is, the result of executing these tests on an entrie file must be
;;    the same as executing the tests on each individual problem instance
;;    in the file and then combining the results.  
;;
;;  InitResults:
;;   A funcallable name or lambda exp that returns a list of "empty"
;;   results of the appropriate type for the set.  It takes the set
;;   as an argument.
;;
;; The logic of how the tests are executed, the results produced, the results
;; updated, and the results combined is up to the writer.  The only 
;; restirctions are that the results and tests be lists that correspond 
;; appropriately to the tests, that the tests be stored as lists and that both 
;; can be printed to 
;;
;;
;; NOTE:: Test names must be funcallable symbols composed of Integers, Characters
;;  and some symbols.  The bollowing symbols are known to be forbidden:
;;    . Used for table connection.
;;    - Used for Negative numbers.



|#

(defstruct TestSet
  Name  ;; An atomic name.
  Tests
  Executor 
  ResultType
  ExternalType
  ResultCombiner
  InitResults)



;;; -----------------------------------------------------------------------------
;;; Utility functions.
;;; The utility functions are used to perform common operations on the test sets
;;; for later use. And to siplify typical parts.

;;; Execute the testset on the specified arguments including 
;;; the optional resultset and return the results.
(defun testset-execute (Set Command Stack &optional (Results Nil))
  (funcall (Testset-Executor Set) (testset-Tests Set) Command Stack Results))


;;; Given a pair of results and a corresponding testset combine the results
;;; and return the resulting value.
(defun testset-combine-results (Set R1 R2)
  (funcall (testset-ResultCombiner Set) R1 R2))

;;; Given a Testset and a set of resultsets combine the resultsets in
;;; order using the testset's combining algorithm.
(defun testset-combine-resultsets (Set Results)
  (let ((Result (car Results)))
    (dolist (Next (cdr Results))
      (setq Result (apply (testset-resultcombiner Set) Result Next)))
    Result))
          
;;; Given a Testset generate an appropriate list of "Empty" results of the 
;;; appropriate type whatever they happen to be.
(defun testset-make-empty-results (Set)
  (funcall (testset-initresults Set) Set))


;;; Given a list of testsets map them to lower-case names
;;; for use as tableids or other values.
(defun map-testsets->tablenames (Sets)
  (mapcar #'(lambda (S) (format Nil "~(~a~)" (testset-name S))) Sets))


;;; Given a testset map it to a valid tablename
(defun map-testset->tablename (Set)
  (format Nil "~(~a~)" (Testset-Name Set)))


;;; Given a Testset generate a list of columnspecs from it that can 
;;; be added to any table generation command.
(defun map-testset->colspecs (Set)
  (odbc-format-list-columnspecs 
   (testset-tests Set) (testset-Resulttype Set)))

;;; Given a testset map the set to a list of type specifiers 
;;; of the same length as the number of tests in the list.
(defun map-testset->exttypes-list (Set)
  (make-list (length (testset-tests Set)) 
	     :initial-element (testset-Externaltype set)))


