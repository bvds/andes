#ifndef _H_SOLVER_H_
#define _H_SOLVER_H_
/////////////////////////////////////////////////////////////////////////////////////////////////
// solver.h -- includes/macros/prototypes/defines for using the solve dll
// Copyright (C) 2001 by <Linwood H. Taylor's Employer> - All Rights Reserved.
// Author(s): Linwood H. Taylor <lht@lzri.com>
// Modified:
//              31 January 2001 - lht -- created
//        19 February 2001 - lht -- modified for additional functionality
//    26 April 2001 - JaS -- replaced isIndependent and Expand functions
/////////////////////////////////////////////////////////////////////////////////////////////////
#ifndef NO_DLL
  #ifdef SOLVER_EXPORTS
          #ifdef __cplusplus
                  #define SOLVER_API extern "C" __declspec(dllexport)
          #else // ifndef __cplusplus
                  #define SOLVER_API __declspec(dllexport)
          #endif // ndef __cplusplus
  #else
          #ifdef __cplusplus
                  #define SOLVER_API extern "C" __declspec(dllimport)
          #else // ifndef __cplusplus
                  #define SOLVER_API __declspec(dllimport)
          #endif // ndef __cplusplus
  #endif
#else // def NO_DLL
  #define SOLVER_API
#endif // def NO_DLL

/////////////////////////////////////////////////////////////////////////////////////////////////
// defines that just make code easier to read
/////////////////////////////////////////////////////////////////////////////////////////////////
#define RETURN_INT SOLVER_API int
#define RETURN_CSTRING SOLVER_API char*

/////////////////////////////////////////////////////////////////////////////////////////////////
// following pragma disable MSVC++ warning about truncating long var in debug
/////////////////////////////////////////////////////////////////////////////////////////////////
#pragma warning(disable : 4786)

/////////////////////////////////////////////////////////////////////////////////////////////////
#include "lrdcstd.h"

/////////////////////////////////////////////////////////////////////////////////////////////////
// because there is a naming mismatch between these functions and the
//   functions that these hide; the following is a mapping between the
//   calls made available here and the associated calls used from the
//   solver/independence/red-green library and the lisp-side calls
//
//   lisp                     DLL                            Algebra
// solve-problem-file     - solveBubbleFile              - doColAnderMain
// solve-problem          - solveBubble                  - solveTheProblem
// solve-more-problem     - solveMoreBubble              - solveMoreOfTheProblem
// send-problem-statement - solveAdd                     - handleInput
// new-problem            - solveClear                   - clearTheProblem
//
// (obs)                  - c_indyIsIndependent          - indyIsCanonIndy
// indyAddVar             - c_indyAddVariable            - indyAddVar
// indyDoneAddVar         - c_indyDoneAddVariable        - indyDoneAddVar
// indyAddEquation        - c_indyAddEquation            - indyAddCannonEq
// indyEmpty              - c_indyEmpty                  - indyEmpty
// indyAddEq2Set          - c_indyAddEq2Set              - indyAddEq2CanSet
// indyKeepN              - c_indyKeepNOfSet             - indyKeepN
// (obs)                  - c_indyExpandInSet            - indyExpSetCanEq
// isIndependent          - c_indyCannonHowIndy
//
// (obs)                  - c_indyStudentExpandInSet     - indyExpSetStudEq
// (obs)                  - c_indyStudentisIndependent   - indyIsStudIndy
// studentAddOkay         - c_indyStudentAddEquationOkay - indyAddStudEq
// isstudentokay          - c_indyIsStudentEquationOkay  - indyIsStudEqnOkay
// studentIsIndependent   - c_indyStudHowIndy
// indysimplifyeqn        - c_simplifyEqn                - simplifyEqn
// indysolveoneeqn        - c_solveOneEqn                - solveOneEqn
// indysubinoneeqn        - c_subInOneEqn                - subInOneEqn


/////////////////////////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////
//                  Solver routines follow
/////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////////////////////////
// solveBubbleFile -- opens a new problem set
// argument(s):
//     src -- specification of file and location of file that is used to get
//                      input (will fail if null)
//     dst -- specification of file and location of file that will be written
//                      to (if null results will be returned in char*)
// returns:
//     char* - "t" if all went well else an error string of the form:
//                      (Error: <function(arg)> "description")
// notes:
//     will fail on file not found, can't read or can't write minimally should
//                      also return fail if file not correctly formatted
//              will perform a solveClear if not cleared
/////////////////////////////////////////////////////////////////////////////////////////////////
RETURN_CSTRING solveBubbleFile(const char* const src, const char* const dst);

/////////////////////////////////////////////////////////////////////////////////////////////////
// solveBubble - begins solution process
// argument(s):
//      NONE
// returns:
//      char* containing first portion of solution or error message of the form:
//                      (Error: <function(arg)> "description")
// notes:
/////////////////////////////////////////////////////////////////////////////////////////////////
RETURN_CSTRING solveBubble();

/////////////////////////////////////////////////////////////////////////////////////////////////
// solveMoreBubble - gets more of solution
// argument(s):
//      NONE
// returns:
//      char* with value next un-returned portion of solution, or "nil" if all
//              protions of solution have been retrieved; or an error message 
//              of the form: (Error: <function(arg)> "description")
// notes:
/////////////////////////////////////////////////////////////////////////////////////////////////
RETURN_CSTRING solveMoreBubble();

/////////////////////////////////////////////////////////////////////////////////////////////////
// solveAdd -- uses argument to define variable(s)/equation(s)/etc to be added
// argument(s):
//      task -- null terminated c_string that is parsed to determine task to be
//              done
// returns:
//      char* - "t" if all went well else an error string of the form:
//              (Error: <function(arg)> "description")
// notes:
//      char* will be a lisp expression representing the information to be added
/////////////////////////////////////////////////////////////////////////////////////////////////
RETURN_CSTRING solveAdd(const char* const lispExpression);

/////////////////////////////////////////////////////////////////////////////////////////////////
// solveClear -- clears state for next problem
// argument(s):
//      NONE
// returns:
//      char* - "t" if all went well else an error string of the form:
//              (Error: <function(arg)> "description")
// notes:
//      should always succeed
/////////////////////////////////////////////////////////////////////////////////////////////////
RETURN_CSTRING solveClear();


/////////////////////////////////////////////////////////////////////////////////////////////////
// c_powerSolve
// argument(s):
//   data of the form (howStrong variableName equationSlot)
//     howstrong is an indication of what the solve tool should try to
//        use in solving for the variable. The only value you want to
//        use now is 31.
//     variableName is the name of the variable to solve for
//     equationSlot is the slot to write the new equation to
// return(s): The string returned is an empty string if the equations could
//     not be solved, or a lisp form of the equation to be entered into slot
//     destslot.
/////////////////////////////////////////////////////////////////////////////////////////////////
RETURN_CSTRING c_powersolve(const char* const lispExpression);

/////////////////////////////////////////////////////////////////////////////////////////////////
// c_simplifyEqn
// argument(s):
//   data of the form (sourceSlot destSlot)
//     sourceSlot is the slot number of the slot to be simplified
//     destSlot is the slot to write the new equation, if any change is made
// return(s): The string returned is an empty string if the equations could
//     not be solved, or a lisp form of the equation to be entered into slot
//     destslot.
/////////////////////////////////////////////////////////////////////////////////////////////////
RETURN_CSTRING c_simplifyEqn(const char* const lispExpression);

/////////////////////////////////////////////////////////////////////////////////////////////////
// c_solveOneEqn
// argument(s):
//   data of the form (varName sourceSlot destSlot)
//     varName is the canonical name of the variable to be solved for
//     sourceSlot is the slot of the equation to be simplified.
//     destSlot is the slot to write the new equation to.
// return(s): The string returned is an empty string if the equation could
//     not be solved, or a lisp form of the equation to be entered into slot
//     destslot.
/////////////////////////////////////////////////////////////////////////////////////////////////
RETURN_CSTRING c_solveOneEqn(const char* const lispExpression);

/////////////////////////////////////////////////////////////////////////////////////////////////
// c_subInOneEqn
// argument(s):
//   data of the form (sourceSlot targetSlot destSlot)
//     sourceSlot is the slot of the (possibly symbolic) assignment statement
//        to be substituted in
//     targetSlot is the slot of the equation in which to substitute
//     destSlot is the slot to write the new equation to.
// return(s): The string returned is an empty string if the substitution 
//     produced no change, or a lisp form of the equation to be entered into 
//     slot destSlot.
/////////////////////////////////////////////////////////////////////////////////////////////////
RETURN_CSTRING c_subInOneEqn(const char* const lispExpression);

/////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////
//                  Independence routines follow
/////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////////////////////////
// c_indyAddVariable -- used to add/collect variables and their value prior to
//              use in equations
// argument(s):
//      data - of the form "(varName value units)"
//              varName - string - represents name of variable
//              value - double - the value associted with varName
//      units - string - a 'book' form unit (for example: m/s^2 for meters per
//                      second squared)
// returns:
//      char* - "t" if all went well else an error string of the form:
//              (Error: <function(arg)> "description")
// notes:
/////////////////////////////////////////////////////////////////////////////////////////////////
RETURN_CSTRING c_indyAddVariable(const char* const data);

/////////////////////////////////////////////////////////////////////////////////////////////////
// c_indyDoneAddVariable -- used to signal the end or cessation of adding vars
// arguments(s):
//      NONE
// returns:
//      char* - "t" if all went well else an error string of the form:
//              (Error: <function(arg)> "description")
// note(s):
//      should always return "t"
/////////////////////////////////////////////////////////////////////////////////////////////////
RETURN_CSTRING c_indyDoneAddVariable();

/////////////////////////////////////////////////////////////////////////////////////////////////
// c_indyAddEquation -- used to add equations to the equation 'database'
// argument(s):
//      data - of the form "(equationID equation)"
//              equationID - integer - the identifier used to refer to equation
//              equation - list - lisp-style prefix equation
// returns:
//      char* - "t" if all went well else an error string of the form:
//              (Error: <function(arg)> "description")
// notes:
/////////////////////////////////////////////////////////////////////////////////////////////////
RETURN_CSTRING c_indyAddEquation(const char* const data);

/////////////////////////////////////////////////////////////////////////////////////////////////
// c_indyEmpty -- clear equations, sets of equations and variables current in
//              preparation for new series of questions
// argument(s):
//      NONE
// returns:
//      char* - "t" if all went well else an error string of the form:
//              (Error: <function(arg)> "description")
// notes:
/////////////////////////////////////////////////////////////////////////////////////////////////
RETURN_CSTRING c_indyEmpty();

/////////////////////////////////////////////////////////////////////////////////////////////////
// c_indyAddEq2Set -- used to build sets of equations
// argument(s):
//      data - of the form "(setID equationID)"
//              setID - integer - set to be modified
//              equationID - integer - id of equation to be included in this set
// returns:
//      char* - "t" if all went well else an error string of the form:
//              (Error: <function(arg)> "description")
// notes:
/////////////////////////////////////////////////////////////////////////////////////////////////
RETURN_CSTRING c_indyAddEq2Set(const char* const data);

/////////////////////////////////////////////////////////////////////////////////////////////////
// c_indyKeepNOfSet - used to remove portion of a set
// argument(s):
//      data - of the form "(setID numberToKeep)"
//          setID - integer - identifies the set to be modified
//          numberToKeep - integer - removes all but the first 'numberToKeep eqs
// returns:
//      char* - "t" if all went well else an error string of the form:
//              (Error: <function(arg)> "description")
// notes:
/////////////////////////////////////////////////////////////////////////////////////////////////
RETURN_CSTRING c_indyKeepNOfSet(const char* const data);

/////////////////////////////////////////////////////////////////////////////////////////////////
// c_indyStudentAddEquationOkay -- used to add equations to the equation
//    'database'
// Equation should first be checked with c_indyIsStudentEquationOkay
// argument(s):
//    data - of the form "(equationID equation)"
//      equationID - integer - the identifier used to refer to equation
//      equation - list - lisp-style prefix equation
// returns:
//    char* -  if fails returns an error string of the form:
//             (Error: <function(arg)> "description") or
//      0: (ADDEDEQN)                   added equation okay
//      1: (SLOTEMPTIED)                slot emptied as requested
//      2: (NOSUCHSLOT)                 slot number not a valid slot
//      3: (NOPARSE)                    equation didn't parse, 
//      [NOPARSE is currently impossible - an exception is thrown instead
//      4: (EQNNOTOK)                   equation is not an equation.
//      [SINGULAR is not implemented. It is intended for when the 
//          gradient is infinite at the solution point. If this happens,
//          independence checking will not work.
//        SINGULAR currently causes an exception if gradient is undefined, so
//        SINGULAR is never returned]
//       
//       
//       
RETURN_CSTRING c_indyStudentAddEquationOkay(const char* const data);

/////////////////////////////////////////////////////////////////////////////////////////////////
// c_indyIsStudentEquationOkay -- used to check student equation.
// argument(s):
//    data - of the form "(equation)"
//      equation - list - lisp-style prefix equation
// returns:
//    char* -  if fails returns an error string of the form:
//             (Error: <function(arg)> "description") or
// return values from indyIsStudEqnOkay
//      UNPARSEABLE throws an exception
//      32: (NOTANEQ)           string is not a parseable equation 
//      (0|1|2) + (0|4) + (0|8|16)
//      (0|8|16): 
//          0: units check correctly
//          8: units may have been omitted
//         16: units are just wrong
//      (0|4):
//          0: equation balences to better than 1 percent
//          4: it doesn't
//      (0|1|2):
//          0: equation balences to better than expected descrepancy
//          1: doesn't, but balences within 100 times expected descrepancy
//          2: worse than that
// spelled out:                                 units   units           units
//                                               OK     left off        not OK
// value checks precisely                       0           8           16
// value off by 1% and also                     
//      by >100 exp err                         6           14          22
//      by > exp err but not 100 x              5           13          21
//      <- expected error                       4           12          20
// value better than 1% but
//      by >100 exp err                         2           10          18
//      by > exp err but not 100 x              1           9           17
//      <- expected error                       0           8           16
//
RETURN_CSTRING c_indyIsStudentEquationOkay(const char* const data);

/////////////////////////////////////////////////////////////////////////////////////////////////
// c_indyCanonHowIndy - asks whether a given canonical equation is independent
//        of those in a set, and if not, which equations can derive it
// argument(s):
//     data - of the form "(setID eqnID)"
//         setID - integer - identifies the set to be examined
//         eqnID - integer - index of the tested equation in the list canoneqf
//                           of canonical equations
// returns:
//     char* - of the form (type (list1) (list2))  or
//         an error message of the form (Error: <function(arg)> "description")
//     type is an integer:
//         0: equation eqnID is definitely independent of those in set
//         1: in linear approximation it appears to depend on the equations
//            given in list1, but the full equation appears likely to depend
//            on those in list2 as well, and appears to depend on more as
//            well, so may well be independent of equations in set.
//         2: in linear approximation it appears to depend on the equations
//            given in list1, but the full equation appears likely to depend
//            on those in list2 as well. It is probably only dependent on
//            the union of the two lists.
//         3: The equation appears to be dependent only on the equations in
//            list 1.
//         4: The equation is dependent on the equations in list 1.
//     list1: a list of integers, the index in canoneqf of the equations
//            upon which eqnID depends.
//     list2: a list of integers, indices in canoneqf of equations on which
//            eqnID may also depend.
// notes:
/////////////////////////////////////////////////////////////////////////////////////////////////
RETURN_CSTRING c_indyCanonHowIndy(const char* const data);

/////////////////////////////////////////////////////////////////////////////////////////////////
// c_indyStudHowIndy - asks whether a given canonical equation is independent
//     exactly the same as c_indyCanonHowIndy, cf., except eqnID is slot
//     number of student equation rather than index in canoneqf of canonical
//     equation.
/////////////////////////////////////////////////////////////////////////////////////////////////
RETURN_CSTRING c_indyStudHowIndy(const char* const data);

/////////////////////////////////////////////////////////////////////////////////////////////////
// the two functions c_indyCanonHowIndy and c_indyStudHowIndy replace the four
//    following obsolete functions


/////////////////////////////////////////////////////////////////////////////////////////////////
// -- OBSOLETE -- ///////////////////////////////////////////////////////////////////////////////
// c_indyIsIndependent -- used to determine if equation is independent of cur-
//      rent equation set
// argument(s):
//      data - of the form "(setID equationID)"
//              setID - integer - identifes which set of equations to use
//              equationID - integer - identifies which equation to check
// returns:
//      char* - "t" if all went well else an error string of the form:
//              (Error: <function(arg)> "description")
// notes:
// -- OBSOLETE -- ///////////////////////////////////////////////////////////////////////////////
// RETURN_CSTRING c_indyIsIndependent(const char* const data);
/////////////////////////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////////////////////////
// -- OBSOLETE -- ///////////////////////////////////////////////////////////////////////////////
// c_indyExpandInSet - used to determine set of equations that are independant
//      and complete solutions from the set specified and the equation specified
// argument(s):
//      data - of the form "(setID equationID)"
//          setID - integer - identifies the set to be modified
//          equationID - integer - identifies the set to be modified
// returns:
//      char* - of the form (x0 x1 x2 ... xn) indeces of resultant equations or
//          an error message of the form (Error: <function(arg)> "description")
// notes:
// -- OBSOLETE -- ///////////////////////////////////////////////////////////////////////////////
// RETURN_CSTRING c_indyExpandInSet(const char* const data);
/////////////////////////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////////////////////////
// -- OBSOLETE -- ///////////////////////////////////////////////////////////////////////////////
// c_indyStudentExpandInSet - used to determine set of equations that are in-
//  dependant   and complete solutions from the set specified and the equation
//  specified
// argument(s):
//      data - of the form "(setID equationID)"
//          setID - integer - identifies the set to be modified
//          equationID - integer - identifies the set to be modified
// returns:
//      char* - of the form (x0 x1 x2 ... xn) indeces of resultant equations or
//          an error message of the form (Error: <function(arg)> "description")
// notes:
// -- OBSOLETE -- ///////////////////////////////////////////////////////////////////////////////
// RETURN_CSTRING c_indyStudentExpandInSet(const char* const data);
/////////////////////////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////////////////////////
// -- OBSOLETE -- ///////////////////////////////////////////////////////////////////////////////
// c_indyStudentIsIndependant -- used to determine if equation is independent
//    of current equation set
// argument(s):
//      data - of the form "(setID equationID)"
//              setID - integer - identifes which set of equations to use
//              equationID - integer - identifies which equation to check
// returns:
//      char* - "t" if all went well else an error string of the form:
//              (Error: <function(arg)> "description")
// notes:
// -- OBSOLETE -- ///////////////////////////////////////////////////////////////////////////////
// RETURN_CSTRING c_indyStudentIsIndependent(const char* const data);
/////////////////////////////////////////////////////////////////////////////////////////////////

// end of OBSOLETE   ////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////////////////////////
// End of file solver.h
// Copyright (C) 2001 by <Linwood H. Taylor's Employer> - All Rights Reserved.
/////////////////////////////////////////////////////////////////////////////////////////////////
#endif // ndef _H_SOLVER_H_
