// neweqnokay.cpp    
//	new version of indyIsStudEqnOkay, 
//	new version of indyAddStudEq
// 	new function isEqnAnAssign
//        
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved

#include "decl.h"
#include "dbg.h"
#include "extstruct.h"
#include "unitabr.h"
#include <math.h>
#include "indysgg.h"
#include "extoper.h"
#include "valander.h"

using namespace std;

#define DBG(A) DBGF(INDYEMP,A)

// return values from 
#define UNPARSEABLE "not used"
#define OKAY 0
#define NOTANEQ 32
#define UNITSNG 8
#define NOT1PC 4
#define IMPREC 1
#define VERYNG 1
// note VERYNG is only added if IMPREC has already been

// return values from indyAddStudEq
#define ADDEDEQN 0
#define SLOTEMPTIED 1
#define NOSUCHSLOT 2
#define NOPARSE 3
// currently impossible - an exception is thrown instead
#define EQNNOTOK 4
#define SINGULAR "not used"
// SINGULAR currently causes an exception if gradient is undefined, so
//   SINGULAR is never returned

// in extstruct: canonvars, canoneqf, studeqf
extern vector<valander *> studgrads;
extern bool gotthevars;					// in indysgg.cpp
binopexp * getAnEqn(const string bufst, bool tight);	// in getaneqwu.cpp
bool getStudEqn(int slot, const string bufst);		// in getaneqwu.cpp
int checksol(const binopexp* const eqn, 		// in checksol.cpp
	     const vector<double>* const sols, const double reltverr);
numvalexp * getfromunits(const string & unitstr);	// in unitabr.cpp


/************************************************************************
 *  indyIsStudEqnOkay(equation)
 *     
 *         it doesn't evaluate a gradient
 *         it doesn't add anything to slots
 ************************************************************************/
int indyIsStudEqnOkay(const char * const equation) {
  DBG(cout << "indyIsStudentEquation asked about " << equation << endl;);
  // ensure that any variables to be added have been (as well as we can <g>)
  if (! gotthevars) {
    throw(string("indyIsStudEqnOkay called before indyDoneAddVar"));
  }
  int retval = OKAY;
  // check that all can be parsed --- should always be the case
  // otherwise implies bug in caller
  binopexp * theeqn =  getAnEqn(equation,true);
  // currently getAnEqn throws exceptions rather than returning NULL, 
  // so never returns UNPARSEABLE
  if (theeqn->op->opty != equalse) { return(NOTANEQ); }
  expr* eqexpr = (expr*)theeqn;
  expr * trouble = dimenchk(true,eqexpr);
  if (trouble != (expr *) 0L) {
    DBG(cout << "dimensional inconsistency at " << trouble->getInfix() 
             << endl;);
    retval += UNITSNG;
  }
  if (checksol((binopexp*)eqexpr, numsols, .01) > 0) retval += NOT1PC;
  if (checksol((binopexp*)eqexpr, numsols, RELERR) > 1) retval += IMPREC;
  if (checksol((binopexp*)eqexpr, numsols, 100 * RELERR) > 1) retval += VERYNG;

  if (retval >= UNITSNG)	// see if more lax units parsing would help
    {
      theeqn->destroy();
      theeqn =  getAnEqn(equation,false);
      eqexpr = (expr*)theeqn;
      trouble = dimenchk(true,eqexpr);
      if (trouble != (expr *) 0L) {
	DBG(cout << "bad dimensional inconsistency at " << trouble->getInfix() 
             << endl;);
	retval += UNITSNG;
      }
    }
  theeqn->destroy();
  DBG(cout << "Returning " << retval << " from indyIsStudEqnOkay" << endl;);
  return(retval);
}

/************************************************************************
 * indyAddStudEq(int slot, const char* const equation)			*
 *     places the student equation given in Lisp form in canonical      *
 *     variables by the string equation, into the student slot slot.	*
 *   Aborts if indyDoneAddVar has not already been called		*
 *   Each equation is inserted in studeqsorig[slot], although this is	*
 *     currently not used. If the string is empty, we clear the slot	*
 *     in studeqf and studgrads. Otherwise				*
 *   equation is converted to expr form and placed in studeqf[slot],	*
 *      by a call to getStudEqn, and its gradient at the solution point *
 *      is calculated and stored in studgrads[slot]			*
 ************************************************************************/
int indyAddStudEq(int slot, const char* const equation) {
  DBG(cout << "indyAddStudEq asked to add to slot " << slot
      << " the equation" << endl;);

  // ensure that any variables to be added have been (as well as we can <g>)
  if (! gotthevars) {
    throw(string("indyAddStudEq called before indyDoneAddVar"));
  }

  // ensure slot specified is within bounds
  if ((slot < 0) || (slot > HELPEQSZ)) {
    DBG(cout << "indyAddStudEq returning NOSUCHSLOT" << endl;);
    return(NOSUCHSLOT);
  }

  studeqsorig[slot]->assign(equation);
  // if equation is an empty string (or a NIL) empty slot
  if ((strlen(equation) == 0) || 
      (strcmp(equation,"NIL")== 0)) { // if empty we'll delete
    DBG(cout << "nothing, ie clear slot" << endl;);
    if (studeqf[slot]) { // It really means empty out
      studeqf[slot]->destroy(); // what had been there.
      studeqf[slot] = 0L;
    }
    if (studgrads[slot]) {
      delete studgrads[slot]; // (lht) was studeqf[slot];
      studgrads[slot] = 0L;
    }
    DBG(cout << "indyAddStudEq returning SLOTEMPTIED" << endl;);
    return(SLOTEMPTIED);
  }

  // check that all can be parsed --- should always be the case
  // otherwise implies bug in caller
  if (! getStudEqn(slot, equation)) {
    DBG(cout << "indyAddStudEq returning NOPARSE" << endl;);
    return(NOPARSE); // throw(string("Couldn't parse ") + string(equation));
  }

  DBG(cout << slot << ": " << studeqf[slot]->getInfix() << endl;);
  // equation is not an equation
  if ((studeqf[slot]->etype != binop) || 
      ((binopexp*)studeqf[slot])->op->opty != equalse) {
    DBG(cout << "indyAddStudEq returning EQNNOTOK" << endl;);
    return(EQNNOTOK);
  }
  DBG(cout << "Ready to enter equation gradient" << endl;);
  // What if derivative is singular at numsol?
  studgrads[slot] = getvnd(studeqf[slot], canonvars, numsols);
  return(OKAY);
}

//   not done Linn is doing it      bool isEqnAnAssign(slotID) {

  
