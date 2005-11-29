// indysgg3.cpp   more routines to be called from the interface
//        
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved
//
// Defines:
//   solveOneEqn
//   subInOneEqn

#include "decl.h"
#include "dbg.h"
#include "extstruct.h"
#include "indyset.h"
#include "unitabr.h"
#include <math.h>
#include "indysgg.h"
#include "extoper.h"

using namespace std;

#define DBG(A) DBGF(INDYEMP,A)
#define DONCHOICE
// this is to not make decisions about multiple roots, but ask instead
// if DONCHOICE is not defined, solveOneEqn will see if constraints leave
// just one root, and if so gives it, if not fails.
// currently this is only implemented in the polysolve portion

// in extstruct: canonvars, canoneqf, studeqf
extern vector<valander *> studgrads;
int indyAddStudEq(int slot, const char* const equation);	// in indysgg
extern bool gotthevars;					// in indysgg.cpp
int checksol(const binopexp* const eqn, 		// in checksol.cpp
	     const vector<double>* const sols, const double reltverr);
numvalexp * getfromunits(const string & unitstr);	// in unitabr.cpp

/************************************************************************
 * solveOneEqn attempts to solve the equation in sourceSlot for the 
 *    variable varName. If it can do so, it places an equation of the 
 *    form varName = expression in destSlot and returns that equation
 *    in lisp form. If not, it returns an empty string.
 ************************************************************************/
string solveOneEqn(const char * const varName, const int sourceSlot, 
		   const int destSlot)
{
  int q;
  binopexp * simpeq;
  DBG(cout<<"Entering solveOneEqn with " << varName << ", "
      << sourceSlot << ", " << destSlot << endl;);
  if (destSlot >= STUDEQSZ) throw(string("destination slot ") 
				  + itostr(destSlot) + " doesn't exist");
  if (sourceSlot >= STUDEQSZ) throw(string("source slot ") 
				  + itostr(sourceSlot) + " doesn't exist");
  if (studeqf[sourceSlot] == (binopexp *) NULL)
    throw(string("Asked to solve an empty equation"));
  if (studeqf[destSlot] != (binopexp *) NULL) {
    studeqf[destSlot]->destroy();
    studeqf[destSlot] = (binopexp *) NULL; }
  if (studgrads[destSlot] != (valander *) NULL) {
    delete studgrads[destSlot];
    studgrads[destSlot] = (valander *) NULL; }
  for (q=0; q < canonvars->size(); q++) 
    if ((*canonvars)[q]->clipsname == varName) break;
  if (q == canonvars->size()) 
    throw(string("solveOneEqn called with nonexistant varName ")+varName);
  varindx var = q;
  expr * simp = copyexpr(studeqf[sourceSlot]);
  DBG(cout << "solveOneEqn trying to solve for " << varName << " in equation "
      << simp->getInfix() << endl;);
  if (!exprcontains(simp,var)) {
    simp->destroy();
    throw(string(
	 "solveOneEqn asked to solve for variable not in equation specified"));
  }
  // tools used to solve one equation in one unknown:
  // linvarcoefs  should work if equation in linear in the one variable.
  // others to add?
  expr * numer = (expr *) NULL;  
  expr * coef = (expr *) NULL;  
  if (linvarcoefs(simp,var,coef, numer)) {
    DBG(cout << "In solveOneEqn, linvarcoefs said var was "
	<< numer->getInfix() << " / " << coef->getInfix() << endl;);
    simp->destroy();
    bool encorep = true;  // this loop prob unneccessary, as normexpr does some
    while (encorep) { encorep = flatten(coef); eqnumsimp(coef,true); }
    numvalexp * cfact = normexpr(coef);
    if (cfact == NULL)
      throw(string("solveOneEqn found zero coef of variable to solve for"));
    cfact->value = -1./cfact->value;
    cfact->MKS *= -1;
    kmult(numer,cfact);
    if (coef->etype == numval) {
      cfact =(numvalexp *)coef;
      if ((cfact->value == 1.) && cfact->MKS.zerop()) {
	simp = numer;
	coef->destroy();
	goto didcoef;
      }}
    simp = new binopexp(&divby,numer,coef);
  didcoef:
    encorep = true; 
    while (encorep) { encorep = flatten(simp); eqnumsimp(simp,true); }
    DBG(cout << "In solveOneEqn, after simplifying linvarcoefs, var is "
	<< simp->getInfix() << endl;);
    goto didsol;
  } // end of if linvarcoefs
  if (simp->etype != binop) throw(string("lost equation in solveOneEqn"));
  simpeq = (binopexp *) simp;
  if (nlsolvov(simpeq))	{	// we already know simp contains var, so if
    simp = simpeq;              // it only contains one variable, must be var
    DBG(cout << "solveOneEqn got answer from nlsolvov : "
	<< simp->getInfix() << endl;);
    goto didsol; }
  if (hasjustonevar(simp,var)) {
    DBG(cout << "solveOneEqn hasjustonevar said true" << endl;);
    rationalize(simpeq);
    vector<double> * coefs = NULL; 		// Is this code 
    if (polyexpand(simpeq->lhs,var,coefs))   // duplicating what is
      {					// already in polyslv
	DBG(				// and polysolve?
	{
	  cout << "polyexpand gives ";
	  for (q = ((int)coefs->size())-1; q >= 0; q--)
	    cout << (*coefs)[q] << "*x^" << q << " +";
	  cout << "0"<< endl;
	} ) ;
	vector<double> * roots = findallroots(coefs);
	delete coefs;
	DBG( cout << "findallroots found " << roots->size() 
	     << endl; );
	// Don thinks we should not choose roots, but present all
	// choices to the student
#ifndef DONCHOICE
	for (q=0; q < roots->size(); q++)
	  {
	    if ((((*roots)[q]<0) && (*canonvars)[var]->isnonneg)
		|| (((*roots)[q] == 0) && 
		    (*canonvars)[var]->isnonneg &&
		    (*canonvars)[var]->isnonzero  ))
	      {
		(*roots)[q]=(*roots)[roots->size()-1];
		roots->pop_back();
		q--;
	      }
	  }
	DBG( cout << "After discarding wrong ones, have "
	     << roots->size() << endl; );
#endif
	if (roots->size() == 1)
	  {
	    simpeq->destroy();
	    simpeq = new binopexp(&equals,
				     new physvarptr(var),
				     new numvalexp((*roots)[0]));
	    delete roots;
	    simp->destroy();
	    simp = simpeq;
	    DBG(cout << "solveOneEqn polysolve got " << simp->getInfix()
		<< endl;);
	    goto didsol;
	  }
	else { 
#ifdef DONCHOICE
	  if (roots->size() > 1) {
	    string answer("(");
	    binopexp *tempeq = new binopexp(&equals,new physvarptr(var),
					    new numvalexp(1.));
	    tempeq->rhs->MKS = tempeq->lhs->MKS;
	    for (q=0; q < roots->size();q++) {
	      ((numvalexp *)tempeq->rhs)-> value = (*roots)[q];
	      answer.append(tempeq->getLisp(true));
	    }
	    answer.append(")");
	    tempeq->destroy();
	    delete roots;
	    return(answer);
	  } // end of if there are two or more roots
#endif	  
	  delete roots; 
	}
      }	// end of if polyexpand
  } // end of if hasjustonevar
  return(string(""));
  
  didsol:
  DBG(cout << "solveOneEqn entering didsol" << endl;);
  binopexp * reteq = new binopexp(&equals,new physvarptr(var),simp);
  if (equaleqs(reteq,studeqf[sourceSlot])) {
    DBG(cout << "solveOneEqn solved but found no change" << endl;);
    reteq->destroy();
    return string("");
  }
  indyAddStudEq(destSlot,reteq->getLisp(false).c_str());
  //  string answer = reteq->getLisp(true).c_str();
  string answer = reteq->getLisp(true);
  DBG(cout << "solveOneEqn solved it, got " << reteq->getInfix()
      << " , returning " << answer << endl;);
  reteq->destroy();
  return(answer);
}


/************************************************************************
 * subInOneEqn(source, target, dest)
 *    the source should be of the form var = expr, where expr does not
 *    contain var. 
 *    target has the equation into which the source equation will be
 *    substituted. The resulting equation, if different from the 
 *    target, is entered into the dest slot and returned as a lisp
 *    expression
 ************************************************************************/
string 
subInOneEqn(const int sourceSlot, const int targetSlot, const int destSlot)
{
  DBG(cout << "Entering subInOneEqn(" << sourceSlot << ", "
           << targetSlot << ", " << destSlot << endl;);
  if (destSlot >= STUDEQSZ) throw(string("destination slot ") 
				  + itostr(destSlot) + " doesn't exist");
  if (sourceSlot >= HELPEQSZ) throw(string("source slot ") 
				  + itostr(sourceSlot) + " doesn't exist");
  if (targetSlot >= HELPEQSZ) throw(string("target slot ") 
				  + itostr(targetSlot) + " doesn't exist");
  if (studeqf[sourceSlot] == (binopexp *) NULL)
    throw(string("Asked to use an empty equation"));
  if (studeqf[targetSlot] == (binopexp *) NULL)
    throw(string("Asked to substitute into an empty equation"));
  if (studeqf[destSlot] != (binopexp *) NULL) {
    studeqf[destSlot]->destroy();
    studeqf[destSlot] = (binopexp *) NULL; }
  if (studgrads[destSlot] != (valander *) NULL) {
    delete studgrads[destSlot];
    studgrads[destSlot] = (valander *) NULL; }
  if (studeqf[sourceSlot]->lhs->etype != physvart)
    throw(string("asked to use substitution rule which is not an assignment"));
			// wouldn't it be better to return something else?
  varindx var = ((physvarptr *)studeqf[sourceSlot]->lhs)->varindex;
  if (exprcontains(studeqf[sourceSlot]->rhs,var))
    throw(string(
	 "asked to use substitution rule which is eqn not yet solved"));
      // wouldn't it be better to return something else?
  DBG(cout << "subInOneEqn got var and okay substitution equation" << endl;);
  expr * simp = copyexpr(studeqf[targetSlot]);
  if (subexpin(simp,studeqf[sourceSlot])) {
    bool encorep = true; 
    while (encorep) { encorep = flatten(simp); eqnumsimp(simp,true); }
    if (simp->etype != binop) throw(string("lost equation in subInOneEqn"));
  indyAddStudEq(destSlot,simp->getLisp(false).c_str());
  string answer = simp->getLisp(true).c_str();
  simp->destroy();
  return(answer);
  } // end of if subexpin
  simp->destroy();
  return(string(""));
}
