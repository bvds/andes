// solvetool.cpp    tools to help student's with their algebra
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved
/************************************************************************
 *  powersolve(int howstrong, varindx sought, destslot)			*
 *	attempt to solve for variable sought using all the student's	*
 *	green equations, using the power of checkeqs as follows:	*
 *	    howstrong = 0 --- only recursive assignment			*
 *	    howstrong = 1 --- also solve simultaneous linear eqs	*
 *	    howstrong = 2 --- add in factor + nlsolv			*
 *	    howstrong = 4 --- add in polysolve				*
 *	    howstrong = 8 --- add in solvetrig and undotrigvars		*
 *	    howstrong = 16 -- add in desperate				*
 *      these may be added to combine features. To get all the power	*
 *	of checkeqs, set howstrong = 31 .				*
 *	sought is the index of the sought variable in the canonvars list*
 *	destslot is the student slot in which to put the result, as an 	*
 *	    equation.							*
 *  returns a string lisp version of equation to place in destslot,	*
 *	if a solution found, or a zero-length string if no solution	*
 ********								*
 *  NOT YET IMPLEMENTED:						*
 *  expr * solveone(eqnID, physvar * sought)				*
 *      solve a single student equation for one variable in it. 	*
 *	returns NULL if can't. else returns expression that can be 	*
 *	substituted for sought elsewhere				*
 *  studsub(physvar *sought, expr * value, eqnID, destslot)		*
 *	substitute value for sought in student's slot eqnID		*
 ************************************************************************/
#include "decl.h"
#include "extoper.h"
#include "dbg.h"
#include "extstruct.h"
#include <math.h>
#include <float.h>
#include "binopfunctions.h"
using namespace std;

#define DBG(A) DBGF(NEWCKEQSOUT,A)
//  CHKEQS, NEWCKEQSOUT, CHKEQSDTL   might want to rethink these
#define DTL(A) DBGFM(CHKEQS,A)
#define DBGEQ(A) DBGF(CHKEQS,A)
#define NEWDTL(A) DBGFM(NEWCKEQSOUT,A)

int indyAddStudEq(int slot, const char* const equation);	// in indysgg

  // in this file
bool checkifdone(const varindx sought, binopexp * & ansexpr,
		 const vector<binopexp *> * const soleqs);

/************************************************************************
 * powersolve  tries to solve for variable at varindx sought in 	*
 *	canonvars. See top file for more description. See below for	*
 *	version which takes canonical name instead of varindx		*
 ************************************************************************/
string powersolve(const int howstrong, const varindx sought, 
		  const int destslot)
{
  int k, q;
  string answer;
  numpasses = 0;
  DBG( cout << "entering powersolve " << howstrong << ", " 
       << (*canonvars)[sought]->clipsname
       << ", " << destslot << endl;);
  vector<binopexp *> * eqn = new vector<binopexp *>;
  if (destslot >= STUDEQSZ) throw(string("destination slot ") 
				  + itostr(destslot) + " doesn't exist");
  // gather student equations in list eqn
  for (q = 0; q < HELPEQSZ; q++)
    if (studeqf[q] != (binopexp *)NULL) 
      eqn->push_back((binopexp *) copyexpr(studeqf[q]));
  DBG(cout << "solvetool built "<<eqn->size()<<" eqns = " << endl;
      for (q = 0; q < eqn->size();q++) 
      cout << "          " << (*eqn)[q]->getLisp(false) << endl);
  vector<varindx> *vars = new vector<varindx>;
  // get a list of vars appearing in those equations
  for (q = 0; q < eqn->size(); q++)
    numunknowns((*eqn)[q],*vars,false);
  DBG( cout << "and variable list with " << vars->size() << " variables: ";
       cout<< endl << "          ";
       for (q = 0; q < vars->size();q++) 
       cout << (*canonvars)[(*vars)[q]]->clipsname << ", ";
       cout << endl);

  // Add dummy values for all variables that are marked as parameters
  // I use random numbers in [0,1]
  for(q=0; q< vars->size(); q++)
    if( (*canonvars)[(*vars)[q]]->isparam ){
      eqn->push_back((binopexp *) 
		     new binopexp(&equals, new physvarptr((*vars)[q]),
				  new numvalexp((double) rand()/RAND_MAX)));
      DBG(cout << "add eqn for parameter " << 
	  (*canonvars)[(*vars)[q]]->clipsname<< endl);
    }
      
  // start the solution process. This will repeat if doagain winds up
  // positive
  vector<binopexp *> * partsols; // partially solved vars in purelin
  vector<binopexp *> * soleqs = new vector<binopexp *>;
  binopexp * ansexpr;		// will hold answer
  int doagain = 1;			// should we repeat
  expr * numer = (expr *)NULL;		// expr will be rhs of answer 
  while (doagain > 0) {
    DBG(cout << "about to recassign in solvetool" << endl;);
    recassign(eqn,vars,soleqs);
    DBG(cout << "After recassign, eqn = " << endl;
	for (q = 0; q < eqn->size();q++) 
	cout << "          " << (*eqn)[q]->getLisp(false)<< endl;
	cout << "          and soleqs =" << endl;
	for (q = 0; q < soleqs->size(); q++) 
	cout << "          " << (*soleqs)[q]->getLisp(false) << endl);
    if (checkifdone(sought, ansexpr, soleqs)) goto success;
    if (howstrong & 1) {
      DBG(cout << "About to try dopurelin" << endl;);
      partsols = dopurelin(eqn,vars,soleqs,doagain);
      if (checkifdone(sought, ansexpr, soleqs)) goto success;
      DBG(cout << "After dopurelin, soleqs =" << endl;
	  for (q = 0; q < soleqs->size();q++) 
	  cout << "          " << (*soleqs)[q]->getLisp(false) << endl;
	  cout << "          and partsols =" << endl;
	  for (q = 0; q < partsols->size(); q++) 
	  cout << "          " << (*partsols)[q]->getLisp(false) << endl);
    }
    else {
      doagain = 0;
      partsols = new vector<binopexp *>;
    }
    if (howstrong & 2) {
      if (dofactor(eqn,vars)) doagain = partsols->size() +1;  // force redo
      DBG(cout << "after dofactor, doagain is " << doagain << endl;);
      if (donlsolv(eqn)) doagain = partsols->size() +1; // force redo
      DBG(cout << "after donlsolv, doagain is " << doagain << endl;);
    }
    if (howstrong & 4) {
      if (polysolve(eqn,vars)) doagain = 1;
      DBG(cout << "after polysolve, doagain is " << doagain << endl;);
    }
    if (howstrong & 8) if (eqn->size() >= 2) {
      //      if (dotrig(eqn))  doagain = 1;
      int dotriggave = dotrig(eqn); // 8/9/02 JaS
      if (dotriggave == 2) doagain = 1;
      else if (dotriggave == 1) if (polysolve(eqn,vars)) doagain = 1; //8/9/02
      for (q = 0; q < eqn->size(); q++)
	if (fixupforpls((*eqn)[q])) {doagain = 1; break;}
      DBG(cout << "after dotrig, doagain is " << doagain << endl);
    }
    if ((doagain <= 0) && (howstrong & 16)) {
      if (desperate(eqn,vars)) doagain = 1;
      DBG(cout << "after desperate, doagain is " << doagain << endl);
    }
    // now, if we are going to try over, put partsols back into eqn list.
    if (doagain > 0) {
      for (q = 0; q < partsols->size(); q++) eqn->push_back((*partsols)[q]);
      while (partsols->size() > 0) partsols->pop_back();
    }
    // remove duplicate or proportional equations:
    DBG(cout << "solvetool about to remove duplicate equations" << endl);
    for (q = 0; q < eqn->size(); q++)
      {
	expr * eqexpr = (*eqn)[q];
	eqnumsimp(eqexpr,true);
	flatten(eqexpr);
	eqnumsimp(eqexpr,true);	// is this overkill?
	// this assumes eqexpr still points to same
	// place as (*eqn)[q]. Not obvious, but if 
	// wrong, better fix checkeqs as well
	numvalexp * answer = normexpr(eqexpr);	
	if(answer) answer->destroy(); //stop memory leak
      }
    for (k = 0; k+1 < eqn->size(); k++)
      for (q = k+1; q < eqn->size(); q++)
	{
	  numvalexp * factd=NULL;  // result not used
	  if (uptonum((*eqn)[k],(*eqn)[q],factd))
	    {
	      if(factd) factd->destroy();
	      (*eqn)[q]->destroy();
	      (*eqn)[q] = (*eqn)[eqn->size()-1];
	      eqn->pop_back();
	      q--;	    
	    }
	}
  } // end of while doagain > 0
  // check partsols for solution to student variable
  DBG(cout << "solvetool exited while doagain loop, checking partsols" 
      << endl);
#if 0 // AW: take out check for partial solutions
  if (partsols->size() > 0) 
    {
      for (q = 0; q < partsols->size(); q++)
	if (exprcontains((*partsols)[q],sought)) break;
      if (q == partsols->size()) { // failed to solve for variable at all
	DBG(cout << "powersolve failed" << endl);
	return(string(""));		// AW: this skips cleanup -- OK?
      }
      // partsols are only made by purelin, so must be linear
      expr * denom = (expr *)NULL;
      if (!linvarcoefs((*partsols)[q],sought,denom,numer))
	throw string("impossible got nonlinear in partsols in solvetool");
      if (denom->etype != numval) {
	DBG(cout << "in solvetool linvarcoefs bad coef: " << denom->getInfix()
	    << "with const term " << numer->getInfix() << endl;);
	throw string("impossible got nonnumerical denominator in solvetool");
      }
      ((numvalexp *)denom)->value = -1./((numvalexp *)denom)->value;
      denom->MKS *= -1.;
      numvalexp *multhis = (numvalexp *) denom;
      kmult(numer,multhis);
      eqnumsimp(numer,true);
      flatten(numer);
      goto partsuccess;
    } // end of if partsols not empty after doagain not >0
  else { answer = string(""); goto cleanup; }

 partsuccess:
  ansexpr = new binopexp(&equals,new physvarptr(sought),numer);
  DBG(cout << "gotten answer case partsuccess " << ansexpr->getInfix() << endl);

#else // AW: replace partsol handling code: just return failure
  answer = string(""); goto cleanup; 
#endif // replacement for partsol handling

 success: 

  // If the answer depends on a parameter, then the numerical
  // will not match the canonical numerical value.
  // Thus, we remove solutions whose numerical value does not
  // match the canonical numerical value.
  // in principle, this would use the equaleqs.cpp function.
  if(ansexpr->rhs->etype==numval){
    numvalexp *vallexp=(numvalexp *) ansexpr->rhs;
    if(fabs(vallexp->value-(*numsols)[sought]) >
       100* DBL_EPSILON * fabs(vallexp->value+(*numsols)[sought]))
      {
	DBG(cout << "numerical value  != canonical value "
	    << (*numsols)[sought] << endl);
	DBG(cout << "Assuming this indicates dependence on a parameter"<<endl);
	answer = string(""); goto cleanup;
      }
  }
  
  indyAddStudEq(destslot,ansexpr->getLisp(false).c_str());
  if (ansexpr->rhs->etype==numval) {
      answer = ansexpr->solprint(true);
      DBG(cout << "solvetool returning assignment string " << answer<< endl);
    }
  else answer = ansexpr->getLisp(true);
 cleanup:
  for (q = eqn->size(); q > 0; q--) (*eqn)[q-1]->destroy();
  for (q = soleqs->size(); q > 0; q--) (*soleqs)[q-1]->destroy();
  delete eqn;
  delete soleqs;
  delete vars;
  DBG(cout << "powersolve returning " << answer << endl;);
  return answer;
} // end of powersolve

/************************************************************************
 * checkifdone looks through the list soleqs to see if an equation for 	*
 *	the solution for sought is there. If so, it returns it as 	*
 *	a binopexp * and returns returns true. 				*
 ************************************************************************/
bool checkifdone(const varindx sought, binopexp * & ansexpr,
		 const vector<binopexp *> * const soleqs)
{
  int k;
  varindx thisvp;
  DBG(cout << "checkifdone called for " << (*canonvars)[sought]->clipsname
      << endl;);
  for (k = 0; k < soleqs->size(); k++) {
    if ((*soleqs)[k]->lhs->etype != physvart)
      throw(string("checkifdone called with soleqs not assignment on ") 
	    + itostr(k));
    thisvp = ((physvarptr *)(*soleqs)[k]->lhs)->varindex;
    if (sought == thisvp) { 
      ansexpr = (*soleqs)[k]; 
     DBG(cout << "checkifdone success, returning " 
	  << ansexpr->getInfix() << endl;);
      return(true); }
  }
  DBG(cout << "checkifdone failure, returning false" << endl;);
  return (false);
}


/************************************************************************
 * powersolve  tries to solve for variable with name varname	 	*
 *	canonvars. See top file for more description. See below for	*
 *	version which takes canonical name instead of varindx		*
 ************************************************************************/
string powersolve(const int howstrong, const string varname, 
		  const int destslot)
{
  for (int k = 0; k < canonvars->size(); k++)
    if ((*canonvars)[k]->clipsname == varname)
	return(powersolve(howstrong,k,destslot));
  return string("");
}

