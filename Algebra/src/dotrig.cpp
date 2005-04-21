// dotrig calls solvetrig and then, if unsuccessful, undotrigvar
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved
#include "decl.h"
#include "extoper.h"
#include "dbg.h"
#include "extstruct.h"
using namespace std;

#define DBG(A) DBGF(NEWCKEQSOUT,A)

/* Comment 8/9/02 JaS - note - its been a long time since I looked at
     most of this!
   Changed 8/9/02 to return an int
       2: if solvetrig worked, solving for an angle  (had returned true)
       1: if undotrigvar worked, adding one or more eqs (had ret false)
       0: neither worked
*/
int dotrig(vector<binopexp *> * & eqn)
{
  int k;
  int answer = 0;
  for (k = 0; k < eqn->size(); k++) // get rid of rhs for trigsolve.
    {
      expr * eqexpr = (*eqn)[k];
      normexpr(eqexpr);
      if (eqexpr->etype != binop) 
	throw(string("normexpr gives a non-binop equation!"));
      (*eqn)[k] = (binopexp *)eqexpr;
    }
  VEQCHK(eqn);
  DBG( { cout << "Just after normexpr and before solvetrig we have " 
	      << eqn->size() << " equations left, namely" << endl;
  for (k = 0; k < eqn->size(); k++) 
    cout << (*eqn)[k]->getInfix() << endl; } );
  vector<expr *> *trigvars = new vector<expr *>;
  maketrigvars(eqn,trigvars);
  DBG( { cout << "found " << trigvars->size() 
	      << " angle vars to solve for" << endl;
  for (k = 0; k < trigvars->size(); k++)
    cout << (*trigvars)[k]->getInfix() << endl;
  cout << "\t in the " << eqn->size() << " equations:" << endl;
  for (k = 0; k < eqn->size(); k++) 
    cout << (*eqn)[k]->getInfix() << endl; } );
  VEQCHK(eqn);
  for (k=0;k < trigvars->size(); k++)
    {
      DBG( cout << "about to try solvetrigvar for " 
	   << (*trigvars)[k]->getInfix() << endl; );
      if (solvetrigvar((*trigvars)[k],eqn)) 
	{
	  DBG( {
		cout << "Checkeqs: solvetrigvar returned true on " << endl;
		cout << (*trigvars)[k]->getInfix() << endl; } );
	  answer = 2;
	  DBG( cout << "Checkeqs: found solution for "
	       << (*trigvars)[k]->getInfix() << endl
	       << "So forcing do again." << endl; );
	}
      else DBG( cout << "false returned by solvetrigvar for " 
		<< (*trigvars)[k]->getInfix() << endl; );
    }
  DBG(cout << "After calling solvetrig we have" << endl;
      cout << eqn->size() << " equations left:" << endl;
      for (k = 0; k < eqn->size(); k++) 
      cout << "          " << (*eqn)[k]->getInfix() << endl);
  DBG( cout << "gotsomething  = " << 
       ((answer==2) ? "true" : "false") << endl);
  VEQCHK(eqn);
  
  if (answer==0) for (k=0;k < trigvars->size(); k++)
    {
      if (undotrigvar((*trigvars)[k],eqn)) 
	{
	  DBG( {
		cout << "Checkeqs: undotrigvar returned true on " << endl;
		cout << (*trigvars)[k]->getInfix() << endl; } );
	  answer = 1;	// 8/9/02 JaS
	  //	      doagain = partsols->size() +1; // force redo
	  DBG( cout << "Checkeqs: found equation from trig arg "
	       << (*trigvars)[k]->getInfix() << endl; );
	}
      else DBG( cout << "false returned by undotrigvar for " 
		<< (*trigvars)[k]->getInfix() << endl; );
    }
  DBG(cout << "After undotrigvar = " << answer << ", we have" << endl;
      cout << eqn->size() << " equations left:" << endl;
      for (k = 0; k < eqn->size(); k++) 
      cout << "          " << (*eqn)[k]->getInfix() << endl);
  VEQCHK(eqn);
  return(answer);
}
