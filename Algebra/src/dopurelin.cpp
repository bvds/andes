// dopurelin	part of process to try solving simultaneous linear eqs.
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved

/************************************************************************
 *  dopurelin    extracts purely linear equations from the list eqn	*
 *	and solves for as many of their variables as possible. Writes	*
 *	out to file any fully solved variables, and returns a vector	*
 *	of partially solved variables as equations. Removes all the 	*
 *	linear equations from eqn.					*  
 *   Note coefs are required to be pure numvals, not known expr's	*
 *  vector<binopexp *> * dopurelin(					*
 *		vector<binopexp *> * & eqn, 				*
 *		vector<varindx> * & vars,   (index into canonvars)	*
 *		vector<binopexp *> * soleqs, int & doagain)		*
 ************************************************************************/

#include "decl.h"
#include "extoper.h"
#include "dbg.h"
#include "extstruct.h"
using namespace std;

#define DBG(A) DBGF(NEWCKEQSOUT,A)
//  CHKEQS, NEWCKEQSOUT, CHKEQSDTL   might want to rethink these
#define DTL(A) DBGFM(CHKEQS,A)
#define DBGEQ(A) DBGF(CHKEQS,A)
#define NEWDTL(A) DBGFM(NEWCKEQSOUT,A)

  // TRY PURELINSOLV
  // We still have unsolved equations. Try to get ready for and use
  // purelinsolv
  // First we extract the equations which are linear, taking these off
  //  the list eqn of equations and placing them on another, trylineqs
  // We extract a list linvars of the variables which appear in trylineqs
  // The linvars are reordered so that we prefer expressing the earlier ones
  //   in terms of the later ones if we can't find numerical solutions. This
  //   order is (currently)
  // 	  variables not qualifying as below
  //	  variables known to be nonnegative
  //	  variables which are parameters (I assume all parameters are nonneg)
  //   The solutions will be placed in sols
  // Now use purelinsolv to solve the equations (DO PURELINSOLV)
  // The solutions, both complete and partial, are substituted into
  //   the remaining equations not including in the solution of the 
  //   simultaneous linear ones.
  // Those variables which were completely solved by purelinsolv, as a
  //   number rather than a linear function of other variables, have their
  //   assignments written out and they are removed from the variable list.
  //   The other solutions are placed on partsols.
  // Now the nonlinear equations are checked for triviality and discarded
  //   if they have no variables in them. If they are linear, doagain is
  //   incremented (not conclusively??)
  // This ends at END OF PURELINSOLV

vector<binopexp *> * dopurelin(vector<binopexp *> * eqn, 
			       vector<varindx> * & vars, 
			       vector<binopexp *> * soleqs, int & doagain )
{
  int k, q;
#if WITHDBG
  unsigned long thisdbg = ++dbgnum;	// recursive calls for debug
#endif
  DBGEQ( cout << "dopurelin call " << thisdbg << ": Still have " 
	 << eqn->size() 
	      << " equations after rec assign" << endl);
  vector<binopexp *> * trylineqs = new vector<binopexp *>;
  for (q=0; q < eqn->size(); )
    {
      DBGEQ( cout << "Eq " << q << ": " << (*eqn)[q]->getInfix() << endl;);
      if (fixupforpls((*eqn)[q])) 
	{
	  DBGEQ( cout << "On eq "<< q <<" fixupforpls returned true" << endl;);
	  trylineqs->push_back((*eqn)[q]);
	  DBGEQ( cout << "Copied to trylineqs, now trylineqs has " << 
		      trylineqs->size() << " and eqn has " << eqn->size()
		      << " equations" << endl;);
	  for (k = q+1; k < eqn->size(); k++)
	    (*eqn)[k-1]= (*eqn)[k];
	  eqn->pop_back();
	  DBGEQ( cout << "Moved up remaining eqs in eqn, now " << eqn->size()
		      << " equations" << endl;);
	}
      else
	{
	  DBGEQ( cout << "On eq " << q << " fixupforpls returned false" 
		      << endl;);
	  q++;
	}
    }

  DBGEQ( cout << "After fixupforpls, we have " << trylineqs->size() 
	      << " equations for purelinsolv and " << eqn->size() 
	      << " equations left over" << endl; );

  vector<binopexp *> *partsols = new vector<binopexp *>;
  doagain = 0;
  if (trylineqs->size() > 0) 
    {
      // Now we need to make variable list for purelinsolv containing only
      // the variables occurring in trylineqs
      vector<varindx> *linvars = new vector<varindx>;
      for (k=0; k < trylineqs->size(); k++) 
	{
	  expr * eqexpr = (*trylineqs)[k];
	  flatten(eqexpr);
	  numunknowns(eqexpr,*linvars,false);
	  if (eqexpr->etype != binop) throw(string(
 	    "flatten gives a non-binop equation!"));
	  (*trylineqs)[k] = (binopexp *)eqexpr;
	}

      // Put the linvars in an order which places ones with unknown
      // sign before all those known to be nonnegative
      DTL( cout << "About to sort linvars" << endl; );
      for (k=0,q=((int)linvars->size())-1;	// k is first one not examined
	   k<q;)			// q is last one not examined
	if (((*canonvars)[(*linvars)[k]])->isnonneg)
	  {
	    int temp = (*linvars)[k];	 // This needs improving, in
	    (*linvars)[k] = (*linvars)[q];	 // particular parameters last
	    (*linvars)[q] = temp;
	    q--;
	  }
	else  k++;
      DTL( { cout << "Did is/not nonneg sort" << endl;
             cout << "leaving k =" << k << " of " << linvars->size() << endl;
	     for (q=k; q < linvars->size(); q++) 
	       cout << q << ": " << (*canonvars)[(*linvars)[q]]->clipsname 
		    << endl;
	     for (q=k; q < linvars->size(); q++) 
	       cout << q << ": nonneg: " <<
	         (((*canonvars)[(*linvars)[q]]->isnonneg) ? "true" : "false")
		    <<", nonzero: " <<
	         (((*canonvars)[(*linvars)[q]]->isnonzero) ? "true": "false")
		    <<", isparam: " <<
	         (((*canonvars)[(*linvars)[q]]->isparam) ? "true" : "false")
		    << endl; } );
      // now put parameters and algebraic givens
      // (always nonnegative, I hope) after others
      if (!((*canonvars)[(*linvars)[k]])->isnonneg) k++;
      for (q=((int)linvars->size())-1;k<q;)	// k is first one not examined
	if ( (*canonvars)[(*linvars)[k]]->isparam ||
	    (*canonvars)[(*linvars)[k]]->keepalgebraic )
	  {
	    DTL( cout << "linvars "<<k<<" isparam, exchanging with "<< q
		 << endl; );
	    int temp = (*linvars)[k];
	    (*linvars)[k] = (*linvars)[q];
	    (*linvars)[q] = temp;
	    q--;
	  }
	else  k++;
      DTL( cout << "Sorted linvars" << endl; );
      // Now ready to try to solve the linear equations
      DBGEQ(			
	{			
	  cout << "about to try purelin on " << trylineqs->size()
	       << " equations in these " << linvars->size() << " variables:"
	       << endl;
	  for (k = 0; k < linvars->size(); k++)
	    cout << (*canonvars)[(*linvars)[k]]->clipsname << endl;
	  cout << "The equations are:" << endl;
	  for (k = 0; k < trylineqs->size(); k++)
	    cout << (*trylineqs)[k]->getInfix() << endl;
	  cout << "The equations not tried are :" << endl; 
	  for (k = 0; k < eqn->size(); k++)
	    cout << (*eqn)[k]->getInfix() << endl;
	});
      vector<binopexp *> *sols = new vector<binopexp *>;
      // DO PURELINSOLV
      if (!purelinsolv(trylineqs,linvars,sols))
	{
	  cerr << "purelinsolv failed" << endl;
	  cout << "purelinsolv failed" << endl;
	  //	  solfile << "purelinsolv failed, leaving" << endl;
	  for (k = 0; k < trylineqs->size(); k++)
	    eqn->push_back((*trylineqs)[k]);
	  throw(string("Purelinsolv found inconsistent equations"));
	}
      DBG( cout << "purelinsolv reported success on "
	        << sols->size() << " of " << linvars->size() 
	        << " variables" << endl;);
      DBGEQ( { cout << "Purelinsolv: The solutions found are :" << endl;
	       for (k = 0; k < sols->size(); k++) 
		 cout << (*sols)[k]->getInfix()<<endl; } );

      // substitute in for solved variables in remaining eqs?
      for (k = 0; k < sols->size(); k++)
	for (q = 0; q < eqn->size(); q++)
	  {
	    expr * eqexpr = (*eqn)[q];
	    subexpin(eqexpr,(*sols)[k]);
	    eqnumsimp(eqexpr,true);
	    while(flatten(eqexpr)); // while added 2/11
	    if (eqexpr->etype != binop) throw(string(
  	        "subexpin/eqnumsimp/flatten gives a non-binop equation!"));
	    (*eqn)[q] = (binopexp *)eqexpr;
	  }
      DBG( { if (eqn->size() > 0) 
	       {
		 cout << "After purelinsolv/subexpin/eqnumsimp,"
		      << "other equations are" << endl;
		 for (q = 0; q < eqn->size(); q++) 
		   cout << (*eqn)[q]->getInfix() << endl; } } );
      // remove solved-for variables from remaining variable list
      doagain = 0;
      for (k = 0; k < sols->size(); k++)
	{
	  if ((*sols)[k]->rhs->etype == numval)
	    {
	      doagain = sols->size(); // large enough to stay positive
	      for (q = 0; q < vars->size(); q++)
		if (( (*sols)[k]->lhs->etype == physvart) &&
		    ( ((physvarptr *)((*sols)[k]->lhs))
		      ->varindex  == (*vars)[q]))
		  {
		    (*vars)[q]=(*vars)[vars->size()-1];
		    vars->pop_back();
		    break;
		  }
	    }
	}
      DBG( cout << "Checkeqs: after rm solved-for, doagain = " 
		<< doagain << endl;);
      // write out those fully solved, and save partially solved
      for (; sols->size() > 0;)
	{
	  if ((*sols)[0]->rhs->etype == numval)
	    {
	      expr *thiseq = (*sols)[0];
	      expr *troub = dimenchk(true,thiseq);
	      DBG( if (troub != (expr *) NULL) 
		   cout << "Dimenchk before write-out had trouble with "
		   << troub->getInfix() << endl;);
	      // was   solfile << ((binopexp *)thiseq)->solprint(false)<< endl;
	      soleqs->push_back((binopexp *)thiseq);
	    }
	  else 
	    {
	      partsols->push_back((*sols)[0]);
	      doagain--;	// compensate for adding 1 later for linear eq
	    }
	  (*sols)[0] = (*sols)[sols->size()-1];
	  sols->pop_back();
	}
      delete sols;
      DBG( { cout << "Checkeqs " << thisdbg 
		  << ": after writing out fully solved, " 
		  << "and saving partially solved, doagain = "
		  << doagain << endl;
	     cout << "The partially solved variable equations are:" << endl;
	     for (k = 0; k < partsols->size(); k++)
	       cout << (*partsols)[k]->getInfix() << endl; } );
      if (eqn->size() > 0) {
	// check that remaining equations are still
	//    nontautological   (what about nonlinear?)
	DBGEQ( cout << "CHKEQS: simplifying after purelin +subst" << endl;);
	for (k = 0; k < eqn->size(); k++)
	  {
	    expr * eqexpr = (*eqn)[k];
	    eqnumsimp(eqexpr,true);
	    if (ordunknowns(eqexpr,false) == 0) // (ignore inconsistencies)
	      {					// might rethink knowns
		eqexpr->destroy();
		(*eqn)[k] = (*eqn)[eqn->size()-1];
		eqn->pop_back();
		k--;
		continue;
	      }
	    if ((k < eqn->size()) && 
		(ordunknowns(eqexpr,false) == 1)) // might rethink knowns
	      {
		DBGEQ( cout << "doagain set on eq " << k << endl;);
		doagain++;
	      }
	    if (eqexpr->etype != binop) throw(string(
		     "eqnumsimp gives a non-binop equation!"));
	    (*eqn)[k] = (binopexp *)eqexpr;
	  }
	DBGEQ(cout << "After purelinsolv and substitution and purging," << endl
	      << "We have left " << vars->size()
	     << " unsolved variables, " << partsols->size()
	     << " partially solved variables, and " << eqn->size()
	      << " unused equations, and doagain = "<< doagain << endl);
      } // end of if eqn->size > 0
      delete linvars;
    } // end of if trylineqs->size() > 0
  delete trylineqs;
  return(partsols);
}
