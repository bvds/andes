// desperate (needs name change once we figure out what it does
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved
// Modifications by Brett van de Sande, 2005-2008
//
//  This file is part of the Andes Solver.
//
//  The Andes Solver is free software: you can redistribute it and/or modify
//  it under the terms of the GNU Lesser General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  The Andes Solver is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU Lesser General Public License for more details.
//
//  You should have received a copy of the GNU Lesser General Public License
//  along with the Andes Solver.  If not, see <http://www.gnu.org/licenses/>.

#include "decl.h"
#include <algorithm>
#include "extoper.h"
#include "dbg.h"
#include "extstruct.h"
using namespace std;

int solvetwoquads(binopexp * & ,  binopexp * & , 
		  const varindx , const varindx );
int twoonevareqs(binopexp * & eq1,  binopexp * & eq2, const varindx v);

#define DBG(A) DBGF(NEWCKEQSOUT,A)
#define DBGM(A) DBGFM(NEWCKEQSOUT,A)

// desperate makes several attempts to make further progress in solving
// the equations in eqn which are expressed in terms of the vars in 
// vars. These are expected to be much reduced from the original set,
// and desperate should only be called after simpler procedures, like
// using all assignment statements, solving simultaneous linear equations,
// have been tried. It is also most useful after solvetrig, as it only
// looks at equations which are polynomials in their variables.
//
// First it attempts to find "the simplest" equation linear in a variable, and
//   solve it. It then tries to substitute this into each of the other 
//   equations, rationalize, and if this is now a polynomial in just one
//   variable, solve it. If the solution is unique (given known constraints),
//   it replaces the equation by an assignment statement.
// If this process has a success, it returns true. 
//
//  [NOTE: Is there a good reason to include what follows in this 
//     function rather than making it a separate one? I don't see it
//     right now.]
//
// Second, it compiles a list of equations overall quadratic and involving
//   no more than three unknowns. It looks for pairs of equations involving
//   only the same two unknowns, and if they are independent, solves them,
//   and returns true. The three variable equations are currently ignored.

bool desperate(vector<binopexp *> * eqn, vector<varindx> * & vars)
{
  vector<expr *> * lininthis = new vector<expr *>(vars->size(),NULL);
  vector<int> * besthardness = new vector<int>(vars->size(),100000);
  vector<int> * ordsinthis = NULL;
  expr * coef = NULL;
  expr * numer = NULL;
  int thishardness;
  int k, q; 
  int doagain = -1;

#if WITHDBG
  unsigned long thisdbg = ++dbgnum;	// recursive calls for debug
#endif
  DBG(cout << "desperate " << thisdbg << " called for " 
      << eqn->size() << " equations:" << endl;
       for (k=0; k < eqn->size(); k++) 
	 cout << (*eqn)[k]->getInfix() <<endl);
  
  for (k = 0; k < eqn->size(); k++)
    {
      if (!ordinvars((*eqn)[k],vars,ordsinthis)) continue;
      for (q = 0; q < vars->size(); q++)
	{
	  if ((*ordsinthis)[q] != 1) continue;
	  if (!linvarcoefs((*eqn)[k],(*vars)[q],coef,numer)) continue;
	  thishardness = 3 * ordunknowns(coef, false) 
	    + ordunknowns(numer, false);
	  if (((*lininthis)[q] == (expr *)NULL) ||
	      ((*besthardness)[q] > thishardness))
	    {
	      (*lininthis)[q] = (*eqn)[k];
	      (*besthardness)[q] = thishardness;
	    }
	  coef->destroy(); coef = NULL;  // didn't help
	  numer->destroy(); numer = NULL;
	} // end of one var from one equation
      delete ordsinthis;
      ordsinthis = NULL;	// this one did help
      
    } // end of loop over equations looking for linear vars
  thishardness = 100000;
  int bestq = -1;
  
  for (q = 0; q < vars->size(); q++)
    if ((*besthardness)[q] < thishardness) {
      thishardness = (*besthardness)[q];
      bestq = q;
    }
  DBGM(
       cout << "In linvarcoefs block in desperate, starting with "
       << vars->size() << " variables " << endl;
       for (k=0; k < vars->size(); k++) cout 
	 << (*canonvars)[(*vars)[k]]->clipsname <<endl;
       cout << "in " << eqn->size() << " equations:" << endl; 
       for (k=0; k < eqn->size(); k++) 
	 cout << (*eqn)[k]->getInfix() <<endl;
       for (q = 0; q < vars->size(); q++)
	 if ((*lininthis)[q] != NULL)
	   cout << (*canonvars)[(*vars)[q]]->clipsname << " has hardness " 
		<< (*besthardness)[q] << " in equation" << endl
		<< (*lininthis)[q]->getInfix() << endl;
       );
  
  if (bestq >= 0)
    {
      linvarcoefs((*lininthis)[bestq],(*vars)[bestq],coef,numer);
      kmult(numer,-1.);
      binopexp * neweq = new binopexp(&equals,
				      new physvarptr((*vars)[bestq]),
				      new binopexp(&divby,numer,coef));
      expr * trythis = NULL;
      
      for (k = 0; k < eqn->size(); k++)
	if ((*eqn)[k] != (*lininthis)[bestq])
	  {
	    trythis = copyexpr((*eqn)[k]);
	    DBGM( cout << "Substituting " << neweq->getInfix() << " into "
		      << trythis->getInfix() << endl; ) ;
 	    subexpin(trythis,neweq);
	    flatten(trythis);	 // added 8/14/02 JaS
	    eqnumsimp(trythis,false); // added 8/14/02 JaS. false is a guess
	    DBGM( cout << "Substitution gave " << trythis->getInfix() << endl);
	    int pv = -1;
	    if (trythis->etype != binop) 
	      throw(string("Desperate: subexpin killed binop"));
	    binopexp * trybin = (binopexp *) trythis;

	    if (hasjustonevar(trythis,pv) && 
		(pv >= 0) &&
		rationalize(trybin))
	      {
		DBGM( cout << "Desperate: successful replacing " 
		          << neweq->getInfix() << " leaving "
		          << trybin->getInfix() << endl;);
		vector<double> * coefs = NULL; 		// Is this code 
		if (polyexpand(trybin->lhs,pv,coefs))   // duplicating what is
		  {					// already in polyslv
		    DBGM(				// and polysolve?
		      {
			cout << "polyexpand gives ";
			for (q = ((int)coefs->size())-1; q >= 0; q--)
			  cout << (*coefs)[q] << "*x^" << q << " +";
			cout << "0"<< endl;
		      } ) ;
		    vector<double> * roots = findallroots(coefs);
		    DBGM( cout << "findallroots found " << roots->size() 
			       << endl; );
		    for (q=0; q < roots->size(); q++)
		      {
			if ((((*roots)[q]<0) && (*canonvars)[pv]->isnonneg)
			    || (((*roots)[q] == 0) && 
				(*canonvars)[pv]->isnonneg &&
				(*canonvars)[pv]->isnonzero  ))
			  {
			    (*roots)[q]=(*roots)[roots->size()-1];
			    roots->pop_back();
			    q--;
			  }
		      }
		    DBGM( cout << "After discarding wrong ones, have "
			      << roots->size() << endl; );
		    
		    if (roots->size() == 1)
		      {
			(*eqn)[k]->destroy();
			(*eqn)[k] = new binopexp(&equals,
						 new physvarptr(pv),
						 new numvalexp((*roots)[0]));
			doagain = 1;
		      }
		  } // end of if polyexpand
		else 
		    DBGM({ cout << "polyexpand unsuccessful expanding" << endl;
			   cout << trythis->getInfix() << " in variable "
				<< (*canonvars)[pv]->clipsname << endl; } );
	      } // end of ifhasjustonevar ...
	    else DBGM( cout << "failed hasjust... test" << endl; );
	    trythis->destroy();
	  } // end of this equation, k, is not the one I linvarcoefed
    } // end of if there was a linear variable found (bestq > 0)
  delete lininthis;
  delete besthardness;
  if (doagain >= 0){
    DBG(cout << "desperate " << thisdbg << " returning true, doagain="
	<< doagain << endl);
    return(true);
  }
  // end of block to solve eq linear in one var with nonconst coefs.
  // If unsuccessful, try to find two equations quadradic in the same 
  // two unknowns

  // because of the kludgy format used to store simpeq info, we are 
  // limited to 2^10 - 2 equations and 2^7 - 2 vars.
  if ((eqn->size() > 1022) || (vars->size() > 126)){
    DBG(cout << "desperate " << thisdbg << " returning false, doagain="
	<< doagain << endl);
    return(false);
  }
  vector<int>  simpeqs;  
  int thissimpeq;
  for (k = 0; k < eqn->size(); k++) {
    if (ordunknowns((*eqn)[k], false) !=2) continue; // only looking at quadrcs
    if (!ordinvars((*eqn)[k],vars,ordsinthis)) continue;  // shouldn't happen
    thissimpeq = 0;
    for (q = 0; q < vars->size(); q++)
      if ((*ordsinthis)[q] > 0)		// had thought all must be 2, but 
	{				// forgot first part doesn't always go.
	  if (thissimpeq < 16384) // 2^14, enough for two vars stored
	    thissimpeq = (thissimpeq << 7) | (q+1);
	  else { thissimpeq = 0; break; } // can't handle more than 3 vars
	}
    if (thissimpeq != 0) simpeqs.push_back((thissimpeq << 10) | (k+1));
    delete ordsinthis;
    ordsinthis = NULL;
  } // end of k loop over equations.
  if (simpeqs.size() < 2) { 
    DBG(cout << "desperate " << thisdbg << " returning false, doagain="
	<< doagain << endl);
    return(false); 
  }
  DBGM( { cout << "Here is simpeqs, together and by part" << endl;
	 for (k = 0; k < simpeqs.size(); k++) {
	   cout << simpeqs[k] << ": " << (simpeqs[k]>>17) 
		<< ", " << (int) ((simpeqs[k]>>10) & 0x7f) << ", " 
		<< (int) (simpeqs[k] & 0x3ff) << endl; } } ) ;
  sort(simpeqs.begin(),simpeqs.end());
  DBGM( { cout << "simpeqs after sort, together and by part" << endl;
	 for (k = 0; k < simpeqs.size(); k++) {
	   cout << simpeqs[k] << ": " << (simpeqs[k]>>17) 
		<< ", " << (int) ((simpeqs[k]>>10) & 0x7f) << ", " 
		<< ((int) (simpeqs[k] & 0x3ff)) << endl; } } ) ;
  int lastonevar = -2;		// last equation with just one var. maybe -1
  for (k = 0; k+1 < simpeqs.size(); k++) // changed -2 -> -1, 3/5/01, side 3/7
    {
      if ((thissimpeq = simpeqs[k]) < 131072) // 2 ^ 17, one vars + eqn
	{
	  if (((thissimpeq ^ simpeqs[k+1]) & 0xfffc00) == 0) // same vars
	    switch(twoonevareqs((*eqn)[(thissimpeq & 0x3ff)-1],	// eq1
			     (*eqn)[(simpeqs[k+1] & 0x3ff)-1],	// eq2
			     (*vars)[((thissimpeq>>10) & 0x7f) -1]))	// var
	      {
	      case 0:		// Equations inconsistent
	      case 1:		// Equations ok, but didn't solve all. 
		continue;
	      case 2:		// an equation replaced by assignment
		DBG(cout << "desperate " << thisdbg 
		    << " returning true, doagain="
		    << doagain << endl);
		return(true);
	      }
	  continue;
	} // end of has justonevar
    if (lastonevar == -2) lastonevar = k-1;
    if ((thissimpeq = simpeqs[k]) >= 16777216) // 2 ^ 24, two vars + eqn
      { break; }
    if (((thissimpeq ^ simpeqs[k+1]) & 0xfffc00) == 0) // same two vars
      {
	DBG( cout << "calling solvetwoquads in variables vars number "
	     << (int)((thissimpeq>>17) -1) << " and "
	     << (int)(((thissimpeq>>10) & 0x7f) -1) << endl);
	switch(solvetwoquads((*eqn)[(thissimpeq & 0x3ff)-1], 		// eq1
			     (*eqn)[(simpeqs[k+1] & 0x3ff)-1],	// eq2
			     (*vars)[(thissimpeq>>17) -1],		// v1
			     (*vars)[((thissimpeq>>10) & 0x7f) -1]))	// v2
	{
	case 4:			// first equation gives full solution one var.
	case 3:			// first eq quadratic in one var, second linear
	  DBG(cout << "desperate " << thisdbg << " returning true" << endl);
	  return(true);
	case 2:			
	  {
	    // both equations used, and both vars partially solved.
	    // but this is only progress if it leads to a solution
	    bool polyresult=polysolve(eqn); 
	    DBG(cout << "desperate " << thisdbg << " returning " 
		<< polyresult << endl);
	    return(polyresult);
	  }		
	case 1:			// equations not independent. First made taut.
	  continue;		//   (what if inconsistent?)
	case 0:			// equations didn't live up to promise
	default:
	  throw(string("solvetwoquads had unexpected problem"));
	} // end of switch on success of solvetwoquads
      } // end of if neighbors have same two vars
    } // end of k loop on simpeqs list of equations.
  DBG(cout << "Returning from desperate " << thisdbg << " with doagain = " 
      << doagain << " after looking at " << lastonevar +1 
      << " eqs in one var" << endl);
  return(doagain>=0);
}
