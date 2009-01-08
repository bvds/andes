// cleanup	cleans up an n_op
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

// Note: this should replace part of flatten, and might be used instead
// of flatten when only this is needed
/************************************************************************
 *  int cleanup(n_opexp * & a)						*
 *	Checks that at most one term is a numvar, if so it is the 	*
 *	first, and that there is no subterm of same plus or mult	*
 *  returns the number (0 or 1) of numvars, and fixed up a		*
 *  NOTE: n_op returned may have 0 or one args				*
 *	so may need to do some fixing up after if appropriate		*
 *  NOTE: only looks at upper levels for repeating n_ops, so does not	*
 *	clean up lower hidden nodes					*
 ************************************************************************/

#include <string>
#include "decl.h"
#include "extoper.h"
using namespace std;
#include "dbg.h"
#define DBG(A) DBGF(CLEANUP,A)

int cleanup(n_opexp * & a)
{
  int k,q;

  bool needagain = true;

  DBG( { cout << "entering cleanup with " << endl; a->dbgprint(4); });

  // search for plus terms in a plus or mult factors in a mult, and 
  // bring them up to top level
  while (needagain) 
    {
      needagain = false;	// need to redo search for + + or * * ?
      for (k=0; k < a->args->size(); k++) 		// if a has a daughter
	if ( ((*a->args)[k]->etype == n_op) && 		// k of the same kind
	     (((n_opexp *)(*a->args)[k])->op->opty 	// of n_op as a, 
	      == a->op->opty))						// 
	  {								// 
	    DBG( {							// 
	      cout << "bringing up term " << k << " which is" << endl;  // 
	      (*a->args)[k]->dbgprint(4); } );				// 
	    n_opexp *temp = ((n_opexp *)(*a->args)[k]);		// remove 
	    for (q = k+1; q < a->args->size(); q++) 	// daugther k and 
	      (*a->args)[q-1] = (*a->args)[q];		// add her children
	    a->args->pop_back();			// at the end of 
	    DBG( {					// of a's arg list.
	      cout << "after delete term " << k 
		   << " but before adding it" << endl;
	      a->dbgprint(4); } );
	    for (q=0; q < temp->args->size(); q++)
	      {
		if (((*temp->args)[q]->etype == n_op) &&
		    (((n_opexp *)(*temp->args)[q])->op->opty ==temp->op->opty))
		  needagain = true;
		a->args->push_back((*temp->args)[q]);	// not addarg-see below
	      }			// if one of the daughter's children is
	    DBG( { cout << "after adding term " << k  << endl; // also of the 
	           a->dbgprint(4); } ) ; 		// same type of n_op,
	    // don't destroy because contents are back in a.
	    delete temp->args;  
	    delete temp;				// need to repeat while
	    k--;
	  } 			// didn't use addarg above because overall dim
    } // end of while needagain                           should not change.
  // at this point no subterm is an n_op of same type.
  // Now check for numvals    SHOULD CHANGE TO DO MORE POWERFUL SIMPLIFICATION
  bool nonumyet = true;		// need to move numval to zero if found
  DBG( {     cout << "now check for numvals"  << endl; } );
  for (k=0; k < a->args->size(); k++)
    if ((*a->args)[k]->etype == numval)
      {
	DBG( cout << "found numval term " << k  << endl; );
	if (nonumyet)
	  {
	    nonumyet = false;
	    if (k != 0)
	      {
		expr * temp = (*a->args)[0];
		(*a->args)[0] = (*a->args)[k];
		(*a->args)[k] = temp;
	      }
	    DBG( { cout << "after finding first numval at term " << k  << endl;
	           a->dbgprint(4); } );
	  }
	else
	  {
	    if (a->op->opty == multe) { 
	      ((numvalexp *)(*a->args)[0])->value *= 
		((numvalexp *)(*a->args)[k])->value;
	      (*a->args)[0]->MKS += (*a->args)[k]->MKS;
	    }
	    else {		// plus
	      ((numvalexp *)(*a->args)[0])->value =
		addnum(((numvalexp *)(*a->args)[0])->value,
		((numvalexp *)(*a->args)[k])->value);
	      if ((*a->args)[k]->MKS.unknp())
		(*a->args)[k]->MKS = (*a->args)[0]->MKS;
	      if ((*a->args)[0]->MKS.unknp())
		(*a->args)[0]->MKS = (*a->args)[k]->MKS;
	      if (!((*a->args)[k]->MKS ==(*a->args)[0]->MKS))
		throw(string("cleanup tried to add terms of different dims"));
	    }
	    (*a->args)[k]->destroy();
	    for (q=k+1; q < a->args->size(); q++)
	      (*a->args)[q-1] = (*a->args)[q];
	    a->args->pop_back();
	    DBG( {
	      cout << "after finding another numval at term " << k  << endl;
	      a->dbgprint(4); } ) ;
	  }
      }
  if (nonumyet) 
    {
      DBG({ cout << "returning 0 from cleanup with " << endl;a->dbgprint(4);});
      return (0);
    }
  if (((a->op->opty == multe) && 
       (((numvalexp *)(*a->args)[0])->value == 1) &&
       ((*a->args)[0]->MKS.zerop())) ||
      ((a->op->opty == pluse) && (((numvalexp *)(*a->args)[0])->value == 0)) )
    {
      DBG( { cout << "about to destroy only numval" << endl;
	     a->dbgprint(4); } );
      (*a->args)[0]->destroy();
      for (q=1; q < a->args->size(); q++)
	(*a->args)[q-1] = (*a->args)[q];
      a->args->pop_back();
      DBG( {cout << "returning 0 from cleanup with " << endl;a->dbgprint(4);});
      return (0);
    }
  else 
    {
      if ((a->op->opty == multe) && (((numvalexp *)(*a->args)[0])->value == 0))
	{
	  for (q = ((int)a->args->size())-1; q > 0; q--)
	    { 
          // AW: rc3b bug, 2/19/04: when solving for var, cleanup of rhs (0 V)*C1 gave (0 V)
		  // leading to unit error later. Need to adjust units on resulting 0 value
		  // when dropping factors. dimens::+ op should handle unknown units correctly.z 
		  (*a->args)[0]->MKS += (*a->args)[q]->MKS;     // added -AW
	      (*a->args)[q]->destroy();
	      a->args->pop_back();
	    }
	  DBG( { cout << "returning 0 from cleanup with " << endl;
	         a->dbgprint(4); } );
	  return(0);
	}
      DBG( { cout << "returning 1 from cleanup with " << endl;
             a->dbgprint(4); });
      return(1);
    }
}
/************************************************************************
 *  bool isclean(const n_opexp * a)					*
 * 	returns true if cleanup would have left unchanged, else false	*
 ************************************************************************/
bool isclean(const n_opexp * a)
{
  int k;
  for (k=0; k < a->args->size(); k++)
    if ( ((*a->args)[k]->etype == n_op) &&
	 (((n_opexp *)(*a->args)[k])->op->opty == a->op->opty))
      return(false);
  // at this point no subterm is an n_op of same type. Check for numvals
  bool foundone=false;
  for (k=0; k < a->args->size(); k++)
    if ((*a->args)[k]->etype == numval)
      {
	if (foundone) return(false);
	else foundone = true;
      }
  if ( (a->args->size() > 0) 
       && (*a->args)[0]->etype == numval
       && ( ( (a->op->opty == multe) 
	      && (((numvalexp *)(*a->args)[0])->value == 1)
	      && (*a->args)[0]->MKS.zerop())
	    || ((numvalexp *)(*a->args)[0])->value == 0))
    return(false);
  else return (true);
}
