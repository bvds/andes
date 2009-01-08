// fixupforpls.cpp
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
//

#include "decl.h"
#include "extoper.h"
#include "dbg.h"
using namespace std;

#define DBG(A) DBGF(FUFPLS,A)
bool fixupterm(expr * & ex);

/************************************************************************
 * bool fixupforpls(binopexp * & eq)					*
 * 	Attempts to put eq in form suitable for purelinsolv, which	*
 *	means: 								*
 *	  rhs is a numval 						*
 *	  lhs is a physvar, 						*
 *		 a mult with at most one numvar and then a physvar	*
 *		 a plus with each term as on last line, or a numval	*
 *	returns true if successful, false if not			*
 ************************************************************************/
bool fixupforpls(binopexp * & bineq)
{
  int k;

  DBG( cout << "entering fixupforpls with " << bineq->getInfix() << endl;);
  expr *eq =bineq;
  flatten(eq);				// flatten and eqnumsimp added 11/14
  eqnumsimp(eq,true);
  if (ordunknowns(eq,false) != 1) return(false);
  
  if (eq->etype != binop)
    throw(string("tried to fixup for pls a non-binop non-equation"));
  bineq = (binopexp *) eq;
  if (bineq->op->opty != equalse)
    throw(string("tried to fixup for pls a non-equation"));
  if ((bineq->rhs->etype != numval)   ||	// bring all to lhs
      (((numvalexp *)bineq->rhs)->value != 0))
    {
      DBG(cout << "fixupforpls about to move rhs to lhs" << endl; );
      apluskb(bineq->lhs,bineq->rhs,-1.);
      bineq->rhs = new numvalexp(0);
      bineq->rhs->MKS = bineq->lhs->MKS;
      DBG({cout << "FUFPLS: after move to lhs, bineq is" << endl;
           bineq->dbgprint(4); });
    }
  bool flattened = flatten(bineq->lhs);
  DBG({ cout << "FUFPLS: from lhs of bineq, flatten just returned "
	     << (flattened ?"true":"false" ) << endl;
	bineq->lhs->dbgprint(4); } );
  eqnumsimp(bineq->lhs,true);
  DBG( { cout << "FUFPLS: After eqnumsimping that, we have" << endl;
         bineq->lhs->dbgprint(4); } );
  if (bineq->lhs->etype == physvart) return (true);
  if (bineq->lhs->etype != n_op) return(false);
  if ( ((n_opexp *)bineq->lhs)->op->opty == multe) 
    return ( (fixupterm(bineq->lhs)) ? true : false);
  else for (k=0;k < ((n_opexp *)bineq->lhs)->args->size(); k++)
    if (!fixupterm( (*(((n_opexp *)bineq->lhs)->args))[k] )) return(false);
  return(true);
}

/************************************************************************
 * fixupterm	must not be called on non-mult n_ops (throws exception	*
 * 	returns true if it can write the expression as			*
 *		physvart						*
 *		numval							*
 *		numval * physvart   (or numval * numval)		*
 *	the only rearranging it will do is replace 			*
 *		physvart * numval  ->   numval * physvart		*
 *	otherwise returns false						*
 ************************************************************************/
 
bool fixupterm(expr * & ex)
{
  DBG( cout << "entering fixupterm with " << ex->getInfix() << endl;);
  switch (ex->etype)
    {
    case physvart:
    case numval:
      return(true);
    case n_op:
      {
	DBG( cout << "entering fixupterm n_op" << endl; );
	n_opexp * nopex = (n_opexp *) ex;
	DBG( cout << "fixupterm defined nopex" << endl; );
	if (nopex->op->opty != multe)
	  throw(string("fixupterm doesn't expect non-mult n_ops"));
	DBG( cout << "n_op is multe" << endl; );
	if (nopex->args->size() > 2) return(false);
	DBG( cout << "n_op has fewer than 3 factors" << endl; );
	for (int k=0; k < nopex->args->size(); k++) 
	  {
	    if (((*nopex->args)[k]->etype != numval) &&
		((*nopex->args)[k]->etype != physvart)) return(false);
	    DBG( cout << "factor " << k << " passed test" << endl; );
	  }
	DBG( cout << "factors individually checked out" << endl; );
	if (nopex->args->size() < 2) return(true);
	if  ((*nopex->args)[0]->etype == numval) return(true);
        if  ((*nopex->args)[1]->etype == physvart) return(false);
	DBG( cout << "fixupterm needs to swap factors" << endl; );
	expr * temp = (*nopex->args)[1];
	(*nopex->args)[1] = (*nopex->args)[0];
	(*nopex->args)[0] = temp;
	return(true);
      }
    case function:
    case binop:
      return(false);
    case unknown:
    case fake:
    default:
      throw(string("impossible expr to fixterm"));
    }
}

