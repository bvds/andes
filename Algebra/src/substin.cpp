// void substin(expr * & target, const expr * assign)
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
#include "extstruct.h"
#include "dbg.h"
using namespace std;

#define DBG(A) DBGF(SUBST,A)

/************************************************************************
 *  bool substin(expr * & target, const binopexp * assign)		*
 *	assign is an equation of the form physvar v = numval.		*
 *	every occurence of v in target is replaced by numval.		*
 *	eqnumsimp should be called on result of top level call if true	*
 *  returns true if change made to target				*
 ************************************************************************/
bool substin(expr * & target, const binopexp * assign)
{				// this could be made more general by not
				// casting value not numvalexp
  if (  (( assign->rhs)->etype != numval) || 
	(( assign->lhs)->etype != physvart))
    throw(string("substin called with assign not var = number"));
  numvalexp *value = (numvalexp *)( assign->rhs);
  varindx replace = ((physvarptr *)( assign->lhs))->varindex;
  DBG( { cout << "replacing " << (*canonvars)[replace]->clipsname << " with "
	      << value->getInfix() << " in" << endl;
	 target->dbgprint(2); } );
  bool answer = false;
  switch (target->etype)
    {
    case numval:
      return(false);
    case physvart:
      if (((physvarptr *)target)->varindex == replace) {
	((physvarptr *)target)->destroy();  //remove previous quantity
	target = copyexpr(value);
	return(true); }
      else return(false);
    case function:
      return(substin(((functexp *)target)->arg, assign));
    case binop:
      answer = substin(((binopexp *)target)->lhs, assign) || answer;
      answer = substin(((binopexp *)target)->rhs, assign) || answer;
      return(answer);
    case n_op:
      {
	for (int k = 0; k < ((n_opexp *)target)->args->size(); k++)
	  answer = substin((*((n_opexp *)target)->args)[k], assign) || answer;
	return(answer);
      }
    case unknown:
    case fake:
    default:
      throw(string("unknown or fake expression in substin first arg"));
    }
}
