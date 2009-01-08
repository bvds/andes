// bool subexpin(expr * & target,  const binopexp * assign)
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
#include "dbg.h"
#include "extstruct.h"
using namespace std;

#define DBG(A) DBGF(SUBST,A)

bool exprcontains(expr * e, int var);

/************************************************************************
 *  bool subexpin(expr * & target, const binopexp * assign)		*
 *	assign is an equation of the form physvar v = expr		*
 *	every occurence of v in target is replaced by numval.		*
 *	eqnumsimp should be called on result of top level call		*
 *  returns true if no problem encountered, and target was changed	*
 *	aborts if lhs is not physvar v, or if v occurs on rhs		*
 *									*
 ************************************************************************/
bool subexpin(expr * & target, const binopexp * assign)
{		
  bool changed;
  if ( (assign->lhs)->etype != physvart) return(false);
  expr * substval  = assign->rhs;
  int replacv = ((physvarptr *)( assign->lhs))->varindex;
  if (exprcontains(substval,replacv)) return(false);
  DBG( { cout << "replacing " << (*canonvars)[replacv]->clipsname << " with " 
	      << substval->getInfix() << " in" << endl;
         target->dbgprint(2); } ) ;
  switch (target->etype)
    {
    case numval:
      return(false);
    case physvart:
      DBG( cout << "trying physvar " << target->getInfix() << endl; );
      if (((physvarptr *)target)->varindex == replacv) {
	target = copyexpr(substval);
	return(true); }
      else return(false);
    case function:
      DBG( cout << "trying function " << target->getInfix() << endl; );
      return(subexpin(((functexp *)target)->arg, assign));
    case binop:
      {
	changed = false;
	DBG( cout << "trying binop " << target->getInfix() << endl; );
	if (subexpin(((binopexp *)target)->lhs, assign)) changed = true;
	if (subexpin(((binopexp *)target)->rhs, assign)) changed = true;
      return(changed);
      }
    case n_op:
      {
	changed = false;
	DBG( cout << "trying n_op " << target->getInfix() << endl; );
	for (int k = 0; k < ((n_opexp *)target)->args->size(); k++)
	  if (subexpin((*((n_opexp *)target)->args)[k], assign)) 
	    changed = true;
	return(changed);
      }
    case unknown:
    case fake:
    default:
      throw(string("unknown or fake expression in subexpin first arg"));
    }
}

bool exprcontains(expr * e, int var)
{
  switch(e->etype)
    {
    case numval:
      return(false);
    case physvart:
      return(var == ((physvarptr *)e)->varindex);
    case function:
      return(exprcontains(((functexp *)e)->arg,var));
    case binop:
      return(exprcontains(((binopexp *)e)->lhs,var) ||
	     exprcontains(((binopexp *)e)->rhs,var));
    case n_op:
      {
	n_opexp * nop = (n_opexp *)e;
	for (int k=0; k < nop->args->size(); k++)
	  if ( exprcontains((*nop->args)[k],var)) return(true);
	return(false);
      }
    case unknown:
    case fake:
    default:
      throw(string("exprcontains called on unknown/fake expr"));
    }
}
