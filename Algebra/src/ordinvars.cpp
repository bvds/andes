// ordinvars.cpp
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

/************************************************************************
 * bool ordinvars(const expr * ex, const vector<varindx> * vars, 
 *	vector<int> * orders)
 *	returns true if ex is a multi-variate polynomial, and gives
 *	the order to which each variable appears in orders, which
 *	should be a null pointer on call
 *  All functions, and binops other than a top level =,
 *	or topow to positive int, will return false
 ************************************************************************/
#include "decl.h"
#include <math.h>
#include "dbg.h"
using namespace std;

#define DBG(A) DBGF(ORDUNK,A)

bool ordinvars(const expr * ex, const vector<varindx> * vars, 
	       vector<int> * & orders)
{
  n_opexp *thnop;
  binopexp *thbin;
  int k, q;

  DBG( cout << "Entering ordinvars with " <<  ex->getInfix() << endl; );
  if (orders != NULL)
    throw(string("ordinvars called with non-null orders"));
  vector<int> * rorders = NULL;

 switch (ex->etype)
    {
    case unknown:
    case fake:
      throw("ordinvars called on unknown or fake expr");
    case numval:
      DBG( cout << "ordinvars entering numval"<<endl; );
      orders = new vector<int>(vars->size(),0);
      DBG( cout << "ordinvars numval return"<<endl; );
      return(true);
    case physvart:
      orders = new vector<int>(vars->size(),0);
      for (k=0; k < vars->size(); k++)
	if ((*vars)[k] == ((physvarptr *)ex)->varindex) 
	  { (*orders)[k] = 1; return(true); }
      throw(string("ordinvars called with physvar not on vars list"));
    case binop:
      thbin = (binopexp *) ex;
      switch (thbin->op->opty)
	{
	case equalse:
	case grte:
	case gree:
	    if (!ordinvars(thbin->lhs,vars,rorders)) return(false);
	    if (!ordinvars(thbin->rhs,vars,orders)) 
	      {
		delete rorders;
		rorders = NULL;
		return(false);
	      }
	    for (k=0; k < vars->size(); k++)
	      if ((*rorders)[k] > (*orders)[k]) (*orders)[k] = (*rorders)[k];
	    delete rorders;
	    rorders = NULL;
	    return(true);
	case divbye:
	  if (thbin->rhs->etype != numval) return(false);
	  else return(ordinvars(thbin->lhs,vars,orders));
	case topowe:
	  if (thbin->rhs->etype != numval) return(false);
	  if (!lookslikeint(((numvalexp *)thbin->rhs)->value, q))
	    return(false);
	  if (!ordinvars(thbin->lhs,vars,orders)) return(false);
	  for (k=0; k < vars->size(); k++) (*orders)[k] *= q;
	  return(true);
	default:
	  break;  // goto error below
	} // end of switch on binop type
      break;
    case n_op:
      DBG( cout << "ordinvars entering n_op"<<endl; );
      thnop = (n_opexp *) ex;
      orders = new vector<int>(vars->size(),0);
      for (k=0; k < thnop->args->size(); k++)
	{
	  DBG( cout << "ordinvars will call arg "<< k<<endl; );
	  if (!ordinvars((*thnop->args)[k],vars,rorders))
	    {
	      DBG( cout << "arg "<< k <<" gave false"<<endl; );
	      delete orders;
	      orders = NULL;
	      return(false);
	    }
	  DBG( { cout << "arg "<< k <<" gave true" << endl;
		 cout << "orders and rorders have sizes " << endl;
		 cout << orders->size() << " and " << endl;
		 cout << rorders->size() << endl; } ) ;
	  for (q=0; q < vars->size(); q++)
	    {
	      if (thnop->op->opty == pluse) {
		if ((*rorders)[q] > (*orders)[q]) (*orders)[q] = (*rorders)[q];
	      }
	      else if (thnop->op->opty == multe)
		(*orders)[q] += (*rorders)[q];
	      else throw(string("unknown n_op in ordinvars"));
	    }
	  delete rorders;
	  rorders = NULL;
	}
      return(true);
    case function:
      return(false);
    default:
      throw(string("unknown expr in ordinvars"));
    } // end of switch in ex type
 throw(string("got to impossible place in ordinvars"));
}

