//	rationalize.cpp
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
#include "extoper.h"
#include "dbg.h"
using namespace std;

#define DBG(A) DBGF(RATEQ,A)

/************************************************************************
 * bool rationalize(binopexp * & eq)					*
 *	eq should be an equation which can be brought into the form	*
 *	(sum of products of numvals, physvars, and powers of physvars)	*
 *	   = 0								*
 *	by multiplying through by common denominators. Attempts to 	*
 *	keep denominator minimal.					*
 ************************************************************************/
//		[do powers include sqrt? ]
bool rationalize(binopexp * & bineq)
{
  n_opexp * ourf;
  int k, q;

  DBG( cout << "Entering rationalize with " << bineq->getInfix() << endl; );
  if (bineq->op->opty != equalse)
    throw(string("rationalize called on non-equation"));  
  expr * eqcpy = copyexpr(bineq);
  DBG( cout << "In rationalize after copyexpr, eqcpy is "
	    << eqcpy->getInfix() << endl; );
  numvalexp * answer = normexpr(eqcpy);
  if(answer) answer->destroy(); //stop memory leak
  DBG( { cout << "In rationalize after normexpr, eqcpy is "
	    << eqcpy->getInfix() << endl;
	 cout << "In rationalize after normexpr, lhs is "
	      << ((binopexp *)eqcpy)->lhs->getInfix() << endl;
	 cout << "I expected a flattened expr, but" << endl; } ) ;
  flatten(((binopexp *)eqcpy)->lhs);
  DBG( cout << "In rationalize after flatten, lhs is "
	    << ((binopexp *)eqcpy)->lhs->getInfix() << endl; );
  if ((((binopexp *)eqcpy)->lhs->etype != n_op) ||
      ((n_opexp *)(((binopexp *)eqcpy)->lhs))->op->opty != pluse)
    {
      ourf = new n_opexp(&myplus);
      ourf->addarg(((binopexp *)eqcpy)->lhs);
    }						// top level expr now sum
  else ourf = (n_opexp *)((binopexp *)eqcpy)->lhs; 
  ((binopexp *)eqcpy)->rhs->destroy();
  delete eqcpy;
  int ksum;
  n_opexp * denom = new n_opexp(&mult);	// current lowest common denom
  n_opexp * tempnop;
  for (ksum = 0; ksum < ourf->args->size(); ksum++)
    {
      switch((*ourf->args)[ksum]->etype)
	{
	case numval:
	case physvart:
	  if (denom->args->size() == 0) continue; 	// multiply by
	  tempnop = new n_opexp(&mult);			// current LCD
//	  tempnop->MKS.put(0,0,0,0,0); // REMOVE after fixing constructor 
	  tempnop->addarg((*ourf->args)[ksum]); // and continue
	  for (k = 0; k < denom->args->size(); k++)
	    tempnop->addarg((*denom->args)[k]);
	  (*ourf->args)[ksum]=tempnop;
	  continue;
	case function:		// functions violate the restriction to polys
	  ourf->destroy();	// clean up and return false
	  denom->destroy();
	  DBG( cout << "Rationalize false from function " << endl; );
	  return(false);
	case binop:
	  {
	    binopexp * binterm = (binopexp *)(*ourf->args)[ksum];
	    if (binterm->op->opty == divbye) 	// this term is a ratio
	      {					// make sure denom is mult
		if ((binterm->rhs->etype != n_op) ||
		    ((n_opexp *)binterm->rhs)->op->opty != multe)
		  {
		    tempnop = new n_opexp(&mult);
//	  tempnop->MKS.put(0,0,0,0,0); // REMOVE after fixing constructor 
		    tempnop->addarg(binterm->rhs);
		    binterm->rhs = tempnop;
		  } 		// ratio denom to be compared to denom to 
		n_opexp * thisden = (n_opexp *) binterm->rhs;   // remove com- 
		n_opexp * denleft = (n_opexp *) copyexpr(denom); // mon factors
		for (int qthd=0; qthd < thisden->args->size(); qthd++) 
		  for (int qdl=0; qdl < denleft->args->size(); qdl++) 
		    if (equaleqs((*thisden->args)[qthd],(*denleft->args)[qdl]))
		      {				// remove common factors
			(*thisden->args)[qthd]->destroy();
			(*thisden->args)[qthd] = 
			  (*thisden->args)[thisden->args->size()-1];
			thisden->args->pop_back();
			(*denleft->args)[qdl]->destroy();
			(*denleft->args)[qdl] = 
			  (*denleft->args)[denleft->args->size()-1];
			denleft->args->pop_back();
			qthd--;
			break;
		      }
		// if factors in this terms denom not already in QCD, add
		// them to QCD and correct all prior terms in sum
		if (thisden->args->size() > 0)
		  {
		    for (k=0 ; k < ksum; k++) {
		      if (((*ourf->args)[k]->etype != n_op) ||
			  ((n_opexp *)(*ourf->args)[k])->op->opty != multe)
			{
			  n_opexp * temp = new n_opexp(&mult);
//	  temp->MKS.put(0,0,0,0,0); // REMOVE after fixing constructor 
			  temp->addarg(((*ourf->args)[k]));
			  (*ourf->args)[k] = temp;
			}
		      for (q = 0; q < thisden->args->size(); q++)
			((n_opexp *)(*ourf->args)[k])->args->
			  push_back(copyexpr((*thisden->args)[q]));
		    }
		    thisden->destroy();
		  }
		(*ourf->args)[ksum] = binterm->lhs;
		delete binterm;
		if (denleft->args->size() > 0)
		  {
		    if (((*ourf->args)[ksum]->etype != n_op) ||
			((n_opexp *)(*ourf->args)[ksum])->op->opty != multe)
		      {
			n_opexp * temp = new n_opexp(&mult);
//	  temp->MKS.put(0,0,0,0,0); // REMOVE after fixing constructor 
			temp->addarg(((*ourf->args)[ksum]));
			(*ourf->args)[ksum] = temp;
		      }
		    for (q = 0; q < denleft->args->size(); q++)
			((n_opexp *)(*ourf->args)[ksum])->args->
			  push_back((*denleft->args)[q]);
		  }
		delete denleft;
		continue;
	      }
	    else if (binterm->op->opty == topowe)
	      {
		if ((binterm->rhs->etype != numval) ||
		    (!lookslikeint(((numvalexp *)binterm->rhs)->value,q)))
		  {
		    ourf->destroy();	// clean up and return false
		    denom->destroy();
		    DBG( cout << "Rationalize false from topowe " << endl; );
		    return(false);
		  }
		if (binterm->lhs->etype != physvart)
		  throw(string("Could happen, but rationalize is not prepared")
			+string(" for powers of other than simple variables"));
		if (denom->args->size() == 0) continue; 	// multiply by
		n_opexp * tempnop = new n_opexp(&mult);		// current LCD
//	  tempnop->MKS.put(0,0,0,0,0); // REMOVE after fixing constructor 
		tempnop->addarg((*ourf->args)[ksum]); // and continue
		for (k = 0; k < denom->args->size(); k++)
		  tempnop->addarg((*denom->args)[k]);
		(*ourf->args)[ksum]=tempnop;
		continue;
	      }
	    else
	      {
		ourf->destroy();	// clean up and return false
		denom->destroy();
		DBG( cout << "Rationalize false from other binop " << endl; );
		return(false);
	      }
	  } // end of case binop
	case n_op:
	  {
	    n_opexp * thnop = (n_opexp *)(*ourf->args)[ksum];
	    if (thnop->op->opty != multe)
	      throw(string("rationalize: terms in sum cant be sums"));
	    for (k = 0; k < thnop->args->size(); k++)
	      switch((*thnop->args)[k]->etype)
		{
		case numval:
		case physvart:
		  continue;
		case function:
		  ourf->destroy();	// clean up and return false
		  denom->destroy();
		  DBG( cout << "Rationalize false from function in nop "
		            << endl; ) ;
		  return(false);
		case binop:
		  // not supposed to be a divbye inside a mult, 
		  {
		    binopexp * binterm = (binopexp *)(*thnop->args)[k];
		    if ((binterm->op->opty != topowe) ||
			(binterm->rhs->etype != numval) ||
			(!lookslikeint(((numvalexp *)binterm->rhs)->value,q)))
		      {
			ourf->destroy();	// clean up and return false
			denom->destroy();
			DBG( cout 
			     << "Rationalize false from divby inside mult "
			     << endl; ) ;
			return(false);		// other things to destroy?
		      }
		    continue;
		  }
		case n_op:
		  throw(string
			("n_op inside mult inside plus shouldn't happen"));
		case unknown:
		case fake:
		default:
		  throw(string
			("impossible expr deep into expr in rationalize"));
		} // end of switch over thnop[k] type, 
		  // and of loop over mult args
	    for (k = 0; k < denom->args->size(); k++)
	      thnop->addarg(copyexpr((*denom->args)[k]));
	    continue;
	  } // end of case term = n_op
	case unknown:
	case fake:
	default:
	  throw(string("invalid expr as term in rationalize"));
	} // end of switch over type of term
    } // end of sum over terms
  denom->destroy();
  bineq->lhs->destroy();
  bineq->rhs->destroy();
  bineq->lhs = ourf;
  bineq->rhs = new numvalexp(0);
  return(true);
  DBG( cout << "Rationalize returns true with " << bineq->getInfix() << endl;);
}

