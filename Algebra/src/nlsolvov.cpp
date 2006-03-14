//    nlsolvov.cpp     (based on normone.cpp which was never completed
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved

/************************************************************************
 *   bool nlsolvov(binopexp * & eqn)					*
 *      eqn is an equation. Unless it is in just one physvar, and	*
 *	it can be solved in that variable, nlsolvov returns false	*
 *      Otherwise it rewrites the equation as 				*
 *	physvar = expression and returns true				*
 *      it asks ispositive(clipsname) if it can assume positive		*
 *  This is only very partially done. In particular, it can only solve	*
 *	the equations (where n* are numvals and v is the variable)	*
 *  	   f(v) = n with f = exp, ln, log10, sqrt, and, if v known >=0,	*
 * 		             abs					*
 *	   v^n1 = n2,  n1^v = n2, n1 * v = n2, v + n1 = n2, and		*
 *		recursions thereof					*
 *  polynomials are not handled here, but by polysolve			*
 ************************************************************************/

#include <cmath>
#include "decl.h"
#include "extoper.h"
#include "dbg.h"
#include "mconst.h"
using namespace std;

#define DBG(A) DBGF(NLSOLV,A)

bool nlsolvov(binopexp * & bineq)
{
  DBG(cout << "Entering nlsolvov with " << bineq->getInfix() << endl;);
  varindx onevar = -1;		// negative means not yet set
  if (!hasjustonevar((expr *)bineq, onevar)) return(false);
  DBG( cout << "NLSOLV justonevar true" << endl;);
  if (onevar < 0) 
    return(false);		// didn't find any variable
  if (bineq->op->opty != equalse)
    throw(string("nlsolvov called on non-equation"));  
  if (bineq->rhs->etype != numval)
    {						// Shouldn't this be done with
      apluskb(bineq->lhs,bineq->rhs,-1.);
      bineq->rhs->destroy();
      bineq->rhs = new numvalexp(0);
      bineq->rhs->MKS = bineq->lhs->MKS;
#if AW_EXP
	  // AW fix: redo var test, in case it was simplified away:
	  varindx onevar = -1;		// negative means not yet set
	  if (!hasjustonevar((expr *)bineq, onevar)) {
		  DBG( cout << "NLSOLV justonevar no longer true!" << endl;);
		  return(false);
	  }
#endif 
    }								//
  // from now on, the rhs remains purely numerical;
  DBG( cout << "NLSOLV rhs at most numval" << bineq->getInfix()<< endl;);
  bool changed = true;
  // AW: should test for bineq->lhs->etype == physvart before downcast?
  while((((physvarptr *)(bineq->lhs))->varindex != onevar) && changed)
    {
      DBG( cout << "NLSOLV at start of while loop " 
	   << bineq->getInfix()<< endl;);
      changed = false;
      if (bineq->rhs->etype != numval) eqnumsimp(bineq->rhs,true);
      if (bineq->rhs->etype != numval) throw(string(
	    "impossible happened - can't make rhs numval in nlsolvov"));
      numvalexp *binrhs =(numvalexp *) bineq->rhs;
      eqnumsimp(bineq->lhs,true);
      switch(bineq->lhs->etype)
	{
	case fake:
	case unknown:
	  throw(string("fake or unknown in call to nlsolvov"));
	case numval:
	  throw(string("impossible num = num in call to nlsolvov"));
	case physvart: 
	  if (((physvarptr *)bineq->lhs)->varindex == onevar) return(true);
	  throw(string("impossible physvar != right one in nlsolvov"));
	case function:
	  {
	    functexp * varside = (functexp *) bineq->lhs;
	    switch(varside->f->opty)
	      {
	      case sine:
	      case cose:
		return(false);	// don't know how to select roots
	      case tane:
		// See Bug #797
		binrhs->value = atan(binrhs->value); // assume [-pi/2,pi/2]
		break;
	      case expe:
		if (binrhs->value <= 0.) 
		  throw(string("attempt to take log of nonpositive"));
		binrhs->value = log(binrhs->value);
		break;
	      case lne:
		binrhs->value = exp(binrhs->value);
		break;
	      case log10e:
		binrhs->value = pow(10.0,binrhs->value);
		break;
	      case sqrte:
		binrhs->value *= binrhs->value;
		binrhs->MKS *=2.;
		break;
	      case abse:
		if (varside->arg->etype != physvart) return(false);
		if (!isnonneg(varside->arg)) return(false);
		break;
	      default:
		throw(string("unknown function in call to nlsolvov"));
	      }
		// AW: rc2a bug, 2/11/04: bineq->lhs and varside point to same functexp, so
		// don't delete it before updating lhs -- VC++ debug CRT lib overwrites freed 
		// blocks w/special pattern (Oxdddd?) to help detect memory errors when debugging.
	    /* delete bineq->lhs; */
	    bineq->lhs = varside->arg; 
		// AW: added next line. No dtor, so delete (as opposed to destroy) should only
		// free parent functexpr node, but not its arg expr now referenced by bineq->lhs.
		// (maybe safer to use copyexpr on arg above followed by destroy?)
		delete varside;
	    changed=true;
	    break;
	  }
	case binop:
	  {
	    binopexp * varside = (binopexp *) bineq->lhs;
	    switch(varside->op->opty)
	      {		
	      case divbye:
	        /***************************************************
		 *    not ready to do this yet
		 * expr * junke;
		 * if (hascommonfactor(varside->lhs,varside->rhs,junke))
		 *   {
		 *     removefactor(junke,varside->lhs);
		 *     removefactor(junke,varside->rhs);
		 *      junke->destroy();
		 *    }
		 * else
		 **************************************************/
		{
		  if (varside->lhs->etype == numval)
		    {
		      binrhs->value = 1./binrhs->value;
		      binrhs->MKS *= -1;
		      expr * junke = varside->lhs;
		      varside->lhs = varside->rhs;
		      varside->rhs = junke;
		      changed = true;
		    }
		  else return(false);
		}
		continue; 
				// what about (x+2)/(x+1) = n ?
	      case topowe:
		DBG(cout << "nlsolvov on topow = numval" << bineq->getInfix()
		    << endl;);
		if ((varside->rhs->etype == numval) && 
		    varside->rhs->MKS.zerop())
		  {
		    int q;
		    double exponv = ((numvalexp *)varside->rhs)->value;
		    if ((exponv < 0) && (binrhs->value == 0))
		      throw(string("can't raise 0 to a negative power"));
		    if (lookslikeint(exponv, q))
		      {
			if (q%2 == 0) // is even?
			  {
			    if (binrhs->value < 0)
			      throw(string(
				   "can't take 1/2n power of negative value"));
			    if (!ispositive(varside->lhs)) {
			      DBG(cout << "couldn't take 1/2n power of "
				  << varside->lhs->getInfix() 
				  << " as it is not known to be positive" 
				  << endl;);
			      return(false); }
			  }
			double temp = pow(fabs(binrhs->value),1./exponv);
			if (binrhs->value < 0.) temp = -temp;
			binrhs->value = temp;
			binrhs->MKS *= 1./exponv;
			bineq->lhs = varside->lhs;
			varside->rhs->destroy();
			delete varside;
			changed = true;
			DBG(cout << "nlsolvov now has " << bineq->getInfix()
			    << endl;);
			continue;
		      }	// end of integer exponent case
		    else
		      {
			if (binrhs->value <0) return(false); // maybe throw?
			binrhs->value = pow(binrhs->value,1./exponv);
			binrhs->MKS *= 1./exponv;
			bineq->lhs = varside->lhs;
			varside->rhs->destroy();
			delete varside;
			changed = true;
			continue;
		      }
		  }
		else
		  {		// can handle a^v=b if a and b numbers > 0
		    if ((varside->lhs->etype != numval ) ||
			(!varside->lhs->MKS.zerop()))
		      return(false);
		    double lhsbase = ((numvalexp *)varside->lhs)->value;
		    if ((lhsbase <=0.) || (binrhs->value <= 0)) return(false);
		    if (!(binrhs->MKS.zerop()))
		      throw(string(
			   "something raised to nondimensionless power"));
		    binrhs->value = log(binrhs->value)/log(lhsbase);
		    binrhs->MKS.put(0,0,0,0,0);
		    bineq->lhs = varside->rhs;
		    varside->lhs->destroy();
		    delete varside;
		    changed = true;
		    continue;
		  }
	      case equalse:
	      case grte:
	      case gree:
	      default:
		throw(string("nlsolvov called with invalid binop on lhs"));
	      }
	  }
	  throw(string("cant get here in nlsolvov binop"));
	case n_op:
	  {
	    n_opexp * varside = (n_opexp *) bineq->lhs;
	    for (int k=0; k < varside->args->size(); k++)
	      if ((*varside->args)[k]->etype == numval)
		{
		  changed = true;
		  switch(varside->op->opty)
		    {
		    case pluse:
		      if (binrhs->MKS == (*varside->args)[k]->MKS)
			binrhs->value -=
			  ((numvalexp *) (*varside->args)[k])->value;
		      else throw(string(
				"nlsolvov tried to add incommensurate terms"));
		      break;
		    case multe:
		      binrhs->value *=
			1./((numvalexp *) (*varside->args)[k])->value;
		      binrhs->MKS += (*varside->args)[k]->MKS * -1.;
		      break;
		    default: 
		      throw(string("unknown n_op in lhs of call to nlsolvov"));
		    }
		  for (int q=k+1;q< varside->args->size(); q++)
		    (*varside->args)[q-1] = (*varside->args)[q];
		  varside->args->pop_back();
		  k--;
		}
	    if (changed) continue; // I've done something, now start over
	    return(false); // didn't solve
	  } // end of case n_op
	default:
	  throw(string("unknown expr type as lhs of eq in nlsolvov"));
	} // end of switch on type of lhs of equation
    } // end of while loop
	return(false);
}
