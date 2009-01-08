// eqnumsimp  
// this version has a choice as to whether all functions which might 
//    convert ints to reals should be disabled.
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
//	units in numvals added 3/25/01

#include "decl.h"
#include "extoper.h"
#include "dbg.h"
#include "extstruct.h"
#include <math.h>
#include "mconst.h"
using namespace std;


#define DBG(A) DBGF(EQNUM,A)
#define EQDEEP(A) DBGFM(EQNUM,A)

// note: handles relerr only for mults
// if 1 degree is a numval with value 1, we have FAKEDEG
#ifdef FAKEDEG
#define DEG2RAD DEGTORAD
#else
#define DEG2RAD 1.
#endif

/************************************************************************
 * void eqnumsimp(expr * & e, bool flok)				*
 *	recursively evaluates n_ops, binops and functions of numbers, 	*
 *	modifying expr e						*
 * in plus and mult expressions, combines pure numbers but not like 	*
 *	terms, eg 2 + 5 -> 7 but 2*x + 5*x is unchanged			*
 * if sum of numbers is less than RELERR * sum of abs, sets to zero	*
 * if flok is false, will not take function which changes an		*
 *	  integer to a real						*
 ************************************************************************/
				// possibly this should be changed to return
				// a bool, true if e is changed. some changes
void eqnumsimp(expr * & e, const bool flok)		// shown as comments
{
  numvalexp *save;
  double value;
				// bool answer -= false;
  EQCHK(e);
#ifdef WITHDBG
  unsigned int thisdbg = ++dbgnum;
#endif
  DBG( cout << "eqnumsimp " << thisdbg << " on " << e->getInfix() << endl);

  if ((e->etype == unknown)  || 
      (e->etype == numval)   ||  
      (e->etype == physvart) ||  
      (e->etype == fake)){
    DBG(cout << "eqnumsimp " << thisdbg << " returning" << endl);
    return;						// (false)
  }
  if (e->etype == function)
    {
      functexp *th = (functexp *) e;
      eqnumsimp(th->arg,flok);			// answer = ... || answer
      if (th->arg->etype == numval) {
	numvalexp *tharg = (numvalexp *) th->arg;
	if (th->f->opty == sqrte)
	  {
	    if (flok) {
	      save = new numvalexp(sqrt(tharg->value));
	      save->MKS = tharg->MKS;
	      save->MKS *= 0.5;
	    }
	    else
	      {
		DBG(cout << "eqnumsimp " << thisdbg << " returning" << endl);
		return;				// (false)
	      }
	  }
	else if (th->f->opty == abse)
	  {
	      save = new numvalexp(fabs(tharg->value));
	      save->MKS = tharg->MKS;
	  }
	else {
	  int q;
	  if (th->MKS.unknp()) th->MKS.put(0,0,0,0,0);
	  switch (th->f->opty)
	    {
	    case sine:
	      if (flok)
		{
		  value = sin(DEG2RAD * tharg->value);
		  if (lookslikeint(value, q)) value = (double) q;
		  save = new numvalexp(value);
		  break;
		}
	      else
		{
		  DBG(cout << "eqnumsimp " << thisdbg << " returning" << endl);
		  return;				// (answer)
		}
	    case cose:
	      if (flok)
		{
		  value = cos(DEG2RAD * tharg->value);
		  if (lookslikeint(value, q)) value = (double) q;
		  save = new numvalexp(value);
		  break;
		}
	      else 
		{
		  DBG(cout << "eqnumsimp " << thisdbg << " returning" << endl);
		  return;				// (answer)
		}
	    case tane:
	      if (flok)
		{
		  value = tan(DEG2RAD * tharg->value);
		  if (lookslikeint(value, q)) value = (double) q;
		  save = new numvalexp(value);
		  break;
		}
	      else 
		{
		  DBG(cout << "eqnumsimp " << thisdbg << " returning" << endl);
		  return;				// (answer)
		}
	    case expe:
	      if (flok)
		{
		  save = new numvalexp(exp(tharg->value));
		  break;
		}
	      else
		{
		  DBG(cout << "eqnumsimp " << thisdbg << " returning" << endl);
		  return;				// (answer)
		}
	    case lne:
	      if (flok)
		{
		  save = new numvalexp(log(tharg->value));
		  break;
		}
	      else {
		DBG(cout << "eqnumsimp " << thisdbg << " returning" << endl);
		return;				// (answer)
	      }
	    case log10e:
	      if (flok)
		{
		  save = new numvalexp(log10(tharg->value));
		  break;
		}
	      else 
		{
		  DBG(cout << "eqnumsimp " << thisdbg << " returning" << endl);
		  return;				// (answer)
		}
	    default:
	      throw(string(
		   "function of unknown type not handled by eqnumsimp"));
	    } // end of switch
	  save->MKS.put(0,0,0,0,0);
	} // end of not sqrt or abs
	th->destroy();
	e = save;				// answer = true;
      }	// end of if arg is numval
      DBG(cout << "eqnumsimp " << thisdbg << " returning" << endl);
      return;						// (answer)
    } // end of if e is function
  if (e->etype == binop)
    {
      binopexp *th = (binopexp *) e;
      eqnumsimp(th->lhs,flok);			// answer = ... || answer
      eqnumsimp(th->rhs,flok);			// answer = ... || answer
      if ((th->lhs->etype == numval) 
	  && (th->rhs->etype == numval))
	{
	  numvalexp * thlhs = (numvalexp *) th->lhs;
	  numvalexp * thrhs = (numvalexp *) th->rhs;
	  switch (th->op->opty)
	    {
	    case equalse:
	    case grte:
	    case gree:
	      if (thlhs->MKS.unknp()) thlhs->MKS = thrhs->MKS;	// would this
	      if (thrhs->MKS.unknp()) thrhs->MKS = thlhs->MKS;	// set answer?
	      if (!(thlhs->MKS == thrhs->MKS))
		throw(string(
	  "equality or inequality between things with different dimensions"));
	      DBG(cout << "eqnumsimp " << thisdbg << " returning " 
		  << e->getInfix()<< endl);
	      return;						// (answer)
	    case divbye:
	      if (fabs(thrhs->value) == 0.0) throw(string("Divide by zero"));
	      value = thlhs->value / thrhs->value;
	      save = new numvalexp(value);
	      save->MKS = thlhs->MKS + thrhs->MKS * -1.;
	      break;
	    case topowe:
	      value = pow(thlhs->value,thrhs->value);
	      if (thrhs->MKS.unknp()) thrhs->MKS.put(0,0,0,0,0);
	      if (!(thrhs->MKS.zerop()))
		throw(string("can't raise to power with dimensions"));
	      save = new numvalexp(value);
	      save->MKS = thlhs->MKS;
	      save->MKS *= thrhs->value;
	      break;
	    default:
	      throw(string("unknown binary operator"));
	    }
	  th->destroy();				// answer = true;
	  e = save;
	} // end of both sides numvals
      else if (th->rhs->etype == numval)
	{
	  numvalexp * thrhs = (numvalexp *) th->rhs;
	  switch (th->op->opty)
	    {
	    case equalse:
	    case grte:
	    case gree:
	      DBG(cout << "eqnumsimp " << thisdbg << " returning." << endl);
	      return;					// (answer)
	    case divbye:
	      if ((thrhs->value == 1.0) && thrhs->MKS.zerop())
		{
		  e = th->lhs;
		  delete th->rhs;
		  delete th;
		  DBG( cout << "eqnumsimp " << thisdbg << " returning " 
		       << e->getInfix() << endl);
		  return;					// (true)
		}
	      else
		{
		  thrhs->value = 1./thrhs->value;
		  thrhs->MKS *= -1.;
		  n_opexp *save = new n_opexp(&mult);
		  save->addarg(th->lhs);
		  save->addarg(thrhs);
		  delete th;
		  e = save;
		  DBG( cout << "eqnumsimp " << thisdbg << " returning " 
		       << e->getInfix() << endl);
		  return;					// (true)
		}
	    case topowe:
	      if (thrhs->value == 0)
		{
		  th->destroy();
		  e = new numvalexp(1.);
		  e->MKS.put(0,0,0,0,0);
		  DBG( cout << "eqnumsimp " << thisdbg << " returning " 
		       << e->getInfix() << endl);
		  return;					// (true)
		}
	      if ((thrhs->value == 1.) && thrhs->MKS.zerop())
		{
		  EQDEEP( cout << "Eqnumsimp working on "
			 << th->getInfix() << endl);
		  e = th->lhs;
		  delete th->rhs;
		  delete th;	   			 // answer = true;
		  EQDEEP( cout << "Eqnumsimp about to return "
			  << e->getInfix() << endl;);
		}
	      DBG( cout << "eqnumsimp " << thisdbg << " returning " 
		   << e->getInfix() << endl);
	      return;	    				// (answer)
	    default:
	      throw(string("unknown binary operator rhs known"));
	    }
	}
      DBG( cout << "eqnumsimp " << thisdbg << " returning " 
	   << e->getInfix() << endl);
      return;
    }
  if (e->etype == n_op)			// NOTE: this does not appear to 
    {					// move numval to arg[0], although it
      bool allnum = true;		// does combine them.
      n_opexp *th = (n_opexp *) e;
      int numk=-1;		// if >=0, first numval arg
      double sumabs;
      numvalexp *numkexp;
   
      for (int k =0; k<th->args->size(); k++)
	{
	  eqnumsimp((*th->args)[k],flok);	   // answer = ... || answer
	  if (!((*th->args)[k]->etype==numval)) allnum = false;
	  else  // th->args[k] is a numval
	    {
	      numvalexp * tharg =  (numvalexp *) (*th->args)[k];
	      if (numk < 0) // first numval: remember it
		{ 
		  numk = k; 
		  numkexp = tharg;
		  sumabs = fabs(tharg->value);
		  continue; 
		}
	      else {  // numk >= 0: not first numval. 
		if (th->op->opty == multe) {
		  if (numkexp->abserr > 0) {
		    if (tharg->abserr <= 0) numkexp->abserr = tharg->abserr;
		    else numkexp->abserr = tharg->abserr * fabs(numkexp->value)
			   + numkexp->abserr * fabs(tharg->value);}
		  DBG(cout << "eqnumsimp " << thisdbg << " multiplying " 
		      << numkexp->getInfix() << " and " 
		      << tharg->getInfix() << endl);
		  numkexp->value *= tharg->value;
		  numkexp->MKS += tharg->MKS;
		}
		else if (th->op->opty == pluse)
		  {
		    if (numkexp->abserr >= 0) {
		      if (tharg->abserr < 0) numkexp->abserr = tharg->abserr;
		      else numkexp->abserr = tharg->abserr + numkexp->abserr; }
		    numkexp->value += tharg->value;
		    sumabs += fabs(tharg->value);
		    if (numkexp->MKS.unknp()) numkexp->MKS = tharg->MKS;
		    if (tharg->MKS.unknp()) tharg->MKS = numkexp->MKS;
		    if (!(numkexp->MKS == tharg->MKS))
		      throw(string(
				   "Eqnumsimp: Can't add terms with different units"));
		  }
	        else throw (string("unknown n-ary with known arg"));
		(*th->args)[k]->destroy();
		for (int q=k+1;q< th->args->size(); q++)
		  (*th->args)[q-1] = (*th->args)[q];
		th->args->pop_back();			// answer = true
		k--;
	      } // end of numk >= 0
	    } // end of th->args[k] a numval
	} // end of loop over k
      if (numk >= 0)
	{
	  (*th->args)[numk] = numkexp; // AW: needed? numkexp points to args[numk] expr above  
	  if ((th->op->opty == pluse)  &&
	      (fabs(numkexp->value) < sumabs * RELERR)) numkexp->value = 0.;
	  if (numkexp->value == 0.)
	    {
	      if (th->op->opty == multe) // multiplying by a 0
		{
		  e = new numvalexp(0.);
#if AW_EXP // experimental fix
		  // AW fix (Exkt13a bug): give 0 same units as product, not unknown
		  e->MKS.put(0,0,0,0,0);
		  for (int q=0; q<th->args->size(); q++) {
			e->MKS += (*th->args)[q]->MKS;
		  }
#endif
		  th->destroy();
		  DBG( cout << "eqnumsimp " << thisdbg << " returning " 
		       << e->getInfix() << endl);
		  return;					// (true)
		}
	      else if (th->op->opty == pluse)
		{
		  (*th->args)[numk]->destroy();
		  for (int q=numk+1;q< th->args->size(); q++)
		    (*th->args)[q-1] = (*th->args)[q];
		  th->args->pop_back();			// answer = true;
		}
	      else throw(string("unknown n_op in eqnumsimp"));
	    }
	  else if ((numkexp->value == 1.) && (th->op->opty == multe)
		   && numkexp->MKS.zerop())
	    {
	      (*th->args)[numk]->destroy();
	      for (int q=numk+1;q < th->args->size(); q++)
		(*th->args)[q-1] = (*th->args)[q];
	      th->args->pop_back();			// answer = true;
	    }
	  EQDEEP( cout << "Eqnumsimp did find numval " << endl; );
	} // end of if (numk >= 0)
      if (th->args->size() == 1)
	{
	  e = (*th->args)[0];
	  th->args->pop_back();
	  delete th->args;
	  delete th;
	  DBG( cout << "eqnumsimp " << thisdbg << " returning " 
	       << e->getInfix() << endl);
	  return;					// (true)
	}
      if (th->args->size() == 0)
	{
	  EQDEEP( cout << "Eqnumsimp in n_op no args " << endl; );
#ifndef AW_EXP // original code
	  if (th->op->opty == pluse) e = new numvalexp(0);
#else // AW_EXP experimental replacement. 
	  // AW: give zero units of sum, in case matters
	  if (th->op->opty == pluse) {
		e = new numvalexp(0);
		e->MKS = th->MKS;  
	  }
#endif
	  else { 
	    e = new numvalexp(1);
	    e->MKS.put(0,0,0,0,0);
	  }
	  delete th->args;
	  delete th;
	  DBG( cout << "eqnumsimp " << thisdbg << " returning " 
	       << e->getInfix() << endl);
	  return;					// (true)
	}
      DBG( cout << "eqnumsimp " << thisdbg << " returning " 
	   << e->getInfix() << endl);
      return;					// (answer)
    } // end of if e is n_op
  throw(string("Unknown form of expression in call to eqnumsimp"));
}
