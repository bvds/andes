// distfrac.cpp		check a plus n_op for terms which are ratios, 
//			if found, return a divby with a least common denom.
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved

#include "decl.h"
#include "extoper.h"
#include "dbg.h"
using namespace std;

#define DBG(A) DBGF(INTFL,A)

/************************************************************************
 * bool distfrac(expr * & ex)						*
 *	ex must be a plus n_op whose terms should have already been 	*
 *	  flattened (so that none of them are plus n_ops).		*
 *	If any of the terms are ratios (divby binops), the entire 	*
 *	  expression is written as one ratio, with the 			*
 *	  (hopefully) least common denominator of all the terms as the	*
 *	  denominator.							*
 *	Returns true if a change has been made				*
 ************************************************************************/
bool distfrac(expr * & ex)
{
  int k, q;

  int thisdbg = ++dbgnum;
  if (ex->etype != n_op) throw(string("distfrac called on non-n_op"));
  n_opexp * enop = (n_opexp *) ex;
  if (enop->op->opty != pluse) throw(string("distfrac called on non-plus"));
  if (enop->args->size() < 2) { unnop(ex);   return(true);  }
  // lcd is the least common denominator of all terms in the plus,
  //   built up first before working on numerator
  //	 WARNING  lcd and denom may have <2 args
  n_opexp *lcd = new n_opexp(&mult);	// overall denominator
  n_opexp *denom;		// denominator of current term
  
  DBG( cout << "distfrac " << thisdbg << " on an plus nop of size "
	     << enop->args->size() << endl);
  for (k=0; k < enop->args->size(); k++)
    if (((*enop->args)[k]->etype == binop)  &&
	(((binopexp *)(*enop->args)[k])->op->opty == divbye))
      {
	DBG(cout << k << " term of enop plus is a divbye" << endl);
	// nkbin = this term as binop
	binopexp *nkbin = (binopexp *)(*enop->args)[k];
	// if this term's denominator is a mult, treat each factor separately,
	// but otherwise treat the whole denominator as one factor
	// It is denom
	if ((nkbin->rhs->etype == n_op) &&
	    (((n_opexp *)nkbin->rhs)->op->opty == multe))
	  denom = (n_opexp *) copyexpr(nkbin->rhs);
	else
	  {
	    denom = new n_opexp(&mult);
	    denom->addarg(copyexpr(nkbin->rhs));
	  }
	// for each factor in denom, see if it matches one in lcd,
	// If not, add it to lcd, and in either case remove from denom
	// ??? what if a factor appears twice in denom and once in lcd?
	for (q=0; q < denom->args->size(); q++)
	  {
	    int r;
	    for (r=0; r < lcd->args->size(); r++)
	      if (equaleqs((*denom->args)[q],(*lcd->args)[r])) break;
	    if (r == lcd->args->size()) 
	      lcd->addarg((*denom->args)[q]);
	    else (*denom->args)[q]->destroy();
	  }
	// rmed 2/4/01 need to check		delete denom->args;
	delete denom;			// lcd holds new denominator
      } // end of loop over terms, and also of this term is divby
  // note: lcd left mult even if zero or one arg until sum done
  // if sum includes a ratio, plsnumer will become numerator, 
  // rete answer.
  if (lcd->args->size() > 0)
    {
      DBG( cout << "finished making lcd of + / ..., it is = "
	   << lcd->getInfix() << endl );
      n_opexp *plsnumer =  new n_opexp(&plus);
      n_opexp * temp4;		// numerator of current term
      for (k=0; k < enop->args->size(); k++)
	{
	  if (((*enop->args)[k]->etype != binop) ||
	      (((binopexp *)(*enop->args)[k])->op->opty != divbye))
	    {		// this one not a ratio
	      temp4 = (n_opexp *) copyexpr(lcd); // temp4 start of 
	      temp4->addarg((*enop->args)[k]);	// current term
	      plsnumer->addarg(temp4);		// of num sum
	    } 					
	  else		// kth term in enop is a ratio, lnum/lden
	    {
	      binopexp *ratio = (binopexp *) (*enop->args)[k];
	      if ((ratio->lhs->etype == n_op) &&
		  ((n_opexp *)ratio->lhs)->op->opty == multe)
		temp4 =(n_opexp *) ratio->lhs;
	      else				// will need to check
		{				// number args > 1
		  temp4 = new n_opexp (&mult);
//		 temp4->MKS.put(0,0,0,0,0); // REMOVE after fixing constructor 
		  temp4->addarg(ratio->lhs);
		}
	      // lden is a list of factors to NOT add when adding each factor
	      // of lcd to temp4
	      vector<expr *> *lden;
	      if ((ratio->rhs->etype != n_op)  ||
		  ((n_opexp *) ratio->rhs)->op->opty != multe)
		{
		  lden = new vector<expr *>;
		  lden->push_back(ratio->rhs);
		}			  
	      else lden = ((n_opexp *)ratio->rhs)->args;
	      int r;
	      for (q=0; q < lcd->args->size(); q++)
		{
		  for (r=0; r < lden->size(); r++)
		    if (equaleqs((*lden)[r],(*lcd->args)[q])) break;
		  if (r == lden->size()) 	// add this one, not in lden
		    temp4->addarg(copyexpr((*lcd->args)[q]));
		}
	      DBG(cout << "distfrac " << thisdbg << ": temp4 of term "
		  << k << " is " << temp4->getInfix() << endl;);
	      // above doesn't do multiple identical factors right
	      for (r=0; r < lden->size(); r++) (*lden)[r]->destroy();
	      delete lden;
	      DBG(cout << "again after deleting lden, temp4 is "
		  << temp4->getInfix() << endl;);
	      if (temp4->args->size() != 1) // can't be 0, right?
		plsnumer->addarg(temp4);
	      else 
		{
		  plsnumer->addarg((*temp4->args)[0]);
		  // rmed 2/4/01 need to check	  delete temp4->args;
		  delete temp4;
		}
	      DBG(cout << "distfrac " << thisdbg << ": At end of term "
		  << k << " plsnumer is " << plsnumer->getInfix() << endl
		  << " and lcd is " << lcd->getInfix() << endl;);
	    } 		// end of processing ratio term
	}		// end of loop over terms in sum
      DBG(cout << "distfrac " << thisdbg << ": At end of loop over terms "
	  << " plsnumer is " << plsnumer->getInfix() << endl
	  << " and lcd is " << lcd->getInfix() << endl;);
      binopexp *rete = new binopexp(&divby, plsnumer, lcd);
      if (lcd->args->size() == 1) // we are in a loop that knows lcdsize > 0
	{
	  rete->rhs = (*lcd->args)[0];
	  // rmed 2/4/01 need to check		  delete lcd->args;
	  delete lcd;
	}
      // does anything else need freeing up?
      ex = rete;
      DBGF(ENTFLP, cout << "distfrac " << thisdbg << 
	   " returns true with  " << ex->getInfix() << endl;);
      return(true);
    }
  DBG(cout << "distfrac "<< thisdbg << ": plus did not contain a ratio" 
      << endl;);
  delete lcd;
  DBGF(ENTFLP, cout << "distfrac " << thisdbg << ": did nothing, returns false "
       << ex->getInfix() << endl;);
  return(false);
}
