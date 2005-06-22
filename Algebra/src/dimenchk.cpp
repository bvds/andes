// dimenchk.cpp
/************************************************************************
 * expr * dimenchk(const bool fix, expr * & ex)				*
 *	returns a pointer to the first dimensionally inconsistent part  *
 *	found, or NULL if everything checks out.			*
 *    if fix == true, repairs unknown expressions when possible, and    *
 *      returns non-null only if these cannot be made consistent or if  *
 *	doing so is not unique						*
 *    NOTE: a null return does not guarantee that top level has known   *
 *    7/9/01: Apparently it is being called on non-eqnumsimped expr,	*
 *      which is a problem for topowe rhs. Added an eqnumsimp there	*
 ************************************************************************/

#include <stdio.h>
#include "decl.h"
#include "extoper.h"
#include "dbg.h"

#define DBG(A) DBGF(DIMCHK,A)

expr * dimenchk(const bool fix, expr * & ex)
{
  int j, k, q;
  expr * trouble = (expr *) NULL;

#if WITHDBG
  unsigned long thisdbg = ++dbgnum;	// recursive calls for debug
#endif
  DBG( cout << "Starting dimenchk call " << thisdbg << " on " 
       << ex->getInfix() << endl; ex->dbgprint(10));
  if(ex->etype==function){
    DBG( cout << "Function is " << ((functexp *) ex)->f->printname <<endl );
  }
  switch(ex->etype)
    {
    case numval:
    case physvart:
      DBG( cout << "Returning NULL from call " << thisdbg << endl);
      return((expr *) NULL);
    case function:
      {
	functexp * fn = (functexp *) ex; 
	switch(fn->f->opty)
	  {
	  case sine:
	  case cose:
	  case tane:
	  case expe:
	  case lne:
	  case log10e:
	    {
	      if (fn->arg->MKS.unknp() && fix)
		fn->arg->MKS.put(0,0,0,0,0);
	      if (trouble == (expr *) NULL) trouble = dimenchk(fix,fn->arg);
	      else dimenchk(fix,fn->arg);
	      if (fn->MKS.unknp() && fix)
		fn->MKS.put(0,0,0,0,0);
	      if ((trouble == (expr *) NULL) &&
		  ((!fn->arg->MKS.zerop()) ||
		   (!fn->MKS.zerop()))) trouble = fn;
	      DBG( if (trouble == (expr *) NULL) 
		   cout << "Returning NULL";
		   else cout << " Non-homogenous function units error " << 
		   trouble->getInfix();
		   cout << " from call " << thisdbg << endl);
	      return(trouble);
	    }
	  case sqrte:
	    {
	      if (!fn->MKS.unknp()) 
		if (fn->arg->MKS.unknp() && fix) 
		  fn->arg->MKS = fn->MKS * 2.;
	      expr * temp = dimenchk(fix, fn->arg);
	      if (temp != (expr *) NULL) {
		DBG( cout << "square root units error " << temp->getInfix()
		   << " from call " << thisdbg << endl);
		return(temp);
	      }
	      if (fn->MKS.unknp() && fix && !fn->arg->MKS.unknp())
		fn->MKS = fn->arg->MKS * 0.5;
	      if ((fn->MKS * -2. + fn->arg->MKS).zerop()) {
		DBG(if (trouble == (expr *) NULL) cout << "Returning NULL";
		else cout << "square root units error "
			  << trouble->getInfix();
		     cout << " from call " << thisdbg << endl);
		return(trouble);
	      }
	      else {
		DBG( cout << "square root units error " << fn->getInfix()
		   << " from call " << thisdbg << endl);
		return(fn);
	      }
	    }
	  case abse:
	    {
	      if (!fn->MKS.unknp()) 
		if (fn->arg->MKS.unknp() && fix) 
		  fn->arg->MKS = fn->MKS;
	      expr * temp = dimenchk(fix, fn->arg);
	      if (temp != (expr *) NULL) { 
		DBG( cout << "abs units error " << temp->getInfix()
		   << " from call " << thisdbg << endl);
		return(temp);
	      }
	      if (fn->MKS.unknp() && fix && !fn->arg->MKS.unknp())
		fn->MKS = fn->arg->MKS;
	      if ((fn->MKS * -1. + fn->arg->MKS).zerop()) {
		DBG( if (trouble == (expr *) NULL) cout << "Returning NULL";
		     else cout << "abs units error " 
		     << trouble->getInfix();
		     cout << " from call " << thisdbg << endl);
		return(trouble);
	      }
	      else {
		DBG( cout << "abs units error " << fn->getInfix()
		   << " from call " << thisdbg << endl);
		return(fn);
	      }
	    }
	  default:
	    throw(string("unknown function in dimenchk"));
	  }
      }	// end case function
    case binop:
      {
	binopexp * binex = (binopexp *) ex;
	switch(binex->op->opty)
	  {
	  case divbye:
	    //strategy: if two of the three have known dimension, can set 3rd
	    {
	      int numunkn = 0;
	      if (binex->MKS.unknp()) numunkn++;
	      if (binex->lhs->MKS.unknp()) numunkn++;
	      if (binex->rhs->MKS.unknp()) numunkn++;
	      if (numunkn == 1) {
		if (binex->MKS.unknp()) binex->MKS = 
				binex->lhs->MKS + binex->rhs->MKS * -1.;
		if (binex->lhs->MKS.unknp()) binex->lhs->MKS = 
				binex->MKS + binex->rhs->MKS;
		if (binex->rhs->MKS.unknp()) binex->rhs->MKS = 
				binex->lhs->MKS + binex->MKS * -1.; }
	      if (trouble == (expr *) NULL) trouble = dimenchk(fix,binex->lhs);
	      else dimenchk(fix,binex->lhs);
	      if (trouble == (expr *) NULL) trouble = dimenchk(fix,binex->rhs);
	      else dimenchk(fix,binex->rhs);
	      dimens tempd = binex->MKS + binex->rhs->MKS + 
		binex->lhs->MKS * -1.;
	      if (tempd.zerop()) {
		DBG( if (trouble == (expr *) NULL) cout << "Returning NULL";
		     else cout << "division units error "
		     << trouble->getInfix();
		     cout << " from call " << thisdbg << endl);
		return(trouble);
	      }
	      if (!tempd.unknp()) {
		DBG( cout << "division units error " << binex->getInfix()
		   << " from call " << thisdbg << endl);
		return(binex);
	      }
	      numunkn = 0;
	      if (binex->MKS.unknp()) numunkn++;
	      if (binex->lhs->MKS.unknp()) numunkn++;
	      if (binex->rhs->MKS.unknp()) numunkn++;
	      if (numunkn == 1) {
		if (binex->MKS.unknp()) binex->MKS = 
				binex->lhs->MKS + binex->rhs->MKS * -1.;
		if (binex->lhs->MKS.unknp()) binex->lhs->MKS = 
				binex->MKS + binex->rhs->MKS;
		if (binex->rhs->MKS.unknp()) binex->rhs->MKS = 
				binex->lhs->MKS + binex->MKS * -1.; 
	      }
	      DBG( if (trouble == (expr *) NULL) cout << "Returning NULL";
		   else cout << "division units error" << trouble->getInfix();
		   cout << " from call " << thisdbg << endl);
	      return(trouble);
	    }
	  case topowe:		// NO We forgot to dimenchk parts
	    if (binex->rhs->MKS.unknp()) binex->rhs->MKS.put(0,0,0,0,0);
	    // BvdS:  I don't see why one wants to demand "tight" units here:
	    eqnumsimp(binex->rhs,true);	// put in binop form, tight units
	    if (trouble == (expr *) NULL) trouble = dimenchk(fix,binex->rhs);
	    else dimenchk(fix,binex->rhs);
	    if (!binex->rhs->MKS.zerop()) {
	      DBG( cout << "error:  exponent must be dimensionless " 
		   << binex->getInfix() << " from call " << thisdbg << endl);
	      return(binex);
	    }
	    // exponent is a special number that we like
	    if (lookslikeint(12. *((numvalexp *)binex->rhs)->value, q)) {
	      if ((binex->MKS.unknp()) && !binex->lhs->MKS.unknp())
		binex->MKS = 
		  binex->lhs->MKS * ((numvalexp *)binex->rhs)->value;
	      if ((binex->lhs->MKS.unknp()) && !binex->MKS.unknp())
		binex->lhs->MKS = binex->MKS * 
		  (1./((numvalexp *)binex->rhs)->value);
	      if (trouble == (expr *) NULL) trouble = dimenchk(fix,binex->lhs);
	      else dimenchk(fix,binex->lhs);
	      if ((!binex->lhs->MKS.unknp()) && !binex->MKS.unknp())
		if (!( binex->lhs->MKS * ((numvalexp *)binex->rhs)->value  ==
		       binex->MKS)) {
		  DBG( cout << "power units inconsistent " << binex->getInfix()
		       << " from call " << thisdbg << endl);
		  return(binex); 
		}
	    }
	    // exponent is algebraic or a number we don't like
	    else {
	      if (binex->lhs->MKS.unknp()) binex->lhs->MKS.put(0,0,0,0,0);
	      if (trouble == (expr *) NULL) trouble = dimenchk(fix,binex->lhs);
	      else dimenchk(fix,binex->lhs);
	      if (binex->MKS.unknp()) binex->MKS.put(0,0,0,0,0);
	      if ((!binex->MKS.zerop()) || (!binex->lhs->MKS.zerop())) {
		DBG( cout << "error:  mantissa must be dimensionless for "
		     "this exponent " << binex->getInfix()
		     << " from call " << thisdbg << endl);
		return(binex); 
	      }
	    }
	    DBG( if (trouble == (expr *) NULL) cout << "Returning NULL";
		 else cout <<"power units error "<<trouble->getInfix();
		 cout << " from call " << thisdbg << endl);
	    return(trouble); 
	  case equalse:
	  case grte:
	  case gree:
	    {
	      if (!binex->rhs->MKS.unknp()) {
		if (binex->lhs->MKS.unknp()) binex->lhs->MKS = binex->rhs->MKS;
		if (binex->MKS.unknp()) binex->MKS = binex->rhs->MKS; }
	      else if (!binex->lhs->MKS.unknp()) {
		if (binex->rhs->MKS.unknp()) binex->rhs->MKS = binex->lhs->MKS;
		if (binex->MKS.unknp()) binex->MKS = binex->lhs->MKS; }
	      else if (!binex->MKS.unknp()) {
		if (binex->lhs->MKS.unknp()) binex->lhs->MKS = binex->MKS;
		if (binex->rhs->MKS.unknp()) binex->rhs->MKS = binex->MKS; }
	      expr * temp = dimenchk(fix,binex->lhs);
	      if ((trouble == (expr *) NULL) &&
		  (temp != (expr *) NULL)) trouble = temp;
	      temp = dimenchk(fix,binex->rhs);
	      if ((trouble == (expr *) NULL) &&
		  (temp != (expr *) NULL)) trouble = temp;
	      if (!binex->rhs->MKS.unknp()) {
		if (binex->lhs->MKS.unknp()) binex->lhs->MKS = binex->rhs->MKS;
		if (binex->MKS.unknp()) binex->MKS = binex->rhs->MKS; }
	      else if (!binex->lhs->MKS.unknp()) {
		if (binex->rhs->MKS.unknp()) binex->rhs->MKS = binex->lhs->MKS;
		if (binex->MKS.unknp()) binex->MKS = binex->lhs->MKS; }
	      if (!binex->rhs->MKS.unknp() && (
		       !(binex->lhs->MKS == binex->rhs->MKS)  ))
		  // || !(binex->MKS == binex->rhs->MKS)))  ???
		{
		  DBG(cout << " equation units error from call " 
		      << thisdbg << endl; binex->dbgprint(1));
		  return(binex);
		}
	      DBG( cout << "returning equation from call " 
		   << thisdbg << endl; binex->dbgprint(1));
	      return(trouble);
	    }
	  default:
	    throw(string("unknown binop in dimenchk"));
	  }
      }	// end of case binop
    case n_op:
      {
	n_opexp * nopex = (n_opexp *) ex;
	int numunk=0;
	DBG( cout << "n_op on " << ex->getInfix() << endl);
	for (j=0; j< nopex->args->size(); j++)
	  if ((*nopex->args)[j]->MKS.unknp()) numunk++;
	if (nopex->MKS.unknp()) numunk++;
	DBG( cout << "n_op " << numunk
	     << " unkn parts, size " << nopex->args->size() << endl);
	// numunk is the number of args with unknown dimensions, plus
	// one if the sum/prod also has unknown dimensions. For plus, if
	// any of these is known, all the others must be the same. For
	// mult, if one of these is unknown it can be determined, but if
	// more are unknown we can't determine them. If we can't fix any
	// dimens yet, check the args now
	if ((numunk > nopex->args->size()) || 
	    ((nopex->op->opty == multe) && numunk > 1))
	  {			// can't fix dimen without dimenchk args
	    for (j=0; j < nopex->args->size(); j++)
	      {
		bool couldfix = (*nopex->args)[j]->MKS.unknp();
		if (trouble == (expr *) NULL) 
		  trouble = dimenchk(fix,(*nopex->args)[j]);
		else dimenchk(fix,(*nopex->args)[j]);
		if (couldfix && !(*nopex->args)[j]->MKS.unknp()) numunk--;
	      }
	    DBG( cout << "n_op no fix before doing parts, now " 
		 << ex->getInfix() << ", numunk = " << numunk  << endl);
	  } // end of had too many to fix, so dimenchk'ed args
	dimens tempds;
	bool foundone = false;
 	switch(nopex->op->opty)
	  {
	  case pluse:
	    if (numunk > nopex->args->size()) {
	      DBG( cout << "plus all unkn " << endl);
	      return(trouble);
	    }
	    DBG( if (numunk != 0)
		 cout << "plus should fix " << endl;
	         else cout << "plus all parts known " << endl);
	    if (!nopex->MKS.unknp()) {
	      foundone = true;
	      k = -1;
	      tempds = nopex->MKS;}
	    for (j=0; j < nopex->args->size(); j++)
	      if ((*nopex->args)[j]->MKS.unknp())
		{ if (foundone) (*nopex->args)[j]->MKS = tempds; }
	      else {		// found a term with known dimen
		if (foundone) {
		  if (!( (*nopex->args)[j]->MKS == tempds)) {
		    DBG( cout << "plus units error from call " 
			 << thisdbg << endl; nopex->dbgprint(6));
		    return(nopex); } }
		else {
		  foundone = true;
		  k = j;
		  tempds = (*nopex->args)[j]->MKS;
		} }
	    if (!foundone) throw(string("impossible in dimenchk") +
				" known known not found");
	    for (j=0; j < k; j++)
	      if ((*nopex->args)[j]->MKS.unknp())
		(*nopex->args)[j]->MKS = tempds;
	      else 
		if (!( (*nopex->args)[j]->MKS == tempds)) {
		  DBG( cout << "plus units error " << nopex->getInfix()
		       << " from call " << thisdbg << endl);
		  return(nopex);
		}
	    DBG(cout << "plus end arg loop" << endl; nopex->dbgprint(8));
	    if (nopex->MKS.unknp()) nopex->MKS = tempds;
	    else if (!(nopex->MKS == tempds)) { // note - this ought to recurse
	      DBG( cout << "Returning " << nopex->getInfix()
		   << " from dimenchk PROBLEM " << thisdbg << endl;);
	      return(nopex);
	    }
	    for (j=0; j < nopex->args->size(); j++)
	      if (trouble == (expr *) NULL) 
		trouble = dimenchk(fix,(*nopex->args)[j]);
	      else dimenchk(fix,(*nopex->args)[j]);
	    DBG( if (trouble == (expr *) NULL) cout << "Returning NULL";
		 else cout << "Returning " << trouble->getInfix();
		 cout << " from dimenchk " << thisdbg << endl;);
	    return(trouble);
	  case multe:
	    if (numunk > 1) {
	      DBG( cout << "Dimenchk mult not known " << numunk << endl;);
	      DBG( if (trouble == (expr *) NULL) cout << "Returning NULL";
		   else cout << "Returning " << trouble->getInfix();
		   cout << " from dimenchk " << thisdbg << endl;);
	      return(trouble);
	    }
	    tempds.put(0,0,0,0,0);
	    k = -1;
	    for (j=0; j < nopex->args->size(); j++) {
	      if (trouble == (expr *) NULL) 
		trouble = dimenchk(fix,(*nopex->args)[j]);
	      else dimenchk(fix,(*nopex->args)[j]);
	      if ((*nopex->args)[j]->MKS.unknp()) k = j;
	      else tempds = tempds + (*nopex->args)[j]->MKS;
	    }
	    DBG( 
		cout << "Dimenchk mult end arg loop, numunk = " << numunk
		<< ", k=" << k << endl;
		nopex->dbgprint(8));
	    if (nopex->MKS.unknp()) { 
	      nopex->MKS = tempds; 
	      DBG( if (trouble == (expr *) NULL) cout << "Returning NULL";
		   else cout << "Returning " << trouble->getInfix();
		   cout << " from dimenchk " << thisdbg << endl;);
	      return(trouble);
	    }
	    if (numunk == 0) {
	      trouble = ((nopex->MKS == tempds) ? trouble : nopex);
	      DBG( if (trouble == (expr *) NULL) cout << "Returning NULL";
		   else cout << "Returning " << trouble->getInfix();
		   cout << " from dimenchk " << thisdbg << endl;);
	      return(trouble);
	    }
	    if ((k>=0) && k < nopex->args->size())
	    (*nopex->args)[k]->MKS = nopex->MKS + tempds * -1.;
	    DBG( cout << "Dimenchk mult returning, with nopex" << endl; 
		 nopex->dbgprint(8);
		 );
	    DBG( if (trouble == (expr *) NULL) cout << "Returning NULL";
		 else cout << "Returning " << trouble->getInfix();
		 cout << " from dimenchk " << thisdbg << endl);
	    return(trouble);
	  default:
	    throw(string("unknown n_op in dimenchk"));
	  } // end of switch on n_op type
      }	// end of case n_op
      case unknown:
      case fake:
      default:
	throw(string("unknown expr in dimenchk"));
    }
}


