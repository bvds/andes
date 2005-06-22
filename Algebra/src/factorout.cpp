// factorout.cpp
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved

#include "decl.h"
#include "dbg.h"

using namespace std;
#define DBG(A) DBGF(FACTOUT,A)

/************************************************************************
 *	bool factorout(const expr * factor,int n, expr * & expression)	*
 *	  removes factor ^(n/2) from expression. 			*
 *	should be used only after finding n from numfactorsof		*
 *	Right now only works for factors which are physvar		*
 ************************************************************************/
bool factorout(const expr * factor,int n, expr * & expression)
{
  int k, q;

  binopexp * binexpr;
  n_opexp * nopexpr;

#if WITHDBG
  unsigned long thisdbg = ++dbgnum;	// recursive calls for debug
#endif

  expr *exprcopy = copyexpr(expression); // keep expression unchanged until
				// verify we can do it.
  switch(factor->etype) {
  case numval:
  case function:				// factor is a function
  case binop:			// factor is a binop
  case n_op:
    throw(string(
	"Sorry, factorout currently only works with physvar factors"));
  case physvart:		// factor is a physvar
    DBG( cout << "FACTOUT " << thisdbg << ": called for " 
	 << factor->getInfix() << " to power "
	 << n << "/2 on " << exprcopy->getInfix() << endl;);
    switch(exprcopy->etype){
    case numval:
      if (((numvalexp *)expression)->value == 0.) goto fixup;
      DBG(cout << "FACTOUT " << thisdbg 
	  << ": returning false on physvar factor of numval" << endl;);
      goto abort;
    case physvart:
      if ( ( ((physvarptr *)exprcopy)->varindex == 
	     ((physvarptr *)factor)->varindex ) && (n == 2) )
	{
	  DBG(cout << "FACTOUT " << thisdbg 
	      << ": factored physvar out of physvar" << endl;);
	  exprcopy = new numvalexp(1.);
	  exprcopy->MKS = factor->MKS; // will be removed in fixup
	  goto fixup;
	}
      else {
	DBG(cout << "FACTOUT " << thisdbg 
	    << ": failed to factor physvar " << factor->getInfix()
	    << " out of physvar " << exprcopy->getInfix()
	    << " with n = " << n << endl;); 
	goto abort;
      }
    case function:
      if (((functexp *)exprcopy)->f->opty == sqrte)
	if (factorout(factor,2*n,((functexp *)exprcopy)->arg))
	  goto fixup;
	else goto abort;
      if (((functexp *)exprcopy)->f->opty == abse) 
	{
	  if (factorout(factor,n,((functexp *)exprcopy)->arg))
	    goto fixup;
	  else goto abort;
	}
      else
	{
	  DBG(cout << "FACTOUT " << thisdbg 
	      << ": returning false on physvar in function" << endl;);
	  goto abort;
	  
	}
    case binop:
      binexpr = (binopexp *)exprcopy;
      DBG(cout << "FACTOUT " << thisdbg 
	       << ": working on physvar factor of binop" << endl;);
      if (binexpr->op->opty == divbye)
	if (factorout(factor, n, ((binopexp *)exprcopy)->lhs))
	  goto fixup;
	else goto abort;
      if (binexpr->op->opty == topowe)
  	{
  	  if (binexpr->rhs->etype != numval) goto abort;
  	  if (((numvalexp *)binexpr->rhs)->value <=0) goto abort;
  	  if (lookslikeint(n/((numvalexp *)binexpr->rhs)->value,q)) // what's
	    if (factorout(factor,q,binexpr->lhs)) goto fixup; 	   //  this? 
	  if (equaleqs(factor,binexpr->lhs)) {
	    ((numvalexp *)binexpr->rhs)->value  -= .5 * n;
	    goto fixup;
	  }
	  goto abort;
  	}
      if (binexpr->op->opty == equalse)
	{
	  if (!factorout(factor,n,binexpr->lhs)) goto abort;
	  if (!factorout(factor,n,binexpr->rhs)) goto abort;
	  goto fixup;
	}
      DBG(cout << "FACTOUT " << thisdbg 
	  << ": returning false from physvar on binop" << endl;);
     goto abort;		// only binops considered are ^ and / and =
    case n_op:
      nopexpr = (n_opexp *) exprcopy;
      if (nopexpr->op->opty == pluse)
  	{
	  DBG(cout << "FACTOUT " << thisdbg 
	      << ": physvar on " << nopexpr->getInfix() << endl;);
  	  for (k=0; k < nopexpr->args->size(); k++)
  	    if (!factorout(factor, n,(*nopexpr->args)[k])) goto abort;
	  goto fixup;
  	}
      if (nopexpr->op->opty == multe)
  	{
	  DBG(cout << "FACTOUT " << thisdbg 
	      << ": physvar on " << nopexpr->getInfix() << endl;);
	  int qleft = n;
	  for (k=0; k < nopexpr->args->size(); k++)
	    {
	      q = numfactorsof(factor, (*nopexpr->args)[k]);
	      if (q == 0) continue;
	      if (!factorout(factor,min(q,qleft),(*nopexpr->args)[k]))
		goto abort;
	      qleft = qleft - min(q,qleft);
	      if (qleft == 0) goto fixup;
	    }
	  goto abort;
  	}
      throw(string("unknown n_op as expression in factorout"));
    case unknown:
    case fake:
    default:
      throw(string("unknown expr as expression in factorout"));
    } // end of case physvart
  case unknown:
  case fake:
  default:
    throw(string("factor sent to factorout unknown expr"));
  }
 abort:
  exprcopy->destroy();
  DBG(cout << "FACTOUT " << thisdbg << ": returning false" << endl;);
  return(false);
 fixup:
  expression->destroy();
  exprcopy->MKS += factor->MKS * (-0.5*n);
  expression = exprcopy;
  DBG(cout << "FACTOUT " << thisdbg << ": returning true" << endl;);
  return(true);
}
