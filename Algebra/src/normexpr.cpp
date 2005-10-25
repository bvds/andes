// normexpr.cpp	
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved
//
//	kmult
//	normexpr
//	uptonum

#include <string>
#include <math.h>
#include "decl.h"
#include "extoper.h"
#include "dbg.h"
using namespace std;

#define DBG(A) DBGF(NORMEX,A)
#define DBGM(A) DBGFM(NORMEX,A)

/************************************************************************
 * void kmult(expr * & ex, const double k)				*
 *	multiplies expression ex by dimensionless k			*
 *	answer is eqnumsimped and flattened, I think, if it had been	*
 ************************************************************************/
void kmult(expr * & ex, const double k)
{
  numvalexp * nv = new numvalexp(k);
  nv->MKS.put(0,0,0,0,0);
  kmult(ex, nv);
  return;
}

/************************************************************************
 * void kmult(expr * & ex, numvalexp * nv)				*
 *	multiplies expression ex by numval nv, and destroys nv		*
 *	answer is eqnumsimped and flattened, I think, if it had been	*
 ************************************************************************/
void kmult(expr * & ex, numvalexp * nv) 
{
  DBG( cout << "entering kmult on "<< ex->getInfix() 
            << " with multiplier " << nv->getInfix() << endl);
  DBGM(ex->dbgprint(4));
 
#if AW_EXP
  // AW: add special case for multiplication by zero
  // code for multiplication below doesn't simplify this
  if (nv->value == 0.) {
	  expr * retexp = new numvalexp(0.);
	  retexp->MKS = nv->MKS + ex->MKS;
	  ex = retexp;        // AW: prev contents not destroyed??
	  DBG( cout << "kmult returns " << ex->getInfix()<< endl);
	  DBGM(ex->dbgprint(4));
	  return;
  }
#endif // AW_EXP
  // AW: Also want special case for multiplication by 1?
  // that also not simplified by code below.

  n_opexp * retexp;
  // if ex not a multiplication, add factor and simplify
  if ((ex->etype != n_op) || ((n_opexp *)ex)->op->opty != multe) 
    {
      retexp = new n_opexp(&mult);
      retexp->addarg(nv);
      retexp->addarg(ex);
      ex = retexp;			// AW: prev contents not destroyed??
      eqnumsimp(ex,true);
      flatten(ex);
      if (ex->etype == n_op) 
	  {  retexp = (n_opexp *)ex;
		  cleanup(retexp);
		  ex = retexp;
	  }
      DBG( cout << "returning from kmult with " << ex->getInfix() << endl);
      DBGM(ex->dbgprint(4));
      return;
    }
  // else ex is a multiplication:
  retexp = (n_opexp * ) ex;
  
  // if doesn't begin with numval, just add factor:
  if (retexp->args->size() == 0 || (*retexp->args)[0]->etype != numval) 
    {
      retexp->addarg(nv);
      cleanup(retexp);
      ex = retexp;              // AW: prev contents not destroyed??
      DBG( cout << "kmult returns " << ex->getInfix()<< endl);
      DBGM(ex->dbgprint(4));
      return;
    }
  // else multiplication begins with numval: multiply by factor
  ((numvalexp *)(*retexp->args)[0])->value *= nv->value;
  (*retexp->args)[0]->MKS += nv->MKS; // Update units of numval.
  retexp->MKS += nv->MKS; // Update units of the multiply.
  nv->destroy();
  DBG( cout << "kmult returns " << ex->getInfix()<< endl);
  DBGM(ex->dbgprint(4));
  return;
}


/******************************************************************************
 *  numvalexp * normexpr(expr * & ex)					      *
 *	returns a new numval factor which multiples the returned 	      *
 *	normalized ex so that the product is the original ex		      *
 *  An expression is normalized if it is eqnumsimped, flattened, and	      *
 *	if a mult n_op, has no numval factor				      *
 *	if a plus n_op, with numval, the numval is 1			      *
 *	       if without a numval, first term is normalized		      *
 *	if a numval = zero, don't do anything, return NULL		      *
 *	if a numval, it is 1						      *
 *	if a physvar, it is always normalized				      *
 *	if a function of type sin, cos, tan, it is normalized		      *
 *	    of type exp of sum, sum has no numval, 			      *
 *		    exp of numval is not normalized			      *
 *		    all other exp are normalized			      *
 *	    of type lne of mult, if mult is normalized			      *
 *	    of type sqrt, arg is normalized (or - normalized)		      *
 *	    of type abs, arg is normalized				      *
 *	if a binop of type topow, lhs is normalized (or - normalized)	      *
 *		of type divbye, bot lhs and rhs are normalized		      *
 *		of type equalse, rhs is 0 and lhs is normalized		      *
 *		of type grte, gree, rhs=0 and lhs normalized (or - normalized)*
 *****************************************************************************/
numvalexp * normexpr(expr * & ex)
{
  numvalexp *answer;
  int k, q;
  binopexp * binptr;
  n_opexp * nopptr;
  functexp * fptr;
#ifdef WITHDBG // for debugging
  static int normcall=0;    
  int thiscall=normcall++;  
#endif

  DBG(cout << "normexpr call " << thiscall << " for " 
      << ex->getInfix() << endl);

  eqnumsimp(ex,true);
  flatten(ex);
  switch(ex->etype)
    {
    case numval:	     
      if (((numvalexp *) ex)->value == 0)
	answer = NULL;  // for zero, don't do anything.
      else {
	answer = (numvalexp *) ex;
	ex = (expr *) new numvalexp(1.);
	ex->MKS.put(0,0,0,0,0);
      }
      DBG(cout << "normexpr call " << thiscall << " returning " 
	  << ex->getInfix() << endl); 
      return(answer);   // answer is never zero
    case physvart:
      answer = new numvalexp(1.);
      answer->MKS.put(0,0,0,0,0);
      DBG(cout << "normexpr call " << thiscall << " returning " 
	  << ex->getInfix() << endl); 
      return(answer);
    case function:
      fptr = (functexp *) ex;
      switch(fptr->f->opty)
	{
	case sine:
	case cose:
	case tane:
	  answer = new numvalexp(1.);
	  answer->MKS.put(0,0,0,0,0);
	  DBG(cout << "normexpr call " << thiscall << " returning " 
	      << ex->getInfix() << endl); 
	  return(answer);
	case expe:
	  switch (fptr->arg->etype) {
	  case numval:
	    throw(string("eqnumsimped flattened expr shouldnt have e^numval"));
	  case n_op:
	    {
	      n_opexp * argnop = (n_opexp *)fptr->arg;
	      if (argnop->op->opty == multe)
		{
		  answer = new numvalexp(1.);
		  answer->MKS.put(0,0,0,0,0);
		  DBG(cout << "normexpr call " << thiscall << " returning " 
		      << ex->getInfix() << endl); 
		  return(answer);
		}
	      cleanup(argnop);
	      if (argnop->args->size() == 0) // did cleanup make 0?
		{
		  ex = new numvalexp(0);
		  answer = new numvalexp(1.);
		  answer->MKS.put(0,0,0,0,0);
		  DBG(cout << "normexpr call " << thiscall << " returning " 
		      << ex->getInfix() << endl); 
		  return(answer);
		}
	      if ((*argnop->args)[0]->etype == numval)
		{
		  answer = new numvalexp(
					 exp(((numvalexp *)(*argnop->args)[0])->value));
		  answer->MKS.put(0,0,0,0,0);
		  ((numvalexp *)(*argnop->args)[0])->value = 0;
		  fptr->arg = argnop; // I don't know, it could have changed
		  flatten(fptr->arg);
		  DBG(cout << "normexpr call " << thiscall << " returning " 
		      << ex->getInfix() << endl); 
		  return(answer);
		}
	      else {	// ?? need to look at this, I got lost here
		answer = new numvalexp(1.);
		answer->MKS.put(0,0,0,0,0);
		DBG(cout << "normexpr call " << thiscall << " returning " 
		    << ex->getInfix() << endl); 
		return(answer);
	      }
	    }
	  default:
	    throw(string("Maybe unknown function in normexpr"));
	  } // end of switch over arg types for exp
	case lne:
	case log10e:
	  answer = normexpr(fptr->arg);
	  throw(string("I haven't finished normalizing logs"));
	case sqrte:
	  answer = normexpr(fptr->arg);
	  if (answer->value < 0) {kmult(fptr->arg,-1.); answer->value *= -1.;}
	  answer->value = sqrt(answer->value);
	  DBG(cout << "normexpr call " << thiscall << " returning " 
	      << ex->getInfix() << endl); 
	  return(answer);
	case abse:
	  answer = normexpr(fptr->arg);
	  answer->value = fabs(answer->value);
	  DBG(cout << "normexpr call " << thiscall << " returning " 
	      << ex->getInfix() << endl); 
	  return(answer);
	default:
	  throw(string("unknown function in normexpr"));
	} // end of switch over function types
    case binop:
      binptr = ( binopexp * ) ex;
      switch(binptr->op->opty)
	{
	case divbye:
	  {
	    answer = normexpr(binptr->lhs);
	    numvalexp *denom = normexpr(binptr->rhs);	    
	    if(answer){
	      answer->value = answer->value/denom->value;
	      denom->MKS *= -1.;
	      answer->MKS += denom->MKS;
	    } // zero in numerator, don't do anything
	    denom->destroy();
	    DBG(cout << "normexpr call " << thiscall << " returning " 
		<< ex->getInfix() << endl); 
	    return(answer);
	  }
	case topowe:
	  if (binptr->rhs->etype != numval) {
	    answer = new numvalexp(1.);
	    answer->MKS.put(0,0,0,0,0);
	    DBG(cout << "normexpr call " << thiscall << " returning " 
		<< ex->getInfix() << endl); 
	    return(answer);
	  }
	  answer = normexpr(binptr->lhs);
	  if (answer->value < 0) 
	    if (!lookslikeint(((numvalexp *)binptr->rhs)->value,q))
	      {
		kmult(binptr->lhs, -1.);
		answer->value *= -1.;
	      }
	  if (answer->value < 0) {
	    answer->value = pow(-answer->value,q);
	    if (q%2 == 1) answer->value *= -1.;
	  }
	  else answer->value = pow(answer->value,
				   ((numvalexp *)binptr->rhs)->value);
	  answer->MKS *=((numvalexp *)binptr->rhs)->value;
	  DBG(cout << "normexpr call " << thiscall << " returning " 
	      << ex->getInfix() << endl); 
	  return(answer);
	case equalse:
	  if ((binptr->rhs->etype != numval) || 
	      ((numvalexp *)binptr->rhs)->value != 0)
	    {
#if 0
	      DBG( cout << "normexpr: about to call apluskb( " 
		   << binptr->lhs->getInfix() << " , " 
		   << binptr->rhs->getInfix() << " , -1.)" << endl);
#endif
	      apluskb(binptr->lhs,binptr->rhs, -1.);
#if 0
	      DBG( cout << "Normexpr: a+kb output first arg "
		   << binptr->lhs->getInfix() << endl;);
#endif
	      binptr->rhs->destroy();
	      binptr->rhs = new numvalexp(0);
	      eqnumsimp(binptr->lhs,true);
	      flatten(binptr->lhs);
#if 1
	      DBG(cout << "normexpr call " << thiscall << " binptr->lhs="
		  << binptr->lhs->getInfix() << endl);
#endif
	    } 
	  answer = normexpr(binptr->lhs); 
	  binptr->rhs->MKS = binptr->lhs->MKS;  // just in case
	  DBG(cout << "normexpr call " << thiscall << " returning " 
	      << answer->getInfix() << " and ex=" << ex->getInfix() << endl); 
	  return(answer);
	case grte:
	case gree:
	  if ((binptr->rhs->etype != numval) || 
	      ((numvalexp *)binptr->rhs)->value != 0)
	    {
	      apluskb(binptr->lhs,binptr->rhs, -1.);
	      binptr->rhs->destroy();
	      binptr->rhs = new numvalexp(0);
	      eqnumsimp(binptr->lhs,true);
	      flatten(binptr->lhs);
	    }
	  answer = normexpr(binptr->lhs);
	  if (answer->value < 0) {
	    kmult(binptr->lhs,-1.); 
	    answer->value *= -1.;}
	  binptr->rhs->MKS = binptr->lhs->MKS;
	  DBG(cout << "normexpr call " << thiscall << " returning " 
	      << ex->getInfix() << endl); 
	  return(answer);
	default:
	  throw(string("unknown binop in normexpr"));
	} // end of switch over binop types
    case n_op:
      nopptr = (n_opexp *) ex;
#if 0
      DBG( cout << "normexpr n_op " << ex->getInfix() << endl; );
#endif
      cleanup(nopptr);
#if 0
      DBG( cout << "n_op cleaned up in normexpr " << ex->getInfix() << endl;);
#endif
      if (nopptr->args->size()==0)
	throw(string("I didn't think null n_op could occur in normfact"));
      if (nopptr->op->opty == multe)
	{
#if 0
	  DBG( cout << "n_op is multe in normexpr " << endl; );
#endif
	  if ((*nopptr->args)[0]->etype != numval) 
	    {
	      ex = nopptr;
	      answer = new numvalexp(1.);
	      answer->MKS.put(0,0,0,0,0);
	      DBG(cout << "normexpr call " << thiscall << " returning " 
		  << ex->getInfix() << endl); 
	      return(answer);
	    }
	  else
	    {
	      answer = (numvalexp *)(*nopptr->args)[0];
	      for (k=0; k+1<nopptr->args->size(); k++)
		(*nopptr->args)[k] = (*nopptr->args)[k+1];
	      nopptr->args->pop_back();
	      nopptr->MKS += answer->MKS * -1.; // BvdS: adjust units, too
	      ex = nopptr;
	      eqnumsimp(ex,true);
	      DBG(cout << "normexpr call " << thiscall << " returning " 
		  << ex->getInfix() << endl); 
	      return(answer);
	    }
	}
      else			// must be plus
	{
	  expr * tempexp = copyexpr((*nopptr->args)[0]);
	  answer = normexpr(tempexp);
	  tempexp->destroy();
	  ex = nopptr;
	  numvalexp *nv = new numvalexp(1/answer->value);
	  nv->MKS = answer->MKS;
	  nv->MKS *= -1.;
	  kmult(ex,nv);
	  DBG(cout << "normexpr call " << thiscall << " returning " 
	      << ex->getInfix() << endl); 
	  return(answer);
	} // end of plus n_op
    case unknown:
    case fake:
    default:
	  throw(string("unknown expr type in normexpr"));
    } // end of switch over types of expressions.
}

/************************************************************************
 *  bool uptonum(const expr * const ans, const expr * const term, 	*
 *	  numvalexp * & coef)						*
 *	if ans and term are the same up to a numerical coefficient, 	*
 *	sets coef so that ans = coef * term and returns true. 		*
 *	returns false, not sure about coef				*
 *    coef must be NULL on entry. It may be set to new numvalexp	*
 *	if uptonum returns true
 *    0 and 0 returns true,  coef=NULL           
 *    x and 0 returns false, coef=NULL
 *    0 and x returns true,  coef=0
 ************************************************************************/
bool uptonum(const expr * const ans, const expr * const term, 
	     numvalexp * & coef)
{				
  bool answer;
  expr * a1 = copyexpr(ans);
  expr * a2 = copyexpr(term);
  numvalexp * ansfact = normexpr(a1);
  DBG(cout << "Uptonum: normexpr returned factor " << ansfact->getInfix() 
      << endl << "         times a1: " << a1->getInfix() << endl);

  numvalexp * termfact  = normexpr(a2);
  DBG(cout << "Uptonum: normexpr returned factor " << termfact->getInfix() 
      << endl << "         times a2: " << a2->getInfix() << endl);
  answer = equaleqs(a1,a2);
  a1->destroy();
  a2->destroy();

  if (coef) throw(string("uptonum expects coef to be null"));

  if(ansfact==NULL && termfact==NULL) // 0 and 0
    {
      DBG(cout << "uptonum returning true, coef=NULL" << endl);
      return(true);  
    }
  else if(termfact==NULL) // x and 0
    {
      ansfact->destroy();
      DBG(cout << "uptonum returning false" << endl);
      return(false);
    }
  else if(ansfact==NULL) // 0 and x
    {
      termfact->destroy();
      coef=new numvalexp(0.);
      DBG(cout << "uptonum returning true, coef=" << coef->getInfix() << endl);
      return(true);
    }
  else if (answer) // nonzero match
    {
      coef = ansfact;
      coef->value = coef->value/termfact->value ;
      termfact->MKS *= -1.;
      coef->MKS += termfact->MKS;
      termfact->destroy();
      DBG(cout << "uptonum returning true, coef=" << coef->getInfix() << endl);
      return(true);
    }
  else   // nonzero, don't match 
    {
    termfact->destroy();
    ansfact->destroy();
    DBG(cout << "uptonum returning false, coef=NULL" << endl);
    return(false);
  }
}
  
