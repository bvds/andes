// solvetrig.cpp
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved
//
//	findtrigvars		called from maketrigvars and recursively
//	maketrigvars  		called from checkeqs only
//	solvetrigvar  		called from checkeqs only
//	trigsearch		called from solvetrigvar and recursively
//	signisknown		called from solvetrigvar

#include <math.h>
#include "decl.h"
#include "extoper.h"
#include "dbg.h"
#include "mconst.h"
using namespace std;

#define DBG(A) DBGF(SLVTRIG,A)
#define DTL(A) DBGFM(SLVTRIG,A)

void trigsimp(expr * & ex);

void findtrigvars( expr * & ex, vector<expr *> * &trigvars);
void maketrigvars(vector<binopexp *> * eqexpr,
		  vector<expr *> * &trigvars);
bool solvetrigvar(const expr * const var, vector<binopexp *> * & eqn);
bool trigsearch(const expr * const arg, expr *& coef,
		const expr * const ex, bool & iscos, expr * & oside);

/************************************************************************
 *	finds equations involving trig functions, makes list of
 *	the arguments of those functions, and for each a list of
 *	equations which use trig functions of those arguments. 
 *	Then for each argument, looks for a pair of equations
 *	of the form 
 * 		c1 fact1 + k1 fact sin(arg) = 0 (c1,c2,k1,k2 are numbers)
 *	and  	c2 fact1 + k2 fact cos(arg) = 0
 *	to get a numerical value for arg
 ************************************************************************/
//  In fact, in this code, solvetrigvar requires the signs of fact1 and
//  fact to be determined (except it uses isnonneg instead of ispositive)
//   We should change this, but it produces a mod Pi uncertainty in 
//  the angle which is either wrong or requires adding an new variable
//  taking on only integer values (or even just 0 or 1).

//  This is not powerful enough to solve exlmom3a. We should implement
//  a more powerful algorith. if fact sin(arg) = v1 and fact cos(arg)=v2,
//  we can conclude that fact^2 = v1^2 + v2^2. While this does not 
//  return a more tractible equation for arg, it might help determine
//  fact, v1 and v2.

/************************************************************************
 * void findtrigvars( expr * & ex, vector<expr *> * &trigvars)		*
 *	searches expression ex for sin, cos, or tan functions of arg.	*
 *	If found, finds or adds arg to trigvars				*
 ************************************************************************/
void findtrigvars( expr * & ex, vector<expr *> * &trigvars)
{
  int k;
  EQCHK(ex);
#define DEBUG_FINDETRIGVARS 0
#if DEBUG_FINDTRIGVARS
  DBG( cout << "findTRIG: starting looking at " << ex->getInfix() << endl);
#endif
  switch (ex->etype)
    {
    case numval:
    case physvart:
      return;
    case function:
      { 
	functexp * exfun = (functexp *)ex;
	if ((exfun->f->opty == sine)  ||
	    (exfun->f->opty == cose)  ||
	    (exfun->f->opty == tane))
	  {
#if DEBUG_FINDTRIGVARS
	    DBG( cout << "findTRIG: found a trig: " 
		      << exfun->getInfix() << endl; );
#endif
	    trigsimp(ex);
	    if (ex->etype == function) exfun = (functexp *)ex;
	    else 
	      {
		if ((ex->etype != n_op) ||
		     ((*((n_opexp *)ex)->args)[1]->etype != function))
		  throw(string(
			"lost trig function in findtrivars, impossible"));
		exfun = (functexp *)(*((n_opexp *)ex)->args)[1];
	      }
	    for (k = 0; k < trigvars->size(); k++)
	      {
		if (equaleqs((*trigvars)[k],exfun->arg)) break;
#if DEBUG_FINDTRIGVARS
		DBG( cout << "findTRIG: no dup with var " 
			  << (*trigvars)[k]->getInfix() << endl; );
#endif
	      }
#if DEBUG_FINDTRIGVARS
	    DBG( if (k < trigvars->size())
		 cout << "findTRIG: matches with trigvar " << k << endl; );
#endif
	    if (k == trigvars->size()) {
#if DEBUG_FINDTRIGVARS
	      DBG( cout << "findTRIG: its a new trig: " << endl; );
#endif
	      trigvars->push_back(copyexpr(exfun->arg));
	    }
	  } // end of if trig function
	findtrigvars(exfun->arg,trigvars);
      }
      return;
    case binop:
      findtrigvars(((binopexp *)ex)->lhs,trigvars);
      findtrigvars(((binopexp *)ex)->rhs,trigvars);
      return;
    case n_op:
      for (k = 0; k < ((n_opexp *)ex)->args->size(); k++)
	findtrigvars((*((n_opexp *)ex)->args)[k],trigvars);
      return;
    case unknown:
    case fake:
    default:
      throw(string("findtrigvars called with unknown expr"));
    }
}


/************************************************************************
 * bool signisknown(expr * const ex)                    		*
 *	returns true if either ex is known to be nonneg or known to be	*
 *	nonpositive.                                                    *
 ************************************************************************/
bool signisknown(const expr * const ex)
{
  if (isnonneg(ex)) return(true);
  expr * excopy = copyexpr(ex);
  kmult(excopy, -1.);
  eqnumsimp(excopy,true);
  flatten(excopy);
  bool answer = isnonneg(excopy);
  excopy->destroy();
  return(answer);
}

/************************************************************************
 * bool nonzeroisknown(expr * const ex)                    		*
 *	returns true if either ex is known to be positive or            *
 *      -ex is known to be positive.                             	*
 ************************************************************************/
bool nonzeroisknown(const expr * const ex)
{
  if (ispositive(ex)) return(true);
  expr * excopy = copyexpr(ex);
  kmult(excopy, -1.);
  eqnumsimp(excopy,true);
  flatten(excopy);
  bool answer = ispositive(excopy);
  excopy->destroy();
  return(answer);
}

/************************************************************************
 * void maketrigvars( vector<binopexp *> * eqexpr,			*
 *		vector<expr *> * &trigvars)		 		*
 *	adds to  trigvars (at least empty list required on input	*
 ************************************************************************/
void maketrigvars( vector<binopexp *> * eqexpr,
		  vector<expr *> * & trigvars)
{
  DBG( cout << "starting maketrigvars with "
	    << eqexpr->size() << " equations." << endl; );
  VEQCHK(eqexpr);
  for (int k=0; k < eqexpr->size(); k++)
  {
    expr * mtexp = (*eqexpr)[k];
    findtrigvars(mtexp,trigvars);
    if (mtexp->etype != binop) 
      throw(string("In maketrigvars findtrigvars returned non-binop"));
    (*eqexpr)[k] = (binopexp *) mtexp;
  }
}

/************************************************************************
 *  bool solvetrigvar(const expr * const arg, 				*
 *		vector<binopexp *> * & eqn)				*
 * 	looks for two equations of the form  				*
 *		fact1 + fact2 sin(arg) = 0			        *
 *	and     fact3 + fact4 cos(arg) = 0			        *
 * 	with fact2, fact4, and fact1*fact3 all nonzero.	                *
 *      Also demand that:  fact3 and fact1 are different only by a      *
 *      known numerical factor.  Likewise with and fact2 and fact4.     *
 *  if found, replaces one equation with arg = numval and other with    *
 *  an equation without arg.                                      	*
 ************************************************************************/
bool solvetrigvar(const expr * const arg, vector<binopexp *> * & eqn)
{
  unsigned int j, k;
  numvalexp *c2, *k2;
  double tempvar;
  expr *fact1, *fact2, *fact3, *fact4;
  expr *fa, *fb;
  bool firstiscos, secondiscos;
  DBG( cout << "Entering solvetrigvar, angle arg=" 
       << arg->getInfix() << endl);
  VEQCHK(eqn);
  for (j = 0; j+1 < eqn->size(); j++) // loop to find first equation of pair
    if (trigsearch(arg, fact2,(expr *)(*eqn)[j], firstiscos, fact1))
      {
	DBG( cout << "First equation " << j << ":  " << (*eqn)[j]->getInfix() 
	     << endl << "          Trigsearch returned fact2="
	     << fact2->getInfix() << " and fact1="
	     << fact1->getInfix() << " and "
	     << (firstiscos ? "cosine" : "sine") << endl);
	if (!nonzeroisknown(fact2)) continue;
	// first equation:  fact2*cos(arg) + fact1 = 0 (if firstiscos)
	for (k = j+1; k < eqn->size(); k++) // loop to find second equation
	  if (trigsearch(arg, fact4, (expr *)(*eqn)[k], secondiscos, fact3))
	    {
	      DBG( cout << "Second Eqn. " << k << ":  " << (*eqn)[k]->getInfix() 
		   << endl << "          Trigsearch returned coef "
		   << fact4->getInfix() << " and constant term "
		   << fact3->getInfix() << " and "
		   << (secondiscos ? "cosine" : "sine") << endl);
	      if(secondiscos == firstiscos || !nonzeroisknown(fact4)) 
		continue;
	      // 2nd Eqn:  fact3 + fact4*sin(arg) = 0 (if firstiscos)
	      if(uptonum(fact3,fact1,c2) && uptonum(fact4,fact2,k2)){
		// 1st Eqn:  fact1 + fact2*cos(arg) = 0 (if firstiscos)
		// 2nd Eqn:   c2*fact1 + k2*fact2*sin(arg) = 0 (if firstiscos)
		fa=fact1; fb=fact4;
		fact2->destroy(); fact3->destroy();  // won't use these
	      }
	      // if fact1 is zero, then we need the second form:
	      else if(uptonum(fact1,fact3,k2) && uptonum(fact2,fact4,c2)) {
		// 1st Eqn:   k2*fact3 + c2*fact4*cos(arg) = 0 (if firstiscos)
		// 2nd Eqn:  fact3 + fact4*sin(arg) = 0 (if firstiscos)
		fa=fact3; fb=fact2;
		fact1->destroy(); fact4->destroy();  // won't use these
	      } else continue;
	      DBG( cout << "k2 is " << k2->getInfix() << 
		   ", and c2 is " << c2->getInfix() << endl);
	      // check if units definitely differ: 
	      if (!(c2->MKS == k2->MKS || c2->MKS.unknp() || k2->MKS.unknp())) 
		throw(string("tan(angle) can't have dimensions"));
	      // calculate arctangent or arccotangent
	      tempvar = (firstiscos ? atan2(c2->value,k2->value) :
		      atan2(k2->value,c2->value));  // interval [-pi,pi]
	      if(isnonneg(fa) == isnonneg(fb)) 
		tempvar += ((tempvar>0)?-M_PI:M_PI); // interval [-pi,pi]
	      if(tempvar < 0 ) tempvar += 2*M_PI;  // interval [0,2*pi)

	      // Make equation for the angle.
	      // trigsearch will crash if eq doesn't have zero rhs, so
	      n_opexp *eqlhs = new n_opexp(&myplus);
	      eqlhs->addarg(copyexpr(arg));
	      numvalexp *tempnv = new numvalexp(- tempvar);
	      tempnv->MKS.put(0,0,0,0,0);
	      eqlhs->addarg(tempnv);
	      (*eqn)[j]->destroy();
	      (*eqn)[j] = new binopexp(&equals,eqlhs,new numvalexp(0));
	      DBG(cout << "Output new Eqn. " <<j << ": " 
		  << (*eqn)[j]->getInfix() << endl);

	      // make equation without an angle
	      tempvar = sqrt(pow(c2->value,2)+pow(k2->value,2));
	      if (isnonneg(fa) == isnonneg(fb)) tempvar *= -1.;
	      // about to make (*eqn)[k] be  tempvar * fa + fb = 0
	      n_opexp * tempnop = new n_opexp(&mult);
	      tempnv = new numvalexp(tempvar);
	      tempnv->MKS.put(0,0,0,0,0);
	      tempnop->addarg(tempnv);
	      tempnop->addarg(fa);
	      eqlhs = new n_opexp(&myplus);
	      eqlhs->addarg(tempnop);
	      eqlhs->addarg(fb);
	      (*eqn)[k]->destroy();
	      (*eqn)[k] = new binopexp(&equals,eqlhs,new numvalexp(0));
	      DBG(  cout << "Output new Eqn. " << k << ":  " 
		    << (*eqn)[k]->getInfix() << endl);
	      return(true);
	    } // end of checking and implementing matching equation
      }	// end of loop searching for first of pair
  return(false);
}

/************************************************************************
 * bool trigsearch(arg, coef, ex, iscos, oside) 			*
 *	when called on an equation, which must have rhs = 0,   		*
 * 	tries to write ex as oside + coef * cos(arg) = 0,		*
 *	[or oside + coef * sin(arg) = 0] and returns coef, oside, 	*
 *	iscos and true if successful, with iscos = true if cos and 	*
 *	false if sin. 							*
 *	iscos is meaningless if coef is numval 0			*
 *	returns false if any other function of arg found		*
 *   but it calls itself with expr inside the equation too:		*
 *	If the expression is of the form  				*
 *		coef * (cos | sin) (arg) + oside 			*
 *	(coef can be zero), it returns coef, oside, and true, and iscos *
 *	true if cosine, false if sine (otherwise unset?)		*
 *   Note: if it returns false, coef and oside are untouched. If it 	*
 *	returns true, they are valid expr *, though I haven't checked	*
 *	if they might be n_ops of <2 args 				*
 ************************************************************************/
bool trigsearch(const expr * const arg, expr * & coef,
		const expr * const ex, bool & iscos, expr * & oside)
{
  n_opexp * cf = (n_opexp *) NULL;
  n_opexp * ov = (n_opexp *) NULL;
  expr * cfe = (expr *) NULL;
  expr * ove = (expr *) NULL;
  int k;

  // if it returns true, are not coef and oside always nonzero? if so,
  // don't need all the checking below
#define DEBUG_TRIGSEARCH 1
#if DEBUG_TRIGSEARCH
#ifdef WITHDBG // for debugging
  static int trigsearchcall=0;    
  int thiscall=trigsearchcall++;  
#endif
  DBG( cout << "trigsearch call " << thiscall << " with arg "
	    << arg->getInfix() << ", in expr:  " << ex->getInfix() 
       << ", coef=" << coef << endl);
#endif
  EQCHK(ex);

  switch(ex->etype)
    {
    case numval:
    case physvart:
      oside = copyexpr(ex);
      coef = new numvalexp(0);
#if DEBUG_TRIGSEARCH
      DBG( cout << "trigsearch call " << thiscall 
	   << " returns true on physvar or numval " << endl);
#endif
      return(true);
    case function:
      {
	functexp * fptr = ( functexp * ) ex;
	if (equaleqs(fptr->arg,arg)) 
	  {
	    switch(fptr->f->opty)
	      {
	      case sine:
		iscos = false;
		coef = new numvalexp(1);
		coef->MKS.put(0,0,0,0,0);
		oside = new numvalexp(0);
#if DEBUG_TRIGSEARCH
		DBG( cout << "trigsearch call " << thiscall 
		     << ":  its a sine , coef =" << coef->getInfix() 
		     << " and oside =" << oside->getInfix() << endl);
#endif
		return(true);
	      case cose:
		iscos = true;
		coef = new numvalexp(1);
		coef->MKS.put(0,0,0,0,0);
		oside = new numvalexp(0);
#if DEBUG_TRIGSEARCH
		DBG( cout << "trigsearch call " << thiscall 
		     << ":  its a cosine , coef =" << coef->getInfix() 
		     << " and oside =" << oside->getInfix() << endl);
#endif
		return(true);
	      case tane:
	      case abse:
	      default:
#if DEBUG_TRIGSEARCH
		DBG(cout << "trigsearch call " << thiscall << " returns false" 
		    << endl);
#endif
		return(false);
	      }
	  }
	else
	  {
	    if (!trigsearch(arg,cfe,fptr->arg,iscos,ove)){
#if DEBUG_TRIGSEARCH
	      DBG(cout << "trigsearch call " << thiscall << " returns false" 
		  << endl);
#endif
	      return(false);
	    }
	    if 		
	      ((cfe->etype != numval) || ((numvalexp *)cfe)->value != 0)
	      {
		cfe->destroy(); ove->destroy();
#if DEBUG_TRIGSEARCH
		DBG(cout << "trigsearch call " << thiscall << " returns false" 
		    << endl);
#endif
		return(false);
	      }
	    else		
	      {
		cfe->destroy(); ove->destroy();
		coef = new numvalexp(0);
		oside = copyexpr(ex);
#if DEBUG_TRIGSEARCH
		DBG( cout << "trigsearch call " << thiscall 
		     << ":  funct arg not right one. coef =" 
		     << coef->getInfix() << " and oside =" << oside->getInfix()
		     << endl);
#endif
		return(true);
	      }
	  }
      }
    case binop:
      {
#if DEBUG_TRIGSEARCH
	DBG(cout << "trigsearch call " << thiscall << " selects binop " 
	    << endl);
#endif
	binopexp * bptr = (binopexp *) ex;
	if (bptr->op->opty == equalse)
	  {
	    if ((bptr->rhs->etype != numval) || 
		(((numvalexp *)bptr->rhs)->value != 0))
	      throw(string(
		   "trigsearch doesn't expect equations with nonzero rhs"));
	    if (!trigsearch(arg,cfe,bptr->lhs,iscos,ove)){
#if DEBUG_TRIGSEARCH
	      DBG(cout << "trigsearch call " << thiscall << " returns false" 
		  << endl);
#endif
	      return(false);
	    }
	    if ((cfe->etype != numval) || ((numvalexp *)cfe)->value != 0)
	      {	
		coef = cfe; oside = ove; 
#if DEBUG_TRIGSEARCH
	      DBG(cout << "trigsearch call " << thiscall << " returns true" 
		  << endl);
#endif
		return(true); 
	      }
	    else { 
	      cfe->destroy(); ove->destroy(); 
#if DEBUG_TRIGSEARCH
	      DBG(cout << "trigsearch call " << thiscall << " returns false" 
		  << endl);
#endif
	      return(false); 
	    }
	  } // below here we have a non-logical kind of binop
	if ((bptr->op->opty == grte) ||(bptr->op->opty == gree)){
#if DEBUG_TRIGSEARCH
	  DBG(cout << "trigsearch call " << thiscall << " returns false" 
	      << endl);
#endif
	  return(false);
	}
	//	 below here for non-boolean binop
	if (!trigsearch(arg,cfe,bptr->lhs,iscos,ove)){
#if DEBUG_TRIGSEARCH
	  DBG(cout << "trigsearch call " << thiscall << " returns false" 
	      << endl);
#endif
	  return(false);
	}
	if((cfe->etype != numval) || ((numvalexp *)cfe)->value != 0)
	  {
	    cfe->destroy(); ove->destroy();
#if DEBUG_TRIGSEARCH
	    DBG(cout << "trigsearch call " << thiscall 
		<< " returns false" << endl);
#endif
	    return(false);
	  }
	cfe->destroy(); ove->destroy();
	if (!trigsearch(arg,cfe,bptr->rhs,iscos,ove)){
#if DEBUG_TRIGSEARCH
	  DBG(cout << "trigsearch call " << thiscall << " returns false" 
	      << endl);
#endif
	  return(false);
	}
	if ((cfe->etype != numval) || ((numvalexp *)cfe)->value != 0)
	  {
	    cfe->destroy(); ove->destroy();
#if DEBUG_TRIGSEARCH
	    DBG(cout << "trigsearch call " << thiscall << " returns false" 
		<< endl);
#endif
	    return(false);
	  }
	cfe->destroy(); ove->destroy();
	oside = copyexpr(ex);	
	coef = new numvalexp(0);
#if DEBUG_TRIGSEARCH
	DBG( cout << "trigsearch call " << thiscall 
	     << " binop no arg right one. coef =" << coef->getInfix() 
	     << " and oside =" << oside->getInfix() << endl);
#endif
	return(true);
      }
    case n_op:
      if (((n_opexp *)ex)->op->opty == multe)
	{
#if DEBUG_TRIGSEARCH
	  DBG( cout << "trigsearch call " << thiscall << " selects mult" 
	       << endl);
#endif
	  cf = new n_opexp(&mult); // keep empty until cos/sin found
	  ov = new n_opexp(&mult);
	  bool iscosthis;
	  for (k = 0; k < ((n_opexp *)ex)->args->size(); k++)
	    {
	      if (!trigsearch(arg,cfe,(*((n_opexp *)ex)->args)[k],
			      iscosthis,ove)) goto abort;
	      if ((cfe->etype != numval) || ((numvalexp *)cfe)->value != 0)
		{  // cfe != 0
		  if (cf->args->size() > 0) {
		    cfe->destroy(); ove->destroy();
		    goto abort;		// found product of two expressions
		  }
		  if(ov->args->size() > 0) cf->addarg(copyexpr(ov));
		  cf->addarg(cfe);
		  iscos = iscosthis;
		}
	      else 
		{  // cfe = 0
		  if (cf->args->size() > 0) cf->addarg(copyexpr(ove));
		  cfe->destroy();
		} 
	      ov->addarg(ove); // always multiply constant
	    }
	  // the code inside the if was executed unconditionally before
	  // 1/24/01, and the else not there. I think thats wrong
	  if (cf->args->size() > 0)
	    {
	      coef = cf;
	      flatten(coef);
	      eqnumsimp(coef,true);
	    }
	  else 
	    {
	      coef = new numvalexp(0);
	      cf->destroy();
	    }
	  oside = ov;
	  flatten(oside);
	  eqnumsimp(oside,true);
	  return(true);
	}
      else			// its a plus n_op
	{
#if DEBUG_TRIGSEARCH
	  DBG( cout << "trigsearch call " << thiscall << " selects plus"
	       << endl);
#endif
	  cf = new n_opexp(&myplus);
	  ov = new n_opexp(&myplus);
	  bool iscosthis;
	  for (k = 0; k < ((n_opexp *)ex)->args->size(); k++)
	    {
	      if (!trigsearch(arg,cfe,(*((n_opexp *)ex)->args)[k],
			      iscosthis,ove)) goto abort;
	      if ((cfe->etype != numval) || ((numvalexp *)cfe)->value != 0)
		{  // cfe != 0
		  if ((cf->args->size() > 0) && (iscos != iscosthis))
		    {
		      cfe->destroy(); ove->destroy();
		      goto abort;	 // found one sine and one cosine
		    }
		  cf->addarg(cfe);
		  iscos = iscosthis;
		} 
	      else  // cfe=0 
		cfe->destroy();
	      ov->addarg(ove); // always add constant
	    }
	  coef = cf;
	  flatten(coef);
	  eqnumsimp(coef,true);
	  oside = ov;
	  flatten(oside);
	  eqnumsimp(oside,true);
#if DEBUG_TRIGSEARCH
	    DBG(cout << "trigsearch call " << thiscall << " returns true" 
		<< endl);
#endif
	  return(true);
	}
    case unknown:
    case fake:
    default:
      throw(string("unknown expr type passed to trigsearch"));
    }
 abort:
  {
    cf->destroy();
    ov->destroy();
#if DEBUG_TRIGSEARCH
    DBG(cout << "trigsearch call " << thiscall << " returns false" 
	<< endl);
#endif
    return(false);
  }
}
