// valander   get value and gradient of expression at solution point
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved

//      valander * getvnd(ex,vars,sols) returns the value and gradient
//        of the function ex of variables vars about the point sols.
//      valander::print  gives an ugly printout of a valander.
//      as of 4/20, this version, also a vector of bools as to whether
//       a variable appears at all, to try to get around linear limits
//      as of 7/6, pluses replace <eps * sum || with 0
#include <stdio.h>
#include "../src/decl.h"
#include "../src/dbg.h"
#include "../src/extstruct.h"
#include "valander.h"
#include <math.h>
#include "../src/mconst.h"
using namespace std;

#define DBG(A) DBGF(VALANDER,A)

// if 1 degree is a numval with value 1, we have FAKEDEG
#ifdef FAKEDEG
#define DEG2RAD DEGTORAD
#else
#define DEG2RAD 1.
#endif

string dtostr(double);

/************************************************************************
 * getvnd takes a function (not an equation, but what is expected to be *
 *      the lhs of an equation whose rhs is 0) in the variables in the  *
 *      variable list vars, and expands it to first order around the    *
 *      point sols, returning the value and gradient of the function    *
 *      in a valander ("value and derivative")                          *
 *    This version also contains a hasvar vector<bool> which tells if   *
 *      the function has any dependence on the variable                 *
 *    Note if presented an equation, it interprets it as a function     *
 *      eq->lhs - eq->rhs                                               *
 ************************************************************************/
// argument(s):
//  ex - const expr* -- is expression to be evaluated
//  vars - const vector<physvar*>* -- list of independant equations
//  sols - const vector<double>* sols -- points at which to evaluate
valander * getvnd(const expr * ex, const vector<physvar *> * vars,
                  const vector<double> * sols) {
  DBG( cout << "Entering valander with " << ex->getInfix() << endl;);

  valander* ret = new valander(vars->size());
  int k;
  double tempval;
  
  switch (ex->etype) {
    case numval: {
      ret->value = ((numvalexp *) ex)->value;
      return ret;
    }
  case physvart: {
    k = ((physvarptr *)ex)->varindex;
    if (k<0) {
      throw(string("valander found physvar not on var list"));
    }
    ret->value = (*sols)[k];
    ret->gradient[k] = 1.;
    ret->hasvar[k] = true;
    return(ret);
  }
  case function: {
    valander * argvnd = getvnd(((functexp *)ex)->arg,vars,sols);
    switch(((functexp *) ex)->f->opty) {
    case sine:
      ret->value = sin(DEG2RAD * argvnd->value);
      if (lookslikeint(ret->value,k)) ret->value = (double) k;
      tempval = cos(DEG2RAD * argvnd->value);
      if (lookslikeint(tempval,k)) tempval = (double) k;
      tempval *= DEG2RAD;
      goto functfin;
    case cose:
      ret->value = cos(DEG2RAD * argvnd->value);
      if (lookslikeint(ret->value,k)) ret->value = (double) k;
      tempval = -sin(DEG2RAD * argvnd->value);
      if (lookslikeint(tempval,k)) tempval = (double) k;
      tempval *= DEG2RAD;
      goto functfin;
    case tane:
      ret->value = tan(DEG2RAD  * argvnd->value);
      tempval = DEG2RAD / pow(cos(DEG2RAD * argvnd->value),2);
      goto functfin;
    case expe:
      ret->value = exp(argvnd->value);
      tempval = ret->value;
      goto functfin;
    case lne:
      if (argvnd->value > 0.) {
        ret->value = log(argvnd->value);
        tempval = 1./argvnd->value;
        goto functfin; }
      else throw(string("valander tried to take log of nonpositive"));
    case log10e:
      if (argvnd->value > 0.) {
        ret->value = log10(argvnd->value);
        tempval = 1.0/(argvnd->value*log(10.0));
        goto functfin; }
      else throw(string("valander tried to take log of nonpositive"));
    case sqrte:
      if (argvnd->value > 0.) {
        ret->value = sqrt(argvnd->value);
        tempval = 0.5 / ret->value;
        goto functfin; }
      else throw(string("valander tried to differentiate sqrt(x) at x<=0"));
    case abse:
      if (argvnd->value != 0.) {
        ret->value = fabs(argvnd->value);
        tempval = (argvnd->value > 0) ? 1. : -1.;
        goto functfin; }
      else throw(string("valander tried to differentiate |x| at x=0"));
    default:
      throw(string("unknown function in valander"));
      
    functfin:
      for (k = 0; k < vars->size(); k++) {
        ret->gradient[k] = tempval * argvnd->gradient[k];
        ret->hasvar[k] = argvnd->hasvar[k];
      }
      delete argvnd;
      return(ret);
    } // end of switch on function type
  }   // end of case function
  case binop: {
    valander * lhsvnd = getvnd(((binopexp *)ex)->lhs,vars,sols);
    valander * rhsvnd = getvnd(((binopexp *)ex)->rhs,vars,sols);
    double temp2;               // this will be d op / d rhs
    
    switch(((binopexp *)ex)->op->opty) {
    case divbye:
      if (rhsvnd->value != 0) {
        ret->value = lhsvnd->value/rhsvnd->value;
        tempval = 1./rhsvnd->value;
        temp2 = -lhsvnd->value/pow(rhsvnd->value,2);
        goto binret; }
      else throw(string("valander tried to divide by 0"));
    case topowe:
      if (lhsvnd->value > 0) {
        ret->value = pow(lhsvnd->value,rhsvnd->value);
        tempval = rhsvnd->value * pow(lhsvnd->value,rhsvnd->value - 1.);
        temp2 = log(lhsvnd->value) * ret->value;
        goto binret; }
      if ((((binopexp *)ex)->rhs->etype == numval) && 
          (lookslikeint(rhsvnd->value,k))) {
        ret->value = pow(fabs(lhsvnd->value),k);
        if ((k%2) == 1) ret->value = -ret->value;
        if (lhsvnd->value == 0){
          if (k <0) throw(string("0 raised to negative power"));
          if (k == 0) throw(string("0 raised to 0 undefined"));
          if (k == 1) { tempval = 1.;temp2=0.; goto binret; }
          if (k > 1)  { tempval = 0.;temp2=0.; goto binret; }
        }
        tempval = rhsvnd->value * ret->value/lhsvnd->value;
        temp2 = 0.;     // irrelevant, as grad is 0
        goto binret;
      }
      else throw(string(
                  "valander tried to raise nonpositive to noninteger power"));
    case equalse:
      ret->value = lhsvnd->value - rhsvnd->value;
      if (fabs(ret->value) < RELERR * 
	  (fabs(lhsvnd->value) + fabs(rhsvnd->value)))
	ret->value = 0.;
      tempval = 1.; temp2 = -1.; goto binret;
    case grte:
    case gree:
      throw(string("valander not prepared for  >, >="));
    default:
      throw(string("valander got unknown binop"));
    binret:
      for (k = 0; k < vars->size(); k++) {
        ret->gradient[k] = tempval * lhsvnd->gradient[k] 
          + temp2 * rhsvnd->gradient[k];
	if (fabs(ret->gradient[k]) < RELERR * 
	    (fabs(tempval * lhsvnd->gradient[k]) 
	     + fabs(temp2 * rhsvnd->gradient[k])))
	  ret->gradient[k] = 0.;
        ret->hasvar[k] = lhsvnd->hasvar[k] || rhsvnd->hasvar[k];
      }
      delete lhsvnd;
      delete rhsvnd;
      DBG(cout << "return from valander on binop, " 
	       << ret->print() << endl; );
      return(ret);
    }
  } // end of case binop
  case n_op:
    {
      int q;
      n_opexp * exnop = (n_opexp *) ex;
      valander * argvnd;
      switch(exnop->op->opty)
        {
        case pluse:
	  {
	    ret->value = 0;
	    double absval = 0;
	    vector<double> absgrad(vars->size(),0.);
	    for (q = 0; q < exnop->args->size(); q++)
	      {
		argvnd = getvnd((*exnop->args)[q],vars,sols);
		ret->value += argvnd->value;
		absval += fabs(argvnd->value);
		for (k = 0; k < vars->size(); k++) {
		  ret->gradient[k] += argvnd->gradient[k];
		  absgrad[k] += fabs(argvnd->gradient[k]);
		  ret->hasvar[k] = ret->hasvar[k] || argvnd->hasvar[k];
		}
		if (fabs(ret->value) < RELERR * absval) ret->value = 0.;
		for (k = 0; k < vars->size(); k++) 
		  if (fabs(ret->gradient[k]) < RELERR * absgrad[k])
		    ret->gradient[k] = 0;
		delete argvnd;
	      }
	    DBG(cout << "return from valander on plus, " 
		     << ret->print() << endl; );
	    return (ret);
	  }
        case multe:
          ret->value = 1;
          for (q = 0; q < exnop->args->size(); q++) 
            {
              argvnd = getvnd((*exnop->args)[q],vars,sols);
	      vector<double> absgrad(vars->size(),0.);
              for (k = 0; k < vars->size(); k++)
                {
                  ret->gradient[k] *= argvnd->value;
		  absgrad[k] *= fabs(argvnd->value);
                  ret->gradient[k] += ret->value * argvnd->gradient[k];
                  absgrad[k] += fabs(ret->value * argvnd->gradient[k]);
                  ret->hasvar[k] = ret->hasvar[k] || argvnd->hasvar[k];
                }
              ret->value *= argvnd->value;
              for (k = 0; k < vars->size(); k++)
		if (fabs(ret->gradient[k]) < RELERR * absgrad[k])
		  ret->gradient[k] = 0;
              delete argvnd;
            }
	  DBG(cout << "return from valander on mult, " 
		     << ret->print() << endl; );
          return(ret);
        default:
          throw(string("unknown n_op in valander"));
        } //  end of switch on n_op type
    } // end of case n_op
  case unknown:
  case fake:
  default:
    throw(string("unknown expr in valander"));
  } // end of switch on expr type
}

string valander::print() {  
  string ret = string("Value =") + dtostr(value) + ", gradient = ";
  for (int k=0; k<gradient.size(); k++) {
    ret += dtostr(gradient[k]);
    if ((gradient[k]==0) && hasvar[k]) {
      ret += "*";
    }
    if (k+1 != gradient.size()) {
      ret += ", ";
    }
  }
  return(ret);
}
