// checksol.cpp
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved
//    version with choice of relerr. Call with RELERR for problem solution
//    and checking, and with default relerr (=0.01?) for student equations.
#include <math.h>
#include "decl.h"
#include "extoper.h"
#include "extstruct.h"
#include "mconst.h"
#include "dbg.h"

// if 1 degree is a numval with value 1, we have FAKEDEG

#define DBG(A) DBGF(CHKSOL,A)
#define DBGM(A) DBGFM(CHKSOL,A)

#ifdef FAKEDEG
#define DEG2RAD DEGTORAD
#else
#define DEG2RAD 1.
#endif

string FPE_handler("FPE:  ");

class answitherr { // evaluation of an expression with error est
public:
  double value;
  double abserr;
};

answitherr* evalexpr(const expr* const ex, const vector<double>* const sols,
		     const double reltverr);


/************************************************************************
 * int checksol(eqn,sols,reltverr)                                      *
 *      plugs the solution values in sols into the equation eqn.        *
 *      compares descrepancy with expection using relative err reltverr *
 * returns                                                              *
 *    0 if seems within error bars                                      *
 *    1 if within 100 * error bars                                      *
 *    2 if not within 100 * error bars                                  *
 *    3 floating point error (really, really not ok)                    *
 * NOTE: currently the error bar calculation has faults                 *
 ************************************************************************/
int checksol(const binopexp* const eqn, const vector<double>* const sols,
	     const double reltverr) {
  DBG(cout << "entered checksol" << endl);
  answitherr* value;
  expr* eqexpr = copyexpr(eqn); // g++ refused this without copyexpr
  try {
    value = evalexpr(eqexpr, sols, reltverr);
  } 
  catch (string &err){
    // don't delete value, it has never been assigned.
    eqexpr->destroy();
    DBG(cout << " ERROR " << err << endl);
    if(FPE_handler==err.substr(0,FPE_handler.length()))  //if beginning matches
      return(3);
    else {
      throw(err);
    }
  }
  eqexpr->destroy();
  DBGM(cout << "Eqn " << eqn->getInfix() <<
       " balenced with discrepancy " << value->value 
       << " and absolute error " << value->abserr << endl);
  if ((fabs(value->value) <= value->abserr)) {
    DBG(cout << " seems OK" << endl);
    delete value;
    return(0);
  } else if ((fabs(value->value) <= 100 * value->abserr)) {
    DBG(cout << " NOT REALLY OK" << endl);
    delete value;
    return(1);
  } else {
    DBG(cout << " seems VERY NOT OK on" << endl; cout << eqn->getInfix() 
	<< endl);
    delete value;
    return(2);
  }
}

/************************************************************************
 * evalexpr  evaluates the expression ex at the solution point, given   *
 *      by the vector of SI values sols. It also estimates the maximum  *
 *      error it should have in calculating the value.                  *
 *  It treats an equation lhs = rhs as if it were                       *
 *      the expression lhs-rhs                                          *
 *  Errors contributions from several sources are added in magnitudes,  *
 *      not in quadrature, as we are interested in a maximum rather     *
 *      than an expected error.                                         *
 * Note: The calculation simply guesses the errors in all numvals and   *
 *      solution points, so this is not a real calculation. To do that, *
 *      we would need to propagate errors in eqnumsimp and other places *
 *      numvals are calculated, and give errors on input given values   *
 *      for which we currently have no facility.                        *
 ************************************************************************/
answitherr* evalexpr(const expr* const ex, const vector<double>* const sols,
		     const double reltverr) {
  answitherr* retval = new answitherr;
  int k;
  
#ifdef WITHDBG // for debugging
  static int call=0;    
  int thiscall=call++;  
#endif
  DBG(cout << "evalexpr call " << thiscall << " for " 
      << ex->getInfix() << " with reltverr = " << reltverr << endl);
  switch (ex->etype) {
  case numval:
    retval->value = ((numvalexp*)ex)->value;
    retval->abserr = reltverr * fabs(retval->value);
    DBG(cout << "evalexpr call " << thiscall << " numval returning " 
	<< retval->value << "+-" << retval->abserr << endl);
    return(retval);
    break;

  case physvart:
    retval->value = (*sols)[((physvarptr *) ex)->varindex];
    retval->abserr = reltverr * fabs(retval->value);
    DBG(cout << "evalexpr call " << thiscall << " physvar returning " 
	<< retval->value << "+-" << retval->abserr << endl;);
    return(retval);

  case function: {
    answitherr* argval = evalexpr(((functexp *)ex)->arg,sols, reltverr);
    switch(((functexp *)ex)->f->opty) {    // remember, if FAKEDEG, trig 
    case sine:                // functions in degrees! if not, trig functions
      retval->value = sin(DEG2RAD * argval->value);      //  of radians
      retval->abserr = max(fabs(cos(DEG2RAD * argval->value) 
				* DEG2RAD * (argval->abserr)), reltverr);
      break;
    case cose:
      retval->value = cos(DEG2RAD * argval->value);
      retval->abserr = max(fabs(sin(DEG2RAD * argval->value) 
				* DEG2RAD * (argval->abserr)), reltverr);
      break;
    case tane:
      retval->value = tan(DEG2RAD * argval->value);
      retval->abserr = fabs((1.0 + pow(retval->value,2)) 
			    * DEG2RAD * (argval->abserr));
      break;
    case expe:
      retval->value = exp(argval->value);
      retval->abserr = fabs(retval->value * argval->abserr);
      break;
    case lne:
      retval->value = log(argval->value);
      retval->abserr = fabs(argval->abserr / retval->value);
      break;
    case log10e:
      if(argval->value<=0){\
	delete retval;
	throw(FPE_handler + string("log of negative"));
      }
      retval->value = log10(argval->value);
      retval->abserr = fabs(argval->abserr / (retval->value * log(10.0)));
      break;
    case sqrte:
      if(argval->value<=0){
	delete retval;
	throw(FPE_handler + string("sqrt of negative"));
      }
      retval->value = sqrt(argval->value);
      if (retval->value > reltverr) {
	retval->abserr = (argval->abserr / (2.0 * retval->value));
      } else {
	retval->abserr = sqrt(argval->abserr);
      }
      break;
    case abse:
      retval->value = fabs(argval->value);
      retval->abserr = argval->abserr;
      break;
    default:
      throw(string("impossible function in evalexpr"));
    }
    DBG(cout << "evalexpr call " << thiscall << " function returning " 
	<< retval->value << "+-" << retval->abserr << endl;);
    delete argval;
    return(retval);
  }
  case binop: {
    answitherr* lhsval = evalexpr(((binopexp*)ex)->lhs,sols,reltverr);
    answitherr* rhsval = evalexpr(((binopexp*)ex)->rhs,sols,reltverr);
    switch(((binopexp*)ex)->op->opty) {
    case divbye:
      if(rhsval->value==0.0){
	delete retval;
	throw(FPE_handler + string("divide by zero"));
      }
      retval->value = lhsval->value/rhsval->value;
      retval->abserr = lhsval->abserr/fabs(rhsval->value) 
	+ rhsval->abserr * fabs(lhsval->value/pow(rhsval->value,2));
      break;
    case topowe:
      retval->value = pow(lhsval->value,rhsval->value);
      if (fabs(lhsval->value)>lhsval->abserr) {
	// AW: joel's suggested corrections for kt5a bug (email 9/8/03)  
	retval->abserr = lhsval->abserr 
	  * fabs(rhsval->value * (retval->value /lhsval->value)) 
	  + rhsval->abserr * fabs(log(fabs(lhsval->value)) * retval->value);
      } else {
	retval->abserr = pow(lhsval->abserr, rhsval->value - rhsval->abserr);
      }
      break;
    case equalse:
      retval->value = lhsval->value - rhsval->value;
      retval->abserr = lhsval->abserr + rhsval->abserr;
      break;
    case grte:
    case gree:
    default:
      throw(string("I can't return eval of >=, >, or unknown expr"));
    }
    delete lhsval;
    delete rhsval;
    DBG(cout << "evalexpr call " << thiscall << " binop returning " 
	<< retval->value << "+-" << retval->abserr << endl;);
    return(retval);
  }
  case n_op: {
    switch(((n_opexp*)ex)->op->opty) {
    case pluse: {
      retval->value = 0;
      retval->abserr = 0;
      for (k=0; k<((n_opexp*)ex)->args->size(); k++) {
	answitherr* argval 
	  = evalexpr((*((n_opexp *)ex)->args)[k], sols,reltverr);
	retval->value += argval->value;
	retval->abserr += argval->abserr;
	delete argval;
      }
      break;
    }
    case multe: {
      retval->value = 1;
      retval->abserr = 0;
      for (k=0; k<((n_opexp*)ex)->args->size(); k++)  {
	answitherr* argval
	  = evalexpr((*((n_opexp*)ex)->args)[k],sols,reltverr);
	retval->abserr = fabs(argval->value*retval->abserr) 
	  + fabs(argval->abserr * retval->value);
	retval->value *= argval->value;
	delete argval;
      }
      break;
    }
    default:
      throw(string("unknown n_op in evalexpr"));
    }
  }
    DBG(cout << "evalexpr call " << thiscall << " n_op returning " 
	<< retval->value << "+-" << retval->abserr << endl);
    return(retval);
  case unknown:
  case fake:
  default:
    throw(string("I can't return eval of fake or unknown expr"));
  }
}
