// parseunit.cpp     (version including units)
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved

#include <string>
#include <math.h>
#include <ctype.h>		// for isdigit in geterr
#include "decl.h"
#include "extoper.h"
#include "extstruct.h"
#include "mconst.h"
#include "unitabr.h"
#include "dbg.h"

#define DBG(A) DBGF(GETEQS,A)

extern unitabrs unittable;
extern vector<string> * constnames;
extern vector<numvalexp *> * constnumvals;
double geterr(const string value); // below

/************************************************************************
 * parseunit  pulls items off stack, starting with first token before   *
 *      the U) and finishing with the ( before the dnum.                *
 *    returns the numvalexp for the physical quantity                   *
 *    or else throws string  (should it return NULL instead?)           *
 ********                                                       *********
 *      This is a finite state machine. They are                        *
 *        A:  expecting a unit. on alpha -> B, on num -> C else error   *
 *        B:  expecting . or / or value                                 *
 *	  B1:  got value from B, need to see if was exponent or value	*
 *        C:  expecting ^, -> D, else error                             *
 *        D:  expecting unit to be raised to power                      *
 *        E:  cleanup, got value, expecting dnum                        *
 *      Transitions are by tokens recieved:   .  /  ^  num  alpha       *
 ************************************************************************/
numvalexp * parseunit(stack<string> *toklist)
{
  DBG(cout << "entering parseunit with " << toklist->size() 
      << " tokens" << endl; );
  int k;
  bool expectu = true;
  enum states { A, B, B1, C, D, E };
  states state = A;
  double expval, abserr;
  numvalexp * curnv = new numvalexp(1.);
  curnv->abserr = 0.;
  curnv->MKS.put(0,0,0,0,0);
  numvalexp * newnv;
  while (!toklist->empty()) {
    string token = toklist->top(); toklist->pop();
    DBG(cout << "parseunit in state " << state 
        << " popped " << toklist->size() << "'th token " << token << endl; );
    if (token.compare("U)")==0) throw(string("end unit inside units"));
    if (isanum(token)) {
      switch (state)
        {
        case A:
          expval = atof(token.c_str()); // got a number when expecting a unit
          state = C;                    // must be the exponent, search for ^
          break;
        case B:
	  {
	    expval = atof(token.c_str()); // could be value or exponent
	    abserr = curnv->value * geterr(token);
	    DBG(cout << "getaneqwu on numval " << token << " assigning error "
		     << abserr << endl; );
	    state = B1;
	    break;
	  }
        default:
          throw(string("parseunit got number when not expecting one"));
        }
      continue;
    }
    if (token.compare(".")==0) {
      if (state == B) {
        state = A;
        continue;
      }
      else throw(string("parseunit got . when not expecting one"));
    }
    if (token.compare("/")==0) {
      if (state == B) {
        state = A;
        curnv->MKS*= -1;
	curnv->value = 1./curnv->value;	// added 7/10 JaS
        continue;
      }
      else throw(string("parseunit got / when not expecting one"));
    }
    if (token.compare("^")==0) {
      if ((state == C) || (state == B1)) {
        state = D;
        continue;
      }
      else throw(string("parseunit got ^ when not expecting one"));
    }
    if (token.compare("dnum")==0) {
      if (state == B1) {
	curnv->value *= expval;
	curnv->abserr = abserr;
	state = E;            // expval was numerical value, so continue
      }
      if (state == E) {
        if (toklist->top().compare("(")!=0) 
          throw(string("in parseunit, expect ( before dnum, not there"));
        toklist->pop();
        DBG(cout << "parseunit to return " << curnv->value <<
            "[" << curnv->MKS.print() << "]" << endl; );
        return(curnv);
      }
      else throw(string("parseunit got dnum when not expecting one"));
    }
    DBG(cout << "only possibilities for " << token << " are const or unit"
        << endl; );
    for (k = 0; k < constnames->size(); k++)
        if ((*constnames)[k]==token) break;
    if (k < constnames->size())
      { 
        DBG(cout << "seems " << token << " is const number " << k << endl; );
        if ((*constnumvals)[k]->MKS.zerop()) // got a number from units table
          {
            switch (state)
              {
              case A:
                expval = (*constnumvals)[k]->value;
                state = C;
                break;
              case B:
                curnv->value *= (*constnumvals)[k]->value;
                state = E;
                break;
              default:
                throw(string("parseunit got number when not expecting one"));
              }
            continue;
          }
        else throw(string("parseunit found dimensioned unit ") +
                   (*constnumvals)[k]->getInfix() +
                   "where it expects a number");
      }
    // Only remaining possibility is string specifying unit or unit-with-prefix
    DBG(cout << "only possibility for " << token << " is unit (w or wo pfx)"
        << endl; );
    newnv = unittable.unitget(token);
    if (newnv == (numvalexp *)NULL) 
      throw(string("parseunit thought ") + token 
            + " should be unit but it isn't");
    DBG(cout << "parseunit unitget returned " << newnv->value <<
        "[" << newnv->MKS.print() << "]" << endl; );
    if (state == D) { 
      newnv->MKS *= expval; 
      newnv->value = pow(newnv->value,expval);
    }
    else if (state != A) throw(string("parseunit got unit when should not"));
    curnv->value *= newnv->value;
    curnv->abserr *= newnv->value;
    curnv->MKS += newnv->MKS;
    newnv->destroy();
    state = B;
  }
  throw(string("parseunit ran out of tokens before finishing"));
}

/************************************************************************
 * geterr(string value)   returns an absolute error to be associated	*
 *	with the value given.						*
 *   If value appears to be an integer (no decimal point) error is 0	*
 *      if it has a decimal point, the error is +- half the last digit	*
 *	specified.							*
 ************************************************************************/
double geterr(const string value)
{
  int j,k;
  k = value.find('.');
  if (k == value.npos) return(0.);
  double retval = 0.5;
  for (j = k+1; j < value.size(); j++) {
    if (isdigit(value[j])) { retval *= .1; continue; }
    if ((value[j] == 'E') || (value[j] == 'e')) break;
    throw(string("geterr couldn't find error value in numval ")
	  + value + ", couldn't interpret character " + itostr(j+1));
  }
  if (j == value.size()) return(retval);	// no exponent in float
  retval *= pow(10,atoi(value.substr(j+1).c_str()));
  return(retval);
}
