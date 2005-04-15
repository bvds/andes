// getaneq.cpp     (version including units)
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved

/************************************************************************
 *  bool getCanonEqn(string bufst)                                      *
 *  bool getStudEqn(slot,bufst)                                         *
 *  binopexp * getAnEqn(bufst,bool tight)                               *
 *      bufst is a String beginning with (=. The whole string is        *
 *      interpreted as a lisp-style equation in known variables.        *
 *  If successful, the binopexp * form of that equation is either       *
 *      returned (getAnEqn), or pushed onto canoneqf (getCanonEqn),     *
 *      or placed in Student Slot slot (getStudEqn).                    *
 *      getCanonEqn and getStudEqn return true if successful            *
 *      all throw exceptions if problem occurs                          *
 *  this version allows (dnum number unitexp)                           *
 *  dimensionless numbers are declared dimensionless if                 *
 *      tight is set and the number is not 0, or                        *
 *        if tight not set but number is in [-2,2], int or int/2,       *
 *               and not 0.                                             *
 *        getStudEqn uses tight, getCanon currently doesn't             *
 *  currently throws a diagnostic if it cannot parse the equation, or   *
 *      if an undeclared variable is encountered. Should make more      *
 *      meaningful.                                                     *
 *  Uses parseeqwunits to tokenize the string and then converts the     *
 *      stack of tokens into an expr.                                   *
 ************************************************************************/

#include <string>
#include <math.h>
#include <stdio.h>
#include <ctype.h>		// for isdigit in geterr
#include "decl.h"
#include "extoper.h"
#include "extstruct.h"
#include "mconst.h"
#include "unitabr.h"
#include "dbg.h"

#define DBG(A) DBGF(GETEQS,A)

extern vector<string> * constnames;
extern vector<numvalexp *> * constnumvals;

stack<string> *parseEqWUnits(const string & lispeq);    // in parseeqwunits
numvalexp * parseunit(stack<string> *toklist);          // in parseunit
double geterr(string value);					// below

binopexp* getAnEqn(const string bufst, bool tight) {
  int k;
  DBG(cout << "getAnEqn [tight = " << ((tight) ? "t" : "f" )
      << "] on " << bufst << endl;);
  stack<string>* toklist = parseEqWUnits(bufst);
  DBG(cout << "parseEqWUnits returned " << toklist->size() << " tokens" 
           << endl;);
  stack<expr*> exprstack;
  while (! toklist->empty()) { 
    string token = toklist->top();
    toklist->pop();
    DBG(cout << "getAnEqn popped " << token << endl;);
    if (token.compare("U)") == 0) { 
      exprstack.push(parseunit(toklist));
      DBG(cout << "parseunit returned " << exprstack.top()->getInfix() 
               << endl;);
      DBG( { expr * tempexp = exprstack.top(); 
             if (tempexp->etype != numval) cout << "NOT A NUMVAL" << endl; 
             else { 
               numvalexp * tempnv = (numvalexp *) tempexp; 
               cout << "numval with value " << tempnv->value << " and units " 
                    << tempnv->MKS.print() << endl;}
            } );
      continue;
    }
    if (isanum(token)) {
      int q;
      // constructor sets dimensions of any number to be "unknown"
      // this is used to guess whether a student has dropped units 
      // for a number in an expression
      numvalexp *nvtemp = new numvalexp(token);
      if (tight) nvtemp->MKS.put(0,0,0,0,0);  // Any number is dimensionless...
#if 1 // For certain numbers, we assume they are known to be dimensionless
      else if (lookslikeint(2*nvtemp->value,q)) {
	if ((q != 0) && (tight || (abs(q) < 5)))
	  nvtemp->MKS.put(0,0,0,0,0);
      }
#endif
      nvtemp->abserr = geterr(token);
      DBG(cout << "getaneqwu on numval " << token << " assigning error "
	  << nvtemp->abserr << " and units " << nvtemp->MKS.print() << endl; );
      exprstack.push(nvtemp);
      continue;                 // don't include numbers in varlist
    }
    if (token.compare("(")==0) {
      if (exprstack.empty())
        throw(string("getAnEqn found ( with empty stack"));
      expr *save = exprstack.top();
      exprstack.pop();
      if (exprstack.empty()) {
        cerr << "Getaneq: ( expr nothing with expr: " << endl;
        cout << "Getaneq: ( expr nothing with expr: " << endl;
        save->pretty(8);
        throw(string("getAnEqn found ( with only one item on stack"));
      }
      if (exprstack.top()->etype == fake) {
        exprstack.pop();
        exprstack.push(save);
        continue;
      } else {
        cerr << "getAnEqn found ( when top of stack not expr )" << endl;
        cerr << "Here is the exprstack:" << endl;
        cerr << save->getInfix() << endl;
        while (!exprstack.empty()) {
          cerr << "exprstack: " << exprstack.top()->getInfix() << endl;
          exprstack.pop();
        }
        throw(string("getAnEqn found ( when top of stack not expr )"));
      }
    }
    if (token.compare(")")==0) {
      exprstack.push(new fakeexpr); // fake marker on stack
      continue;
    }
    if (token.compare("=")==0) {
      expr *lhs = exprstack.top();
      exprstack.pop();
      expr *rhs = exprstack.top();
      exprstack.pop();
      exprstack.push( new binopexp(&equals,lhs,rhs));
      continue;
    }
    if (token.compare("+")==0) {
      n_opexp *thisguy = new n_opexp(&myplus);
      while ((!exprstack.empty()) && (exprstack.top()->etype != fake)) {
        thisguy->addarg(exprstack.top());
        exprstack.pop();
      }
      exprstack.push(thisguy);
      continue;
    }
    if (token.compare("-")==0) {
      expr *lhs = exprstack.top();
      exprstack.pop();
      expr *rhs = exprstack.top();
      if (rhs->etype == fake) {
        n_opexp *temp = new n_opexp(&mult);
        numvalexp *nvtemp = new numvalexp(-1);
        nvtemp->MKS.put(0,0,0,0,0);
        temp->addarg(nvtemp);
        temp->addarg(lhs);
        exprstack.push(temp);
      } else {
        exprstack.pop();
        n_opexp *temp = new n_opexp(&mult);
        numvalexp *nvtemp = new numvalexp(-1);
        nvtemp->MKS.put(0,0,0,0,0);
        temp->addarg(nvtemp);
        temp->addarg(rhs);
        n_opexp *temp2 = new n_opexp(&myplus);
        temp2->addarg(lhs);
        temp2->addarg(temp);
        exprstack.push(temp2);
      }
      continue;
    }
    if (token.compare("*")==0) {
      n_opexp *thisguy = new n_opexp(&mult);
      while ((!exprstack.empty()) && (exprstack.top()->etype != fake)) {
        thisguy->addarg(exprstack.top());
        exprstack.pop();
      }
      exprstack.push(thisguy);
      continue;
    }
    if (token.compare("/") == 0) {
      expr *lhs = exprstack.top();
      exprstack.pop();
      expr *rhs = exprstack.top();
      exprstack.pop();
      exprstack.push( new binopexp(&divby,lhs,rhs));
      continue;
    }
    if (token.compare("^") == 0) {
      expr *lhs = exprstack.top();
      exprstack.pop();
      expr *rhs = exprstack.top();
      exprstack.pop();
      exprstack.push( new binopexp(&topow,lhs,rhs));
      continue;
    }
    if ((token.compare("sin")==0) || 
        (token.compare("Sin")==0) || 
        (token.compare("SIN")==0) ) {
      functexp *thisguy = new functexp(&sinef,exprstack.top());
      exprstack.pop();
      exprstack.push(thisguy);
      continue;
    }
    if ((token.compare("cos")==0) || 
        (token.compare("Cos")==0) || 
        (token.compare("COS")==0) ) {
      functexp *thisguy = new functexp(&cosef,exprstack.top());
      exprstack.pop();
      exprstack.push(thisguy);
      continue;
    }
    if ((token.compare("tan")==0) || 
        (token.compare("Tan")==0) || 
        (token.compare("TAN")==0) ) {
      functexp *thisguy = new functexp(&tanff,exprstack.top());
      exprstack.pop();
      exprstack.push(thisguy);
      continue;
    }
    if ((token.compare("log10")==0) || 
        (token.compare("Log10")==0) || 
        (token.compare("LOG10")==0) ) {
      functexp *thisguy = new functexp(&log10ff,exprstack.top());
      exprstack.pop();
      exprstack.push(thisguy);
      continue;
    }
    if ((token.compare("ln")==0) || 
        (token.compare("Ln")==0) || 
        (token.compare("LN")==0) ) {
      functexp *thisguy = new functexp(&lnff,exprstack.top());
      exprstack.pop();
      exprstack.push(thisguy);
      continue;
    }
    if ((token.compare("exp")==0) || 
        (token.compare("Exp")==0) || 
        (token.compare("EXP")==0) ) {
      functexp *thisguy = new functexp(&expff,exprstack.top());
      exprstack.pop();
      exprstack.push(thisguy);
      continue;
    }
    if ((token.compare("sqrt")==0) || 
        (token.compare("Sqrt")==0) || 
        (token.compare("SQRT")==0) ) {
      functexp *thisguy = new functexp(&sqrtff,exprstack.top());
      exprstack.pop();
      exprstack.push(thisguy);
      continue;
    }
    if ((token.compare("abs")==0) || 
        (token.compare("Abs")==0) || 
        (token.compare("ABS")==0) ) {
      functexp *thisguy = new functexp(&absff,exprstack.top());
      exprstack.pop();
      exprstack.push(thisguy);
      continue;
    }
    // if we got here, must be a new token
    for (k = 0;k < constnames->size();k++)
      if ((*constnames)[k]==token)
        break;
    if (k < constnames->size()) {
      exprstack.push(copyexpr((*constnumvals)[k]));
      continue;
    }

    for (k=0;k< canonvars->size();k++)
      if ((*canonvars)[k]->clipsname==token)
        break;
    if (k==canonvars->size())
      throw(string("variable ")+token+" not declared");
    (*canonvars)[k]->isused = true;
    exprstack.push(new physvarptr(k));
  }
  delete toklist;
  if (exprstack.size() != 1) {
    char buf[6];
    sprintf(buf,"%5d",exprstack.size());
    
    string message(string("getAnEqn had ") + buf 
                   + string(" rather than 1 item at end\n"));
    throw(message);
  } else {        
    if (exprstack.top()->etype != binop)
      throw(string("getAnEqn: trying to return equation not a binop"));
    binopexp * tempbin = (binopexp *)exprstack.top();
    exprstack.pop();
    DBG(cout << "getAnEqn returns " << tempbin->getInfix() << endl;);
    return(tempbin);
  }
}

//  Any equations generated by the SGG should be completely correct

bool getCanonEqn(const string bufst) {
  binopexp * thiseq = getAnEqn(bufst,true);
  canoneqf->push_back(thiseq);
  return(true);
}

bool getStudEqn(int slot, const string bufst) {
  if ((slot < 0) || (slot >= HELPEQSZ))
    throw(string("Invalid slot number in getStudEqn"));
  if (studeqf[slot] != (binopexp *)NULL) studeqf[slot]->destroy();
  studeqf[slot] = (getAnEqn(bufst,true));
  return(true);
}
