// Class expr, new version of physvar  Revised to reintroduce dimens
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

#include <string>
#include "decl.h"
#include "unitabr.h"
#include <stdio.h>
#include <math.h>
// using namespace std;
#include "dbg.h"

#define DBG(A) DBGF(EXPRDB,A)

extern vector<physvar *> * canonvars;

string itostr(int);					// in utils.cpp

bool expr::isknown() { return(known); }
void expr::setknown() { this->known = true; }

physvarptr::physvarptr(int k) : varindex(k)
  {etype = physvart; MKS =(*canonvars)[k]->MKS; }


/************************************************************************
 * binopexp constructor with known op, lhs, and rhs			*
 * NOTE equals constructor has side effect of fixing unknown dimens 	*
 *    in one side if other side has known dimens. Hope that's OK	*
 ************************************************************************/
binopexp::binopexp(oper *op, expr *lhs, expr *rhs) :
    op(op), lhs(lhs), rhs(rhs) { 
  etype=binop; 
  switch (op->opty) 
    {
    case grte:
    case gree:
    case equalse:
      {
      if (lhs->MKS.inconsp() || rhs->MKS.inconsp()) {
	MKS.set_incons();
	return; }
      if (lhs->MKS.unknp()) { MKS = rhs->MKS; lhs->MKS = rhs->MKS; return; }
      if (rhs->MKS.unknp()) { MKS = lhs->MKS; rhs->MKS = lhs->MKS; return; }
      if (lhs->MKS == rhs->MKS) { MKS = rhs->MKS; return; }
      MKS.set_incons();
      return;
      }
    case topowe:
      {				
      if (!(rhs->MKS.zerop() || rhs->MKS.unknp())) {          // rhs must be 
	MKS.set_incons();      // dimensionless
	return;  }
      double expval;
      if (lhs->MKS.zerop()) expval = 1.; // doesn't matter what 0 mult by
      else {
	if (rhs->etype == numval) expval = ((numvalexp *)rhs)->value;
	else {
	  expr * temp = copyexpr(rhs);
	  eqnumsimp(temp,true);
	  if (temp->etype == numval) expval = ((numvalexp *)temp)->value;
	  else {
	    MKS.set_incons();   // can't determined
	    temp->destroy();			
	    return; }
	  temp->destroy(); }
      }
      MKS = lhs->MKS;
      MKS *= expval;
      return;
      }
    case divbye:
      {
      if (lhs->MKS.inconsp() || rhs->MKS.inconsp()) {
	MKS.set_incons();
	return; }
      if (lhs->MKS.unknp() || rhs->MKS.unknp()) {
	MKS.set_unkn();
	return; }
      MKS = rhs->MKS;
      MKS *= -1.;
      MKS += lhs->MKS;
      return;
      }
    default:
      throw(string("constructor for unknown type of binop"));
    } // end of switch
}

functexp::functexp(oper *f,expr * arg) : 
    f(f), arg(arg) {
  etype=function; 
  if (arg->MKS.inconsp()) { MKS = arg->MKS; return; }
  switch (f->opty)
  {
  case sqrte:
    MKS = arg->MKS;
    MKS *= 0.5;
    return;
  case abse:
    MKS = arg->MKS;
    return;
  case sine:    
  case cose:    
  case tane:    
  case expe:    
  case lne:    
  case log10e:    
    if (arg->MKS.zerop() || arg->MKS.unknp()) {
      MKS.put(0,0,0,0,0);
      return; 
    }
    MKS.set_unkn();
    return; 
  default:
    throw("functexp with illegal type");
  } // end of switch
}

n_opexp::n_opexp(oper *op) : op(op) 
{ 
  etype=n_op;
  args = new vector<expr *>;
  // cout << "n_opexp create " << args << endl;
  if (op->opty == multe) { 
    MKS.put(0,0,0,0,0);   // to be activated after addarg replaces arg->push
    return; }
  if (op->opty == pluse) { return; }
  throw(string("n_op created with illegal type") + itostr(op->opty));
}

/************************************************************************
 *  addarg to a n_op expr.  In addition to pushing the arg on the 	*
 *	this->args list, it does the following units fixing:		*
 *    For a mult:  if the previous dimensions of the product and the	*
 *	dimensions of the new arg are known, the product's dimensions	*
 *	are incremented by those of the new arg. 			*
 *    For a plus:  if the sum has unknp and the arg has known, or 	*
 *	vice-versa, sets the unkn one to the known.			*
 *	If both known, if inconsistent makes sum dimens incons		*
 ************************************************************************/
void n_opexp::addarg(expr * arg)
{
  args->push_back(arg);
  if (op->opty == multe)
    {
      MKS += arg->MKS;
      return;
    }
  DBG(cout << "Adding arg " << arg->getInfix() << " to plus" << endl;
      dbgprint(4); );
  if (MKS.inconsp()) return;		// must be plus
  if (MKS.unknp()) { 
    MKS = arg->MKS; 
    DBG(cout << "Added arg ";
	arg->dbgprint(6);
	cout << " to plus with unkn dim" << endl;
	dbgprint(2); );
    return; }
  if (arg->MKS.unknp()) arg->MKS = MKS;
  DBG(cout << "Added arg " << arg->getInfix() << " to plus" << endl;
      dbgprint(4); );
  return;
}



/************************************************************************
 * Declaration of the oper's						*
 * expressions of type function, binop or n_op have a field of type 	*
 *    ptr to oper, which gives the information on which kind. The 	*
 *    oper's are not created or destroyed with the expression, and 	*
 *    is one static oper for each type					*
 ************************************************************************/
oper myplus(pluse,"+");
oper mult(multe,"*");
oper divby(divbye,"/");
oper topow(topowe,"^");
oper equals(equalse,"=");
oper grt(grte,">");
oper gre(gree,">=");
#ifdef FAKEDEG
oper sinef(sine,"sindeg");
oper cosef(cose,"cosdeg");
oper tanff(tane,"tandeg");
#else
oper sinef(sine,"sin");
oper cosef(cose,"cos");
oper tanff(tane,"tan");
#endif
oper expff(expe,"exp");
oper lnff(lne,"ln");
oper log10ff(log10e,"log10");
oper sqrtff(sqrte,"sqrt");
oper absff(abse,"abs");
