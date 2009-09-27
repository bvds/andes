// EXPR header file		expr.h  revised starting 12/12 to reintro dimen
//					version with no minuses or unknown
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

#ifndef EXPRH
#define EXPRH
#include <string>
#include <vector>
#include <iostream>
#include <math.h>
#include <cstring>
#include <cstdlib>
using namespace std;

#include "dimens.h"

enum exprtype { unknown, numval, physvart, binop, n_op, function, fake };

// enum vartype { unspecified, force, mass, etc }  now only for physvars

class expr			/* intended to be abstract, inherited by */
{				/* physvar, binexp, function */
 public:
  // member fields:
  bool known;			/* not yet fully utilized */
  exprtype etype;
  dimens MKS;

  // constructors:
  expr() : known(false), etype(unknown) { }
  expr( exprtype type ) : known(false), etype(type) { }
  expr(expr &);
  // virtual destructor
  // destructors for the derived classes should eventually replace the
  // destroy function.
  virtual ~expr() = 0;
  // functions:
  bool isknown();
  void setknown();
  virtual string getInfix() const =0;
  virtual void pretty(int indent)=0;
  virtual void dbgprint(int indent)=0;
  virtual string getLisp(bool) const =0;
  void destroy();
  double getlengthd() const;
  double getmassd() const;
  double gettimed() const;
  double getcharged() const;
  double gettempd() const;
};


class primexpr	: public expr
{
 public:
  primexpr() : expr(unknown) { }
  primexpr(exprtype type) : expr(type) { }
};

class fakeexpr	: public primexpr	/* just to stick on stack as marker */
{
 public:
  fakeexpr() : primexpr(fake) { }
  string getInfix() const
    {return(string("Shouldn't be getInfix'ing a fakeexp\n"));}
  void pretty(int indent)
  { std::cout << "Shouldn't be printing a fakeexp" << endl; }
  void dbgprint(int indent)
  { std::cout << "Shouldn't be printing a fakeexp" << endl; }
  string getLisp(bool junk) const
    {return(string("Shouldn't be getLisp'ing a fakeexp\n"));}
};

/************************************************************************
 * numvalexp   numerical value with units, and eventually with errors	*
 ************************************************************************/
class numvalexp	: public primexpr
{
 public:
  double value;			/* set in getavar, indysgg, not used */
				/* appears to be in prefUnits, not SI */
  double abserr;		/* added 5/21/01, but not yet used */
  numvalexp(int value) : primexpr(numval), value((double) value) { 
    known=true; abserr = -1.;}
  numvalexp(float value) : primexpr(numval),value((double) value) {
    known=true; abserr = -1.;}
  numvalexp(double value) : primexpr(numval), 
    value((double) value) { known=true; abserr = -1.;}
  numvalexp(string value) : primexpr(numval), value(atof(value.c_str()))
    { known=true; abserr = -1.;}
  string getInfix() const;
  void pretty(int indent);
  void dbgprint(int indent);
  string getLisp(bool) const;
};

class physvarptr : public primexpr
{
 public:
  int varindex; 
  physvarptr() : primexpr(physvart), varindex(-1) { }
  physvarptr(int k);
  string getInfix() const;
  void pretty(int indent);
  void dbgprint(int indent);
  string getLisp(bool) const;
}; 

enum vartype { accel, angle, angaccel, angmom, angvel, length,
	       energy, force, inertiamom, mass, momentum, springk,
	       timev, torque, velocity, 
	       integer, real, unspecified };


class physvar
{
 protected:
 public:
  double value;
  double abserr;		/* added 5/21/01 */
  string prefUnit;
  string clipsname;  // canonical name?
  string shortname; // no longer used?
  string studname;  // no longer used?
  vartype type;
  dimens MKS;
  bool isnonneg;
  bool isnonzero;
  bool isparam;   // solutions are independant of the value of this variable.
  bool keepalgebraic;  // There is no numerical value for this variable.
  bool isused;

  physvar() :
    value(HUGE_VAL), abserr(-1.),  prefUnit(""), clipsname("unknown"), 
    shortname("?"),
    type(unspecified),
    isnonneg(false), 
    isnonzero(false), isparam(false), keepalgebraic(false), isused(false) { }
  physvar(string clp) :
    value(HUGE_VAL), abserr(-1.),  prefUnit(""), clipsname(clp), 
    shortname("?"),type(unspecified), isnonneg(false), 
    isnonzero(false), isparam(false), keepalgebraic(false),isused(false) { }
  ~physvar();
  void putclipsname(string);
  void putshortname(string);
  void putvartype(vartype);
  bool checkifknown();		/* if calculable, does so */
  double getmassd() const;
  double getlengthd() const;
  double gettimed() const;
  double getcharged() const;
  double gettempd() const;
  void putdimens(int lengthd, int massd, int timed, int charged,
			  int tempd);
  void putdimens(double lengthd, double massd, double timed, 
			  double charged, double tempd);

  string getInfix() const;
  void pretty(int indent);
  void dbgprint(int indent);
  string getLisp(bool) const;
  // not yet  bool studknows();
};

enum optype { pluse, multe, divbye, topowe, equalse, grte, gree,
	      sine, cose, tane, expe, lne, log10e, sqrte, abse };

class oper
{
 public:
  optype opty;
  string printname;
  oper(optype op, string printname) : opty(op), printname(printname) { }
};

class binopexp	:	public expr
{
 public:
  oper *op;
  expr *lhs;
  expr *rhs;
  binopexp() : expr(binop) { }
  binopexp(oper *op, expr *lhs, expr *rhs);
  string getInfix() const;
  string solprint(bool forhelp) const;
  void pretty(int indent);
  void dbgprint(int indent);
  string getLisp(bool) const;
};

class functexp	:	public expr
{
 public:
  oper *f;
  expr *arg;
  functexp() { etype=function; }
  functexp(oper *f,expr * arg);
  string getInfix() const;
  void pretty(int indent);
  void dbgprint(int indent);
  string getLisp(bool) const;
};

class n_opexp	:	public expr
{
 public:
  oper *op;
  vector<expr *> *args;
  n_opexp() { etype=n_op; args = new vector<expr *>; }
  n_opexp(oper *op);
  void addarg(expr *arg);
  string getInfix() const;
  void pretty(int indent);
  void dbgprint(int indent);
  string getLisp(bool) const;
};

#endif
