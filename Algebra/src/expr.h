// EXPR header file		expr.h  revised starting 12/12 to reintro dimen
//					version with no minuses or unknown
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved

#ifndef EXPRH
#define EXPRH
#include <string>
#include <vector>
#include <iostream>
#include <math.h>
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
  string clipsname;
  string shortname;
  string studname;
  vartype type;
  dimens MKS;
  bool isnonneg;
  bool isnonzero;
  bool isparam;   // solutions are independant of the value of this variable.
  bool keepalgebraic;  // There is no numerical value for this variable.
  bool isused;

  physvar() :
    clipsname("unknown"), shortname("?"),type(unspecified),
    value(HUGE_VAL), abserr(-1.), prefUnit(""), isnonneg(false), 
    isnonzero(false), isparam(false), keepalgebraic(false), isused(false) { }
  physvar(string clp) :
    clipsname(clp), shortname("?"),type(unspecified),
    value(HUGE_VAL), abserr(-1.), prefUnit(""), isnonneg(false), 
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
  void physvar::putdimens(int lengthd, int massd, int timed, int charged,
			  int tempd);
  void physvar::putdimens(double lengthd, double massd, double timed, 
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
