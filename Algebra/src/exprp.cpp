// Class exprp, printing or string output from physvar and expr
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved

#include <string>
#include "decl.h"
#include "unitabr.h"
#include <stdio.h>
#include <math.h>
using namespace std;
#include "dbg.h"

#define DBG(A) DBGF(EXPRDB,A)

extern vector<physvar *> * canonvars;
extern unitabrs unittable;
extern vector<double> *numsols;

numvalexp * getfromunits(const string & unitstr);

string ustrp(double dpow)	// moved dots from here to unitprint, 7/26
{				// and gives absolute value of power
  int q;
  char buf[8];
  if(dpow==UNKNDIM)  // dimension not determined
    sprintf(buf,"^???");
  else if (lookslikeint(dpow,q))
    {
      q = abs(q);
      if (q==1) return (string(""));
      else sprintf(buf,"^%d",q);
    }
  else sprintf(buf,"^%.1lf",fabs(dpow)); // AW: should show abs val as above
  return(string(buf));
}


////////////////////////////////////////////////////////////////////////////
/******************************** PRINTING ********************************/
////////////////////////////////////////////////////////////////////////////

/************************************************************************
 * unitprint   return a string representing the units in a dimens	*
 ************************************************************************/
string unitprint(const dimens dim)
{
  string unitstr = unittable.match(dim);
  if (unitstr.compare("None") == 0) {
    unitstr.erase();
    string unitden;
    if (dim.getlengthd() > 0) unitstr.append("m" +ustrp(dim.getlengthd())+".");
    if (dim.getlengthd() < 0) unitden.append("m" +ustrp(dim.getlengthd())+"/");
    if (dim.getmassd() > 0) unitstr.append("kg" + ustrp(dim.getmassd())+".");
    if (dim.getmassd() < 0) unitden.append("kg" + ustrp(dim.getmassd())+"/");
    if (dim.gettimed() > 0) unitstr.append("s"	+ ustrp(dim.gettimed())+".");
    if (dim.gettimed() < 0) unitden.append("s"	+ ustrp(dim.gettimed())+"/");
    if (dim.getcharged() > 0) unitstr.append("C"+ustrp(dim.getcharged())+".");
    if (dim.getcharged() < 0) unitden.append("C"+ustrp(dim.getcharged())+"/");
    if (dim.gettempd() > 0) unitstr.append("K"	+ ustrp(dim.gettempd())+".");
    if (dim.gettempd() < 0) unitden.append("K"	+ ustrp(dim.gettempd())+"/");
    int usl = unitstr.length();
    if ((usl > 0) && (unitstr[usl-1] == '.')) unitstr.erase(usl-1,1);
    usl = unitden.length();
    if ((usl > 0) && (unitden[usl-1] == '/')) unitden.erase(usl-1,1);
    if (unitden.length()>0) { unitstr.append("/"); unitstr.append(unitden); }
    }
  return(unitstr);
}


/************************************************************************
 * expr::getInfix   returns a string representing an expr in fully 	*
 * 	parenthesized infix format					*
 ************************************************************************/

string numvalexp::getInfix() const {
  DBG( cout << "getInfix on numval" << endl; );
  int q;
  char valuenum[21];

  if (lookslikeint(value,q)) sprintf(valuenum,"%d",q);
  else if ((fabs(value) < 1.) && (fabs(value)> 0.001))
    sprintf(valuenum,"%19.16lf",value);
  else
    sprintf(valuenum,"%20.14lG",value);
#ifdef UNITENABLE  
  string unitstr = unitprint(MKS);
  if (unitstr.size() == 0)   return(string(valuenum));
  else return(string("(") + string(valuenum) + " " + unitstr + ")");
#else
  return(string(valuenum));
#endif
}

string physvarptr::getInfix() const {
  if (canonvars == (vector<physvar*>*)NULL) 
    return string("no physvar list");
  if (varindex < canonvars->size())
    return (*canonvars)[varindex]->clipsname; 
  else
    return string("physvarptr points to index greater than list size");
}

/************************************************************************
 * solprint of an assignment statement (ie. physvar = numval) returns a	*
 *	string suitable for passing to the help system, in the form	*
 *		(SVAR physvarname number units )   forhelp = false	*
 *      or	(= |physvarname| (DNUM number |units|))  forhelp = true	*
 *  If the physvar has prefUnit and it is consistent with numval's	*
 *	the value is converted to the prefUnit and output as number	*
 *  If the physvar does not have preferred units, value is in SI units	*
 *	and units is taken from first matching element in units.h	*
 *  If the prefUnit and the numval are inconsistent, throws exception	*
 *  Since 4/14/01, also enters SI value in numsols, which better 	*
 *	already exist and be big enough!				*
 ************************************************************************/
string binopexp::solprint(bool forhelp) const {
  int q;
  char valuenum[21];
  string unitstr;

  if ( op->opty != equalse)
    throw(string("Tried to write a solution that is not an equation"));
  if (lhs->etype != physvart)
    throw(string("Tried to write a solution with lhs not a variable"));
  int varidx = ((physvarptr *)lhs)->varindex;
  if (rhs->etype != numval)
    throw(string("Tried to write a solution with rhs not a value"));
  DBG(cout << "Solprint on " << getInfix() << endl;);
  double value = ((numvalexp *)rhs)->value;
  if ((numsols == 0L) || (numsols == (vector<double> *)NULL))
    throw string("solprint called without numsols existing");
  if (numsols->size() <= varidx)
    throw string("solprint called with numsols too small");
  (*numsols)[varidx] = value;
  if ((*canonvars)[varidx]->prefUnit != "") {
    unitstr = (*canonvars)[varidx]->prefUnit;
    DBG(cout << "Solprint got prefUnit " << unitstr << endl;);
    numvalexp * denom = getfromunits(unitstr);
    DBG(cout << "That unitstr means " << denom->getInfix() << endl;);
    if (!(((numvalexp *)rhs)->MKS == denom->MKS))
      throw(string(getInfix()) + " inconsistent with preferred units for "
	    + (*canonvars)[varidx]->clipsname);
    value = value / denom->value;
    denom->destroy();
  }
  else {
    DBG(cout << "Solprint got no prefUnit " << endl;);
    dimens dim = ((numvalexp *)rhs)->MKS;
    unitstr = unitprint(dim);
    DBG(cout << "Solprint: manufactured unitstr " << unitstr << endl;);
  }

  if (lookslikeint(value,q)) sprintf(valuenum,"%d",q);
  else if ((fabs(value) < 1.) && (fabs(value)> 0.001))
    sprintf(valuenum,"%19.16lf",value);
  else
    sprintf(valuenum,"%20.14lG",value);
  if (forhelp)
  return(string("(= |") 
	 + (*canonvars)[varidx]->clipsname
	 + "| (DNUM " + string(valuenum) + " |" + unitstr + "|))");
  else   return(string("(SVAR ") 
	 + (*canonvars)[varidx]->clipsname
	 + " " + string(valuenum) + " " + unitstr + " )");
}

string binopexp::getInfix() const {
  DBG(cout << "getInfix on binop" << endl;);
  
  return string(string("(") + lhs->getInfix() + string(" ") 
		+ op->printname + string(" ") + rhs->getInfix() + string(")"));
}


string functexp::getInfix() const {
  DBG( cout << "getInfix on functexp" << endl; );
  
  return string(string("(") + f->printname + string(" (") 
		+ arg->getInfix() + string("))"));
}


string n_opexp::getInfix() const {
  DBG( cout << "getInfix on n_op" << endl;);
  int k;
  string *ans = new string("( ");
  if (this->args->size() == 0) {
    ans->append( op->printname + ")");
    return (*ans);
  }
  for (k = 0; k+1 < this->args->size(); k++)
    ans->append((*(this->args))[k]->getInfix() + " " + op->printname + " ");
  ans->append((*(this->args))[k]->getInfix() + ")");

  return (*ans);		// memory leak?
}


/************************************************************************
 * expr::pretty(indent)							*
 *	prints out tree structure of expression, with indent spaces	*
 *	from left margin. Not used. Differs from dbgprint in that the	*
 *	latter names the kind of node, and spells out the units for	*
 *	each node							*
 ************************************************************************/
void numvalexp::pretty(int indent)
{
  int q;
  char valuenum[17];
  if (lookslikeint(value,q)) sprintf(valuenum," %d ",q);
  else sprintf(valuenum," %14.8lf ",value);
  cout << string(indent,' ') + valuenum << endl;
}

void physvarptr::pretty(int indent)
{
  if (canonvars == (vector<physvar *> *) NULL) 
    { cout << "no physvar list" << endl; return; }
  if (varindex < canonvars->size()) {
  cout << string(indent,' ') + (*canonvars)[varindex]->clipsname << endl;
  return; }
  else cout << "physvarptr points to index " << varindex 
	    << "greater than list size" << canonvars->size() << endl;
}

void binopexp::pretty(int indent)
{
  cout << string(indent,' ') + op->printname << endl;
  lhs->pretty(indent+2);
  rhs->pretty(indent+2);
}

void functexp::pretty(int indent)
{
  cout << string(indent,' ') + f->printname << endl;
  arg->pretty(indent+2);
}
void n_opexp::pretty(int indent)
{
  int k;
  cout << string(indent,' ') + op->printname << endl;
  for (k=0;k<this->args->size();k++)
    (*(this->args))[k]->pretty(indent+2);
}

/************************************************************************
 * expr::dbgprint(indent)						*
 *	prints out tree structure of expression, with indent spaces	*
 *	from left margin. Differs from pretty in that dbgprint prints	*
 *	out the  names the kind of node, and spells out the units for	*
 *	each node.							*
 ************************************************************************/
void numvalexp::dbgprint(int indent)
{
  int q;
  char valuenum[17];
  if (lookslikeint(value,q)) sprintf(valuenum," %d ",q);
  else sprintf(valuenum," %14.8lf ",value);
  cout << string(indent,' ') + "numval:  " + valuenum + "\t" + 
    MKS.print() << endl;
}

void physvarptr::dbgprint(int indent)
{
  if (canonvars == (vector<physvar *> *) NULL) 
    { cout << "no physvar list" << endl; return;  }
  if (varindex < canonvars->size()) {
  cout << string(indent,' ') + "physvar: " + (*canonvars)[varindex]->clipsname 
    + "\t" + MKS.print() << endl;
  return;
  }
  else cout << "physvarptr points to index " << varindex 
	    << "greater than list size" << canonvars->size() << endl;
}

void binopexp::dbgprint(int indent)
{
  cout << string(indent,' ') + "binop   " +  op->printname 
    + "\t" + MKS.print()<< endl;
  lhs->dbgprint(indent+2);
  rhs->dbgprint(indent+2);
}

void functexp::dbgprint(int indent)
{
  cout << string(indent,' ') + "funct:  " + f->printname 
    + "\t" + MKS.print()<< endl;
  arg->dbgprint(indent+2);
}
void n_opexp::dbgprint(int indent)
{
  int k;
  cout << string(indent,' ')  + "n_op:   " + op->printname 
    + "\t" + MKS.print()<< endl;
  for (k=0;k<this->args->size();k++)
    (*(this->args))[k]->dbgprint(indent+2);
}

/************************************************************************
 * getLisp(withbarp) outputs expression in lisp form,                   *
 *	prefix form inside parentheses                                  *
 * if withbarp, places vertical bars before and after variable names    *
 *      and units                                                       *
 ************************************************************************/
string numvalexp::getLisp(bool withbarp) const {
  DBG( cout << "getLisp on numval" << endl; );
  int q;
  char valuenum[21];

  if (lookslikeint(value,q)) sprintf(valuenum,"%d",q);
  else if ((fabs(value) < 1.) && (fabs(value)> 0.001))
    sprintf(valuenum,"%19.16lf",value);
  else
    sprintf(valuenum,"%20.14lG",value);
#ifdef UNITENABLE  
  string unitstr = unitprint(MKS);
  if (unitstr.size() == 0)   return(string("( ") +valuenum + " )");
  else return(string("(DNUM ") + string(valuenum) + 
	      ((withbarp) ? " |" + unitstr + "|)" : " " + unitstr + ")"));
#else
  return(string(valuenum));
#endif
}


string physvarptr::getLisp(bool withbarp) const {
  if (canonvars == (vector<physvar*>*)NULL) 
    return string("no physvar list");
  if (varindex < canonvars->size()) {
    if (withbarp)
      return (string("|") + (*canonvars)[varindex]->clipsname +"|"); 
    else return ( (*canonvars)[varindex]->clipsname ); 
  }
  else
    return string("physvarptr points to index greater than list size");
}


string binopexp::getLisp(bool withbarp) const {
  DBG(cout << "getLisp on binop" << endl;);
  return string(string("(")+ op->printname + string(" ") 
    + lhs->getLisp(withbarp) + string(" ") + rhs->getLisp(withbarp) 
		+ string(")"));
}


string functexp::getLisp(bool withbarp) const {
  DBG( cout << "getLisp on functexp" << endl; );
  return string(string("(") + f->printname + string(" ") 
		+ arg->getLisp(withbarp) + string(")"));
}


string n_opexp::getLisp(bool withbarp) const {
  DBG( cout << "getLisp on n_op" << endl;);
  int k;
  string *ans = new string("(");
ans->append( op->printname + " ");
  for (k = 0; k+1 < this->args->size(); k++)
    ans->append((*(this->args))[k]->getLisp(withbarp) + " ");
  ans->append((*(this->args))[k]->getLisp(withbarp) + ")");
  return (*ans);		// memory leak?
}
