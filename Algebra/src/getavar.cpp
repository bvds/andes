// getavar.cpp
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved

// parse results from reading in variable specs from Collin/Anders style file
//  or solution file

#include <string>
#include <vector>
#include <stdio.h>
#include <math.h>
using namespace std;
#include "decl.h"
#include "extstruct.h"
#include "mconst.h"
#include "dbg.h"
#include "unitabr.h"

#define DBG(A) DBGF(GETEQS,A)
extern unitabrs unittable;
numvalexp * getfromunits(const string & unitstr);	// in unitabr.cpp

/************************************************************************
 * int getavarwu(string buf, bool varNew, 			 	*
 *		bool valToo, double & SIvalue)			 	*
 *	defines a new variable	 					*
 *   buf is a string varname unitstr (if valToo false)			*
 *	or varname number unitstr (if valToo true)			*
 *   varname is the name of the new variable  (if varNew)		*
 *	otherwise a variable previously defined with units but not value*
 *   number is the numerical value, 					*
 *	unitstr is the units, in book form, for the new variable	*
 *   returns the index in canonvars if this is a new variable that has	*
 *	been inserted or, if !varNew, where it was found		*
 *	A new variable is set with all attributes false, and 		*
 *	dimens set. The value field and prefUnit string in the physvar	* 
 *	is set as given, and if valToo, the value, converted to SI 	* 
 *	units, is returned in SIvalue 					*
 *   could return -1 if the physvar was found already there if varNew,	*
 *	or if it was not found when varNew=false, but right now it	*
 *	throws an exception instead.					*
 ************************************************************************/
// getavarwu is called in three different contexts, which is responsible 
//   for some of the kludginess here
//	1) at the beginning of solver, on  (SVAR g_EARTH m/s^2)
//	   to create a new physvar and enter its prefUnit, without value
//	2) at the beginning of solution checker, to read in the value
//	   from the solution file:  (g_EARTH          9.8 m/s^2 )
//	   It is supposed to check the units and add the SIvalue, but
//	   NOT create a new variable
//	3) in indy, from exactly the same form as output. But now
//	   this is a new variable to be added to the variable list

int getavarwu(const string bufst, const bool varNew, const bool valToo,
	       double & SIvalue)
{
  int k, kstrt;
  int varindx;
  double value;
  physvar * newpv;
  numvalexp * temp;

  DBG (
    cout << "bufst is " << bufst.size() << " chars, "
	 << "text |" << bufst << "|" << endl; );
  bool started = false;
  kstrt = 6;
  for (k = kstrt; k < bufst.size(); k++) if (bufst[k] != ' ') break;
  if (k == bufst.size()) throw(string("nothing after (SVAR "));
  kstrt = k;
  for (k = kstrt; k < bufst.size(); k++) if (bufst[k] == ' ') break;
  if ((k == kstrt) || k >= bufst.size()) throw(string("svar in bad format ")
					   + bufst);
  string newvar = bufst.substr(kstrt,k - kstrt);
  DBG ( cout << "variable |" << newvar <<"|" << endl; );
  kstrt = k+1;
  for (k = kstrt; k < bufst.size(); k++) if (bufst[k] != ' ') break;
  kstrt = k;
  if (valToo) {
    int j = parseanum(bufst,k);
    if (j == k)
      throw(string(
	   "getavarwu(.,.,true,.) requires numerical value before units"));
    value = atof(bufst.substr(k,j-k).c_str());
    DBG ( cout << "value = " << value << endl; );
    kstrt=j+1;
    for (k = kstrt; k < bufst.size(); k++) if (bufst[k] != ' ') break;
    kstrt = k;
  }
  else value = 1.;
  k = bufst.size() - 2;		// want it to be last nonblank, ignoring ")"
  for (;k >= kstrt;k--) if (bufst[k] != ' ') break;
  if (k < kstrt) throw(string("getavarwu found no unit string"));
  string unitstr = bufst.substr(kstrt,k-kstrt+1);
  DBG ( cout << "units |" << unitstr << "|" << endl; );
  for (k = 0; k < canonvars->size(); k++)
    if (newvar == (*canonvars)[k]->clipsname) {
      if (varNew) return(-1);
      varindx = k;
      if (unitstr != (*canonvars)[k]->prefUnit)
	throw(string("preferred units disagree in physvar and solution ")
	      + (*canonvars)[k]->clipsname + " says " +
	      (*canonvars)[k]->prefUnit + " while solution says " + unitstr);
      break;
    }
  if (!varNew) {
    if ( k == canonvars->size())
      throw(string("asked to enter info for nonexistant variable ")
	    + newvar);
    else {
      newpv = (*canonvars)[varindx];
      DBG( cout << "Setting value on established physvar " << varindx 
	   << " whose name is " << newpv->clipsname << endl;);
      temp = getfromunits(unitstr);
      DBG( cout << "Unitstr equiv to " << temp->getInfix() << endl;);
    }
  }
  else {
    newpv = new physvar(newvar);
    //    newpv->isnonneg = false;    new (6/5/01) constructor does this
    //    newpv->isnonzero = false;
    //    newpv->isparam = false;
    //    newpv->isused = false;
    temp = getfromunits(unitstr);
    newpv->MKS = temp->MKS;
    newpv->prefUnit = unitstr;
  }
  if (valToo) { 
    newpv->value = value;
    SIvalue = temp->value * value;
  }
  temp->destroy();
  if (varNew) {
    DBG(cout << "Getavarwu defined " << newpv->clipsname << " with units "
	<< newpv->MKS.print() << endl; );
    varindx = canonvars->size();
    canonvars->push_back(newpv);
  }
  DBG( cout << "About to return true from getavarwu" << endl;);
  return(varindx);
}
