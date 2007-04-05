// unitabr.cpp	get unit abbreviations and handle lookup
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved

//    with changes 3/22/01 to include prefixes. unitget now returns numval
#include "decl.h"
#include "dimens.h"
#include "unitabr.h"
#include <fstream>
#include "dbg.h"
using namespace std;
#pragma warning (disable: 4786)

#define DBG(A) DBGF(UNITS,A)

#define Asize(arr) (sizeof(arr)/sizeof(arr[0]))

numvalexp * parseunit(stack<string> *toklist);          // in getaneqn.cpp

struct punit {
  string abbrev;
  double value;
  int lenu;
  int massu;
  int secu;
  int chgu;
  int ku;
  int takespfx;			// 1 if can take a prefix, else 0
	punit() {}
	punit(string a, double v, int l, int m, int s, int c, int k, int t) {
		abbrev = a; value = v; lenu = l; massu = m; secu = s; chgu = c; ku = k; takespfx = t;
	}
} utab[] = {
#include "units.h"
};



struct pfx {
  string name;
  double value;
	pfx() {}
	pfx(string n, double v) {
		name = n; value = v;
	}
} pfxtab[] = {
#include "prefixes.h"
};

unitabrs unittable;

unitabrs::unitabrs()   // fill up tables from utab
{			     
  string abr;
  dimens dim;
  {for (int k = 0; k < Asize(utab); k++) {
    abr.assign(utab[k].abbrev);
    dim.put(utab[k].lenu,utab[k].massu,utab[k].secu,utab[k].chgu,utab[k].ku);
    abbrev.push_back(abr);
    value.push_back(utab[k].value);
    dims.push_back(dim);
    pfxable.push_back((utab[k].takespfx == 1) ? true : false);
  }}
  {for (int k = 0; k < Asize(pfxtab); k++) {
    abr.assign(pfxtab[k].name);
    pfxs.push_back(abr);
    pfxvals.push_back(pfxtab[k].value);
  }}
}

string unitabrs::match(const dimens dim)
{
  int j;
  for (j=0; j < abbrev.size(); j++)
    if ((dim == dims[j]) && (value[j] == 1.)) // note: we are here refusing
      return(abbrev[j]);			// to use units that are not
  return(string("None"));			// SI, with no prefix.
}

/************************************************************************
 *  unitget  takes a string representing an SI unit (with/wout prefix)	*
 *	and returns a numval for it. returns NULL if not found		*
 ************************************************************************/
numvalexp * unitabrs::unitget(const string & unitname)
{
  int k, q;
  numvalexp * retval;
  string pureunit = unitname;
  double pfxvalue = 1.;
  for (q = 0; q < abbrev.size(); q++)
    if (pureunit == abbrev[q]) break;
  if (q == abbrev.size()) {
    if ((unitname.substr(0,2) == "$m") ||(unitname.substr(0,2) == "mu")) {    
      pureunit = unitname.substr(2,unitname.size()-2);
      pfxvalue = 1.0E-6;
    }
    else {
      pureunit = unitname.substr(1,unitname.size()-1);
      for (k = 0; k < pfxs.size(); k++)
	if (unitname.substr(0,1) == pfxs[k]) {
	  pfxvalue = pfxvals[k];
	  break; }
      if (k == pfxs.size()) return((numvalexp *) NULL);
    }
    for (q = 0; q < abbrev.size(); q++)
      if ((pureunit == abbrev[q]) && pfxable[q]) break;
    if (q == abbrev.size()) return((numvalexp *) NULL);
  }
  retval = new numvalexp(pfxvalue * value[q]);
  retval->MKS = dims[q];
  return(retval);
}

numvalexp * getfromunits(const string & unitstr)
{
  numvalexp *temp = unittable.unitget(unitstr);
  if (temp == (numvalexp *)NULL) {
    // unitget couldn't find unit in table - might be composite
    DBG(cout << "unitget failed on " << unitstr 
	<< ", trying composite" << endl; );
    string tempstr = string("(dnum 1.0 ") + unitstr + ")";
    stack<string> *tempstack = parseEqWUnits(tempstr);
    if (tempstack->empty()) 
      throw(string("parseEqWUnits returned empty stack"));
    tempstack->pop();		// discard the U)
    temp = parseunit(tempstack);
    DBG(cout << "parseunit leaves " << tempstack->size() 
	<< " items on stack and returns" 
	<< ((temp == (numvalexp *)NULL) ? "null" : temp->getInfix())
	<< endl;);
    if (temp == (numvalexp *)NULL) 
      throw(string("getfromunits with unparsable units") + unitstr);
  }
  return(temp);
}


