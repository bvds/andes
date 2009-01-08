// getall.cpp
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

// read in equations and variable specs from Collin/Anders style file

#include <fstream>
#include <string>
#include <vector>
#include <stdio.h>
using namespace std;
#include "decl.h"
#include "extoper.h"
#include "extstruct.h"
#include "mconst.h"
#include "dbg.h"
#include "unitabr.h"

#define DBG(A) DBGF(GETEQS,A)

bool getall(const string oneline);
bool getCanonEqn(const string bufst);		// moved to getaneqn.cpp
bool getavar(const string bufst);
int getavarwu(const string bufst,const bool varNew,    // moved to getavar.cpp
	       const bool valToo, double & SIval);
bool makenn(const string bufst);
bool makepos(const string bufst);
bool makenz(const string bufst);
bool makepar(const string bufst,const bool keep_algebraic);
//  bool setacc(const string bufst);   on hold, wait for decent err treatment
numvalexp * parseunit(stack<string> *toklist); 		// in getaneqn.cpp

/************************************************************************
 * bool startwith(buf, compstr) 					*
 *	returns true if the first L characters of buf are	 	*
 *	case-insensitively equal to compstr, which is a 		*
 *	lower-case string of length L					*
 ************************************************************************/
bool startwith(const string buf, const string compstr)
{
  if (buf.length() < compstr.length()) return false;
  string bufsub = buf.substr(0,compstr.length());
  for (int k = 0; k < bufsub.length(); k++) bufsub[k] = tolower(bufsub[k]);
  return(bufsub == compstr);
}

/************************************************************************
 * bool getall(string ) Gets a string from the help system, processes it*
 *									*
 * string is one of  [Note: first word is case-insensitive]		*
 *	(= ... 		a lisp form equation, to be entered in list	*
 *	(Variable varname)	TO BE CHANGED. enter varname in list	*
 *	(SVAR varname unitstring)
 *	(nonnegative varname)  \					*
 *	(positive varname)	\__ set corresponding attribute of	*
 *	(nonzero varname) 	/   variable varname			*
 *	(parameter varname)    /					*
 *	something beginning with <, space, or tab - line is ignored	*
 ************************************************************************/
bool getall(const string bufst)
{
  double junkval;
  string leadbuf = bufst.substr(0,12);
  if (bufst.substr(0,2) == "(=") { getCanonEqn(bufst); return(true); }
  if (startwith(bufst,"(variable"))
    {getavar(bufst); return(true); }
  if (startwith(bufst,"(svar"))
    {getavarwu(bufst,true,false,junkval); return(true); }
  if (startwith(bufst,"(nonnegative"))
    {makenn(bufst); return(true); }
  if (startwith(bufst,"(positive") )
    {makepos(bufst); return(true); }
  if (startwith(bufst,"(nonzero") )
    {makenz(bufst); return(true); }
  if (startwith(bufst,"(parameter"))
    {makepar(bufst,false); return(true); }
  if (startwith(bufst,"(answer-var"))
    {makepar(bufst,true); return(true); }
  if (bufst.substr(0,1) == "<") return(true);
  if (bufst.substr(0,1) == " ") return(true);
  if (bufst.substr(0,1) == "\t") return(true);
  cout << "Unexpected input as string:" << bufst << endl;
  cout << "in hex: ";
  for (int k = 0; k < bufst.length(); k++) 
    cout << (int)bufst[k] << ", ";
  cout << endl;
  throw(string("Unexpected input: ") + bufst);
}

/************************************************************************
 * bool getavar(string varname) defines a new variable with name 	*
 *	varname								*
 *   returns true if this is a new variable and it has been entered	*
 *	The new variable is set with all attributes false, and 		*
 *	dimens UNKNDIM (this is to be changed)				*
 *   returns false and does nothing if this variable is already in list	*
 ************************************************************************/
bool getavar(const string bufst)
{
  DBG (
    cout << "bufst is " << bufst.size() << " chars, "
	 << "text |" << bufst << "|" << endl; );
  int k, kstrt, kend;
  for(kstrt = 10; kstrt < bufst.size() && 
	(bufst[kstrt] == ' ' || bufst[kstrt] == '|'); kstrt++);
  // drop trailing ")", strip any "|"
  for (kend = bufst.size() - 2; kend >= kstrt 
	 && (bufst[kend] == ' ' || bufst[kend] == '|'); kend--);
  string newvar = bufst.substr(kstrt, kend-kstrt+1);
  DBG (
    cout << "variable |" << newvar << "|" << endl; );
  for (k = 0; k < canonvars->size(); k++)
    if (newvar == (*canonvars)[k]->clipsname) return(false);
  physvar * newpv = new physvar(newvar);
  canonvars->push_back(newpv);
  return(true);
}


/************************************************************************
 * bool makenn(const string varname) 					*
 *	searches for varname in variable list. If found, sets its	*
 *	nonnegative attribute and returns true. If not found, returns	*
 *	false.								*
 ************************************************************************/
bool makenn(const string bufst)
{
  int k, kstrt, kend;
  for(kstrt = 13; kstrt < bufst.size() && 
	(bufst[kstrt] == ' ' || bufst[kstrt] == '|'); kstrt++);
  // drop trailing ")", strip any "|"
  for (kend = bufst.size() - 2; kend >= kstrt 
	 && (bufst[kend] == ' ' || bufst[kend] == '|'); kend--);
  string newvar = bufst.substr(kstrt, kend-kstrt+1);
  DBG ( cout << "variable |" << newvar << "| set nn" << endl; );
  for (k = 0; k < canonvars->size(); k++)
    if (newvar == (*canonvars)[k]->clipsname)
      {
	(*canonvars)[k]->isnonneg = true;
	return(true);
      }
  return(false);
}

/************************************************************************
 * bool makepos(const string varname) 					*
 *	searches for varname in variable list. If found, sets both its	*
 *	nonnegative and nonzero attributes,  and returns true. 		*
 *	If not found, returns false.					*
 ************************************************************************/
bool makepos(const string bufst)
{
  int k, kstrt, kend;
  for(kstrt = 10; kstrt < bufst.size() && 
	(bufst[kstrt] == ' ' || bufst[kstrt] == '|'); kstrt++);
  // drop trailing ")", strip any "|"
  for (kend = bufst.size() - 2; kend >= kstrt 
	 && (bufst[kend] == ' ' || bufst[kend] == '|'); kend--);
  string newvar = bufst.substr(kstrt,kend-kstrt+1);
  DBG (cout << "variable |" << newvar << "| set pos?" << endl);
  for (k = 0; k < canonvars->size(); k++)
    if (newvar == (*canonvars)[k]->clipsname)
      {
	(*canonvars)[k]->isnonneg = true;
	(*canonvars)[k]->isnonzero = true;
	DBG ( cout << "variable |" << newvar << "| set pos" << endl; );
	return(true);
      }
  cerr << "didn't set variable |" << newvar << "| pos, as nonexistent"
       << endl; // diag
  return(false);
}

/************************************************************************
 * bool makenz(const string varname) 					*
 *	searches for varname in variable list. 				*
 *	If found, sets its nonzero attribute,  and returns true.	*
 *	If not found, returns false.					*
 ************************************************************************/
bool makenz(const string bufst)
{
  int k, kstrt, kend;
  for(kstrt = 9; kstrt < bufst.size() && 
	(bufst[kstrt] == ' ' || bufst[kstrt] == '|'); kstrt++);
  // drop trailing ")", strip any "|"
  for (kend = bufst.size() - 2; kend >= kstrt 
	 && (bufst[kend] == ' ' || bufst[kend] == '|'); kend--);
  string newvar = bufst.substr(kstrt,kend-kstrt+1);
  DBG ( cout << "variable |" << newvar << "| set nonzero?" << endl; );
  for (k = 0; k < canonvars->size(); k++)
    if (newvar == (*canonvars)[k]->clipsname)
      {
	(*canonvars)[k]->isnonzero = true;
	DBG ( cout << "variable |" << newvar << "| set nonzero" << endl; );
	return(true);
      }
  cerr << "didn't set variable |" << newvar << "| nonzero, as nonexistent"
       << endl; // diag
  return(false);
}

#if 0
//    setacc below is commented out, waiting for decent treatment of
//      error bars.
/************************************************************************
 * bool setacc(const string bufst) 					*
 *	bufst is of form (RelErr varname relerr)			*
 *	searches for varname in variable list. 				*
 *	If found, sets its abserr field to relerr,  and returns true.	*
 *	If not found, returns false.					*
 * Note: value is temporary and misleading, as it is a RELATIVE error   *
 *      It gets converted after value is known to ABSOLUTE error	*
 ************************************************************************/
bool setacc(const string bufst)
{
  int k;
  k = bufst.find(' ',8);
  if (k >= bufst.npos) throw(string("setacc grammar error in ")+bufst);
  string newvar = bufst.substr(8,k-8);
  string relerrstr = bufst.substr(k+1,bufst.npos-k-2);
  DBG ( cout << "variable |" << newvar << "| relerr to be set to |"
	<< relerrstr << "|" << endl; );
  for (k = 0; k < canonvars->size(); k++)
    if (newvar == (*canonvars)[k]->clipsname)
      {
	(*canonvars)[k]->relerr = atof(relerrstr);
	DBG ( cout << "variable |" << newvar << "| set to " << 
	      (*canonvars)[k]->relerr << endl; );
	return(true);
      }
  cerr << "didn't set variable |" << newvar << "|'s abserr, as nonexistent"
       << endl; // diag
  return(false);
}
#endif

/************************************************************************
 * bool makepar(const string varname) 					*
 *	searches for varname in variable list. 				*
 *	If found, sets its parameter attribute, writes an equation	*
 *	setting it t_o a "random" numerical value if no value argument  *
 *	included, and returns true.	                                *
 *	If not found, returns false.					*
 *	The value it is set to (if found) is e/pi for the first		*
 *	  parameter, and e^n/pi for the n'th one specified		*
 ************************************************************************/
bool makepar(const string bufst, bool keep_algebraic)
{
  int k,taglength;
  int kstrt, kend;
  double value;
  bool value_specified = false;
  extern int numparams;
  if(keep_algebraic)
    taglength=13;
  else
    taglength=12;

  // find beginning of symbol, stripping any '|'
  for(kstrt = taglength-1; kstrt < bufst.size() && 
	(bufst[kstrt] == ' ' || bufst[kstrt] == '|'); kstrt++);
  if (kstrt == bufst.size()) throw(string("parameter in bad format") + bufst);
  // find end of symbol, stripping any '|'
  for(kend = kstrt; kend < bufst.size() 
	&& bufst[kend] != ' ' && bufst[kend] != '|' && bufst[kend] != ')'; kend++);
  if ((kend == kstrt) || kend == bufst.size()) 
    throw(string("parameter in bad format ") + bufst);
  string newvar = bufst.substr(kstrt,kend-kstrt);
  DBG ( cout << "variable |" << newvar << "| set param?" << endl);
  // skip white space and check for optional numerical value argument
  for (kstrt = kend+1; kstrt < bufst.size() && bufst[kstrt] == ' '; kstrt++);
  if ((kend = parseanum(bufst, kstrt)) != kstrt) {
    	value = atof(bufst.substr(kstrt,kend-kstrt).c_str());
	    DBG(cout << "value = " << value << endl);
        value_specified = true;
  }
  
  for (k = 0; k < canonvars->size(); k++)
    if (newvar == (*canonvars)[k]->clipsname)
      {
	// The SGG assigns "parameter" to any "answer-var"
	// So we only assign value first time we see one of these.
	if(!(*canonvars)[k]->keepalgebraic && !(*canonvars)[k]->isparam)
	  {
	    numparams++;
	    // Bug #1374, a random choice can sometimes cause problems. 
	    // So use client-specified numerical value if one was provided.
		// Note no units on value, default units for variable are implied.
	    numvalexp* numval = new numvalexp(value_specified ? value
			                      : exp((double) numparams)/M_PI);
	    binopexp *eq = new binopexp(&equals,new physvarptr(k), numval);
	    paramasgn->push_back(eq);
	    DBG ( cout << "variable |" << newvar << "| set param, no. "
		  << numparams << endl
		  << "And equation for it " << eq->getInfix()<< endl; );
	  }

	// set the appropriate flag.
	if(keep_algebraic)
	  (*canonvars)[k]->keepalgebraic = true;
	else  
	  (*canonvars)[k]->isparam = true;
	
	// in the solver, however, isparam and keepalgebraic are disjoint
	if((*canonvars)[k]->keepalgebraic && (*canonvars)[k]->isparam)
	  (*canonvars)[k]->isparam = false;

	return(true);
      }
  cerr << "didn't set variable |" << newvar << "| param, as nonexistent"
       << endl; // diag
  return(false);
}
