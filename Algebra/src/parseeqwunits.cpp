// parseeqwunits.cpp
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

// based on parseclipseq, but this goes into special mode on finding
//   dnum. It also has no conversions of - and $
#pragma warning (disable: 4786)
#include <string>
#include <stack>
#include "decl.h"
#include "dbg.h"
using namespace std;

#define DBG(A) DBGF(GETEQS,A)

/************************************************************************
 *  parseEqWUnits(const string & lispeq)				*
 *  returns a stack of string tokens, each of which is one of		*
 *		( = + - * / ^ )					  	*
 *	or	a string starting with [A-Z] | [a-z] and continuing	*
 *		  with [A-Z] | [a-z] | [0-9] | _ | $ | :		*
 *	or	a number						*
 *      or      string "U)", which is used for ending units		*
 *  if top element returned is not ), something is wrong		*
 ************************************************************************/

stack<string> *parseEqWUnits(const string & lispeq)
{
  int index=0;
  int j;
  bool unitparse, indenom;
  stack<string> *tokstack = new stack<string>;
  char thiscar;

  unitparse = false;
  for (index=0; index<lispeq.size(); )
    {
      thiscar = lispeq[index];
      string thiscarstr(1,thiscar);
      if (
	  (thiscar=='(') ||
	  (thiscar=='=') ||
	  ((thiscar=='*') && !unitparse) ||
	  ((thiscar=='/') && !unitparse) ||
	  (thiscar=='^') ||
	  ((thiscar==')') && !unitparse))
	{
	  tokstack->push(thiscarstr);
	  DBG(cout << "parseEqWUnits just pushed " << tokstack->top() 
	      << endl;);
	  index++;
	  continue;
	}
      if (unitparse) {
	if (thiscar==')') {
	  tokstack->push("U)");
	  DBG(cout << "parseEqWUnits just pushed " << tokstack->top() 
	      << endl);
	  unitparse = false; 
	  index++; continue;
	}
	if (thiscar=='*') {
	  tokstack->push(".");
	  DBG(cout << "parseEqWUnits just pushed " << tokstack->top() 
	      << endl); 
	  index++; continue;
	}
	if (thiscar=='/') {
	  if (indenom) {
	    tokstack->push(".");
	    DBG(cout << "parseEqWUnits just pushed " << tokstack->top() 
		<< endl); }
	  else { 
	    tokstack->push("/"); 
	    DBG(cout << "parseEqWUnits just pushed " << tokstack->top() 
		<< endl);
	    indenom = true; }
	  index++; continue;
	} // end of if /
      }	  
      if ((thiscar=='+') || (thiscar=='-') || (thiscar=='.'))
	{			// check if part of number
	  j = parseanum(lispeq, index);
	  if (j > index)
	    {
	      tokstack->push(lispeq.substr(index,j-index));
	      DBG(cout << "parseEqWUnits just pushed " << tokstack->top() 
		  << endl);
	      index = j;
	    }
	  else
	    {
	      tokstack->push(thiscarstr);
	      DBG(cout << "parseEqWUnits just pushed " << tokstack->top() 
		  << endl);
	      index++;
	    }
	  continue;
	}
      if (isdigit(thiscar))
	{
	  j = parseanum(lispeq,index);
	  tokstack->push(lispeq.substr(index,j-index));
	  DBG(cout << "parseEqWUnits just pushed " << tokstack->top() 
	      << endl;);
	  index = j;
	  continue;
	}
      if (isalpha(thiscar) || (thiscar == '$') || (thiscar == ':'))
	{
	  j = getclipsvar(lispeq,index);
	  string *p = new string(lispeq.substr(index,j-index));
	  index = j;
	  if ((p->compare("dnum")==0) || (p->compare("DNUM")==0) )
	  {
	    if (unitparse) throw(string("dnum inside dnum"));
	    unitparse = true;
	    indenom = false;
	  }
	  if (p->compare("DNUM")==0) p->assign("dnum");
	  if (p->compare(":ERROR")==0) p->assign(":error");
	  tokstack->push(*p);
	  DBG(cout << "parseEqWUnits just pushed " << tokstack->top() 
	      << endl);
	  delete p;
	  continue;
	}
      if ((thiscar=='|') || (thiscar==' ') || (thiscar=='\r') 
	  || (thiscar=='\n')) index++;
      else
	{
	  cerr << "ParseEqWUnits: string contained uninterpretable character "
	       << thiscarstr << " at index " << index << " in string " << endl;
	  cerr << lispeq << endl;
	  index++;
	}
    }
  return(tokstack);
}
