// parseclipseq.cc
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved

// stack<string> *parseclipseq(const string lispeq)
#pragma warning (disable: 4786)
#include <string>
#include <stack>
#include "decl.h"
using namespace std;

const bool KILLMINUS = true;
void delminus (string * p);

/************************************************************************
 *									*
 *  returns a stack of string tokens, each of which is one of		*
 *		( = + - * / ^ )					  	*
 *	or	a string starting with [A-Z] | [a-z] and continuing	*
 *		  with [A-Z] | [a-z] | [0-9] | - | _ | $		*
 *		(if KILLMINUS, minus signs are changed to 0 and 	*
 *			$ to N in the token string) 			*
 *	or	a number						*
 *  if top element returned is not ), something is wrong		*
 *									*
 ************************************************************************/

stack<string> *parseclipseq(const string & lispeq)
{
  int index=0;
  int j;
  stack<string> *tokstack = new stack<string>;
  char thiscar;

  for (index=0; index<lispeq.size(); )
    {
      thiscar = lispeq[index];
      string thiscarstr(1,thiscar);
      if (
	  (thiscar=='(') ||
	  (thiscar=='=') ||
	  (thiscar=='*') ||
	  (thiscar=='/') ||
	  (thiscar=='^') ||
	  (thiscar==')'))
	{
	  tokstack->push(thiscarstr);
	  index++;
	  continue;
	}
      if ((thiscar=='+') || (thiscar=='-') || (thiscar=='.'))
	{			// check if part of number
	  j = parseanum(lispeq, index);
	  if (j > index)
	    {
	      tokstack->push(lispeq.substr(index,j-index));
	      index = j;
	    }
	  else
	    {
	      tokstack->push(thiscarstr);
	      index++;
	    }
	  continue;
	}
      if (isdigit(thiscar))
	{
	  j = parseanum(lispeq,index);
	  tokstack->push(lispeq.substr(index,j-index));
	  index = j;
	  continue;
	}
      if (isalpha(thiscar))
	{
	  j = getclipsvar(lispeq,index);
	  string *p = new string(lispeq.substr(index,j-index));
	  if (KILLMINUS) delminus(p);
	  tokstack->push(*p);
	  delete p;
	  index = j;
	  continue;
	}
      if (thiscar==' ') index++;
      else
	{
	  cerr << "clipseq contained uninterpretable character "
	       << thiscarstr << " at index " << index << " in string " << endl;
	  cerr << lispeq << endl;
	  index++;
	}
    }
  return(tokstack);
}

void delminus (string * p)	// changed Oct 19 to kill $ as well
{
  size_t k;
  for (k=0;k<p->size();k++)
    {
      if ((*p)[k]=='-') (*p)[k++]='0';
      if ((*p)[k]=='$') (*p)[k++]='N';
    }
}
