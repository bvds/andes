// ispos.cpp	
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved

//	bool isnonneg(string name)
//	bool ispositive(string name)
//	bool isnonneg(const expr *)
//	bool ispositive(const expr *)

#include "decl.h"
#include "dbg.h"
#include "extstruct.h"
using namespace std;

#define DBG(A) DBGF(ISPOS,A)

/************************************************************************
 *   bool isnonneg(string name)						*
 *      returns true if the clipsname indicates variable must be 	*
 *      positive or zero ( magnitudes of vectors, or known positives)	*
 ************************************************************************/
bool isnonneg(string name)
{
  return ( (name.substr(0,3) == "mag")  || ispositive(name));
}

/************************************************************************
 *   bool ispositive(string name)					*
 *      returns true if the clipsname indicates variable must be 	*
 *      strictly positive						*
 *   currently this means mass, r_rev, or g				*
 ************************************************************************/
bool ispositive(string name)
{
  return( (name.substr(0,4) == "mass")  ||
	  (name.substr(0,5) == "r_rev") ||
	  (name.substr(0,1) == "g")	||
	  (name.substr(0,18) == "the_time_at_during"));
}

bool isnonneg(const expr * e)
{
  switch (e->etype)
    {
    case numval:
      return (((numvalexp *)e)->value >=0);
    case physvart:
      return ((*canonvars)[((physvarptr *)e)->varindex]->isnonneg);
    case function:
    case binop:
      return(false);		// we could do better with some work
    case n_op:
      {
	bool answer = true;
	for (int k=0; k < ((n_opexp *)e)->args->size(); k++)
	  answer &= isnonneg((*((n_opexp *)e)->args)[k]);
	DBG( cout << "isnonneg returning " << ((answer) ? "true" : "false") 
	     << " on " << e->getInfix() << endl;);
	return(answer);
      }
    case unknown:
    case fake:
    default:
      throw(string("isnonneg called with sick expr type"));
    }
}

bool ispositive(const expr * e)
{
  switch (e->etype)
    {
    case numval:
      return (((numvalexp *)e)->value > 0);
    case physvart:
      return (( (*canonvars)[((physvarptr *)e)->varindex]->isnonneg) &&
	      ( (*canonvars)[((physvarptr *)e)->varindex]->isnonzero) );
    case function:
    case binop:
      return(false);		// we could do better with some work
    case n_op:
      {
	bool answer = true;
	for (int k=0; k < ((n_opexp *)e)->args->size(); k++)
	  {
	    DBG( cout << k << " term in ispos" ;);
	    answer &= ispositive((*((n_opexp *)e)->args)[k]);
	    DBG( cout << ((answer) ? "true" : "false") << endl;);
	  }
	DBG( cout << "ispos returning " << ((answer) ? "true" : "false") 
	     << " on " << e->getInfix() << endl;);
	return(answer);
      }
    case unknown:
    case fake:
    default:
      throw(string("isnonneg called with sick expr type"));
    }
}
