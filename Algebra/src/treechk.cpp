// treechk.cpp
//   check that all nodes in an expression are different locations, or
//   add them to an already existing list
// 	This is to check for bugs - they should be all different.
//   
//   
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved

#include <string>
#include "decl.h"
#include "extoper.h"
#include "dbg.h"
#include <algorithm>
using namespace std;

#define DBG(A) DBGF(DOEQCHK,A)
/************************************************************************
 * bool treechk(const expr * const ex, vector<int> * vchk)		*
 *	if called with vchk != NULL, adds all node locations in ex to	*
 *	  the list vchk.						*
 * 	if called with vchk == NULL, makes such a list and then checks	*
 *	  it for duplicates. If none found, destroys list and returns	*
 *	  true. If duplicates found ???					*
 * bool listchk(vector<int> * vchk)					*
 *	does the same check on a previously assembled list,		*
 ************************************************************************/
bool listchk(vector<int> * vchk);

bool treechk(const expr * const ex, vector<int> * vchk)
{
  int k;
  bool topp = false;
  if (vchk == (vector<int> *) NULL)
    {
      topp = true;
      vchk = new vector<int>;
    }
  vchk->push_back((int) ex);
  switch (ex->etype)
    {
    case numval:
    case physvart:
    case fake:
      break;
    case function:
      treechk(((functexp *)ex)->arg,vchk);
      break;
    case binop:
      treechk(((binopexp *)ex)->lhs,vchk);
      treechk(((binopexp *)ex)->rhs,vchk);
      break;
    case n_op:
      for (k = 0; k < ((n_opexp *) ex)->args->size(); k++)
	treechk((*((n_opexp *)ex)->args)[k],vchk);
      break;
    default:
      throw(string("unknown expr type in treechk"));
    }
  if (!topp) return(true);
  return (listchk(vchk));
}

bool listchk(vector<int> * vchk)
{
  int k, q;
  bool retval = true;
  sort(vchk->begin(),vchk->end());
  int firstok = 0;		// one more than top of list of problems
  for (k = 1; k < vchk->size(); k++)
    {
      if ((*vchk)[k-1] != (*vchk)[k]) continue;
      DBG( cout << "listchk found duplicate expr " << (*vchk)[k]
		<< " which is " << ((expr *)((*vchk)[k]))->getInfix()
		<< endl; );
       retval = false;		// this is now the return value
      (*vchk)[firstok++] = (*vchk)[k];
    }
  if (retval)
    {
      delete vchk;
      vchk = (vector<int> *) NULL;
      return(true);
    }
  else
    {
      k = ((int)vchk->size()) - firstok;
      for (q = 0; q < k; q++) vchk->pop_back();
      return(false);
    }
}
