// indysgg2.cpp   more routines to be called from the interface
//        
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved
//    simplifyEqn(int sourceSlot, int destSlot)

#include "decl.h"
#include "dbg.h"
#include "extstruct.h"
#include "indyset.h"
#include "unitabr.h"
#include <math.h>
#include "indysgg.h"
#include "binopfunctions.h"

using namespace std;

#define DBG(A) DBGF(INDYEMP,A)

// in extstruct: canonvars, canoneqf, studeqf
extern vector<valander *> studgrads;
int indyAddStudEq(int slot, const char* const equation);	// in indysgg

/************************************************************************
 * simplifyEqn(sourceSlot, destSlot)					*
 *    does arithmetic simplification on the equation in the source slot	*
 *    and returns a Lisp string to be infixed and placed in the dest    *
 *    slot. The algebra system inserts the new expression in its dest   *
 *    slot.								*
 *  As of June 13, it plugs in all assignment statements known to the   *
 *    student, recursively, first, before doing the arithmetic simplify *
 *  If no simplification is made, a zero-length string is returned, and *
 *    the dest slot is left empty					*
 ************************************************************************/
string simplifyEqn(const int sourceSlot, const int destSlot)
{
  int q;
  DBG(cout << "Entering simplifyEqn(" << sourceSlot<< ", " << destSlot 
      << ")" << endl;);
  if (destSlot >= STUDEQSZ) throw(string("destination slot ") 
				  + itostr(destSlot) + " doesn't exist");
  if (sourceSlot >= STUDEQSZ) throw(string("source slot ") 
				  + itostr(sourceSlot) + " doesn't exist");
  if (studeqf[sourceSlot] == (binopexp *) NULL)
    throw(string("Asked to simplify an empty equation"));
  expr * simp = copyexpr(studeqf[sourceSlot]);
  if (studeqf[destSlot] != (binopexp *) NULL) {
    studeqf[destSlot]->destroy();
    studeqf[destSlot] = (binopexp *) NULL; }
  if (studgrads[destSlot] != (valander *) NULL) {
    delete studgrads[destSlot];
    studgrads[destSlot] = (valander *) NULL; }
  // Addition of June 13, to first plug in all known assignments, recursively
  numpasses = 0;  
  vector<binopexp *> * eqn = new vector<binopexp *>;
  for (q = 0; q < HELPEQSZ; q++)
    if ((studeqf[q] != (binopexp *)NULL) && q != sourceSlot)
      eqn->push_back((binopexp *) copyexpr(studeqf[q]));
  vector<varindx> *vars = new vector<varindx>;
  // get a list of vars appearing in those equations
  for (q = 0; q < eqn->size(); q++)
    numunknowns((*eqn)[q],*vars,false);	// just to extract vars
  DBG(cout << "simplifyEqn found " << vars->size() << "vars and "
      << eqn->size() << " equations" << endl;);
  vector<binopexp *> * soleqs = new vector<binopexp *>;
  recassign(eqn,vars,soleqs);
  DBG(cout << "simplifyEqn found " << soleqs->size() << "solved vars" <<endl;);
  for (q = 0; q < soleqs->size(); q++)
    substin(simp,(*soleqs)[q]);
  // clean up
  for (q = 0; q < eqn->size(); q++) (*eqn)[q]->destroy();
  for (q = 0; q < soleqs->size(); q++) (*soleqs)[q]->destroy();
  delete vars;
  delete eqn;
  delete soleqs;
  // end of additions of June 13

  flatten(simp);		// added July 19
  eqnumsimp(simp,true);
  DBG(cout << "simplifyEqn produced " << simp->getInfix() << endl;);
  if (equaleqs(simp,studeqf[sourceSlot])) return string("");
  if ((simp->etype != binop) || ((binopexp *)simp)->op->opty != equalse)
    throw(string("simplifyEqn apparently made a non-equation! impossible"));
  indyAddStudEq(destSlot,simp->getLisp(false).c_str());
  string answer = simp->getLisp(true).c_str();
  simp->destroy();
  return(answer);
}
