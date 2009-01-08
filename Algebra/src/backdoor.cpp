// backdoor    let things be tested by entering fake equation in slot
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

//             numbered 11 on workbench

// enter by typing an equation of the form

//   var = (num1^2) + (arg1^3) + (arg2^4) + (arg3^5) + ...

//     num1 determines which function gets called, var should be any

//     declared dimensionful so returning UNITSNG is right answer.



vector<expr *> * theargs;



binopexp * getAnEqn(const string bufst, bool tight);



const char * getvarfromtrap(int argnum) {

  if ((*theargs)[argnum]->etype != physvart) 

    throw(string("Backdoor couldn't extract variable from arg ")

	  + itostr(argnum));

  return(((*canonvars)[

       ((physvarptr *)(*theargs)[argnum])->varindex])->clipsname.c_str());

}



int getintfromtrap(int argnum) {

  if ((*theargs)[argnum]->etype != numval) 

    throw(string("Backdoor couldn't extract number from arg ")

	  + itostr(argnum));

  numvalexp * nv = (numvalexp *)(*theargs)[argnum];

  int retval;

  if (!lookslikeint(nv->value,retval))

    throw(string("Backdoor couldn't extract integer from arg ")

	  + itostr(argnum));

  return(retval);

}



/************************************************************************

 * getargs gets the arguments from the backdoor fake equation. returns  *

 *   true if got them, false if not (and equation should be treated as  *

 *   real								*

 ************************************************************************/ 

bool getargs(expr * & e)

{

  if (e->etype == binop) {

    binopexp * bexp = (binopexp *) e;

    if (bexp->op->opty != topowe) {

      DBG(cout << "getargs found arg " <<  e->getInfix() << endl;);

      return(false);

    }

					

    if ((bexp->rhs->etype != numval) || 

	((numvalexp *)bexp->rhs)->value != theargs->size()+2) {

      DBG(cout << "getargs expected exponent of " << itostr(theargs->size()+2)

	  <<  " in expression " << e->getInfix() << endl;);

      return(false);

    }

    

    bexp->rhs->destroy();

    theargs->push_back(bexp->lhs);

    delete bexp;

    return(true);

  }

  if (e->etype == n_op) {

    n_opexp * nopexp = (n_opexp *) e;

    if (nopexp->op->opty != pluse) {

      DBG(cout << "getargs expected topow or plus, got " << e->getInfix()

	  << endl;);

      return(false);

    }

    

    for (int q = 0; q < nopexp->args->size(); q++)

      getargs((*nopexp->args)[q]);

    delete e;

    return(true);

  }

  DBG(cout << "getargs expected topow or plus, got " <<e->getInfix() << endl;);

  return(false);

}



bool backdoor(const char * const fakeeqn) {

  int whichfunct;

  int q;

  DBG(cout << "Entering backdoor with " << fakeeqn << endl;);

  if (strlen(fakeeqn) == 0) return(false);

  binopexp *theeqn = getAnEqn(fakeeqn,true);

  if (theeqn->lhs->etype != physvart) {theeqn->destroy(); return(false);}

  varindx var = ((physvarptr *)theeqn->lhs)->varindex;

  if (theeqn->rhs->etype != n_op) {theeqn->destroy(); return(false);}

  expr * therhs = theeqn->rhs;

  theeqn->lhs->destroy(); delete theeqn;

  theargs = new vector<expr *>;

  DBG(cout << "backdoor about to getargs" << endl;);

  if (!getargs(therhs)) return(false);

  DBG(cout << "backdoor: getargs returned " << theargs->size() 

           << " args" << endl;);

  string answer = "Backdoor: ";

  if (theargs->size() < 1) throw(string("backdoor: theargs too small"));

  if ((*theargs)[0]->etype != numval) goto retfalse;

  if (!(lookslikeint(((numvalexp *)(*theargs)[0])->value,whichfunct)))

    goto retfalse;

  

  switch (whichfunct) {

  case 1:			// indyIsStudEqnOkay

    {

      if (theargs->size() < 3) throw(string("backdoor: theargs too small"));

      answer.append("indyIsStudEqnOkay on ");

      string eqntest = "(= ";

      eqntest.append((((*theargs)[1])->getLisp(false)).c_str());

      eqntest.append(" ");

      eqntest.append( (((*theargs)[2])->getLisp(false)).c_str());

      eqntest.append(")");

      answer.append(eqntest + " returned ");

      answer.append(itostr(indyIsStudEqnOkay(eqntest.c_str())));

      break;

    }

  case 2:			// simplifyEqn

    if (theargs->size() < 3) throw(string("backdoor: theargs too small"));

    answer.append("simplifyEqn (");

    answer.append(itostr(getintfromtrap(1)));

    answer.append(", ");

    answer.append(itostr(getintfromtrap(2)));

    answer.append(") gives ");

    answer.append(simplifyEqn(getintfromtrap(1),getintfromtrap(2)));

    break;

  case 3:			// solveOneEqn

    if (theargs->size() < 4) throw(string("backdoor: theargs too small"));

    answer.append("solveOneEqn (");

    answer.append(getvarfromtrap(1));

    answer.append(", ");

    answer.append(itostr(getintfromtrap(2)));

    answer.append(", ");

    answer.append(itostr(getintfromtrap(3)));

    answer.append(") gives ");

    answer.append(solveOneEqn(getvarfromtrap(1),getintfromtrap(2),

			      getintfromtrap(3)));

    break;



  case 4:			// subInOneEqn

    if (theargs->size() < 4) throw(string("backdoor: theargs too small"));

    answer.append("subInOneEqn (");

    answer.append(itostr(getintfromtrap(1)));

    answer.append(", ");

    answer.append(itostr(getintfromtrap(2)));

    answer.append(", ");

    answer.append(itostr(getintfromtrap(3)));

    answer.append(") gives ");

    answer.append(subInOneEqn(getintfromtrap(1),getintfromtrap(2),

			      getintfromtrap(3)));

    break;

  default:

    throw(string("Joel tried to use backdoor with nonexistant type =")

	  + itostr(whichfunct));

  } // end of switch

  for (q = 0; q < theargs->size(); q++) (*theargs)[q]->destroy();

  delete theargs;

  DBG(cout << "backdoor about to throw " << answer << endl;);

  throw(answer);

  retfalse:

  for (q = 0; q < theargs->size(); q++) (*theargs)[q]->destroy();

  delete theargs;

  DBG(cout << "backdoor returning false from retfalse" << endl;);

  return(false);

}

