// trigsimp.cpp		simplify the arg of a sin or cos of +-theta + numval
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

#include <math.h>
#include "decl.h"
#include "extoper.h"
#include "mconst.h"
#include "dbg.h"
using namespace std;

#define DBG(A) DBGF(SLVTRIG,A)

/************************************************************************
 * trigsimp should be called only on a functexp of type sin, cos or tan,*
 *	If the arg is of form +-(number + theta), the function is	* 
 *	rewritten to be of arg: theta + b, where 0<= b < 90		* 
 *   What is returned may be a mult, -1 * trigf(theta+b)		* 
 ************************************************************************/
void trigsimp(expr * & ex)
{
  DBG( cout << "Trigsimp called with " << ex->getInfix() << endl;);
  if (ex->etype != function) throw(string("trigsimp called on non-function"));
  functexp *exfun = (functexp *)ex;
  if ((exfun->f->opty != sine) && (exfun->f->opty != cose) &&
      (exfun->f->opty != tane)) 
    throw(string("trigsimp called on non-trig function"));
  flatten(exfun->arg);
  if (exfun->arg->etype != n_op) return;
  n_opexp *theta = (n_opexp *)exfun->arg;
  cleanup(theta);
  if (theta->args->size() < 2)
    { exfun->arg = theta; unnop(exfun->arg); return;}
  double sign = 1.;
  if ((theta->op->opty == pluse) && 		// if the arg is of form
      ((*theta->args)[0]->etype == numval))	// coef * var + phi( a num), 
    {							//  ...
      double phi = ((numvalexp *)(*theta->args)[0])->value;
      // make sure unknown (or first part of it) has positive coefficient
      if (((*theta->args)[1]->etype == n_op) &&
	  (((n_opexp *)((*theta->args)[1]))->op->opty == multe) &&
	  ((*((n_opexp *)((*theta->args)[1]))->args)[0]->etype == numval))
	{
	  numvalexp *coef = (numvalexp *)	 // first make coef nonnegative
	    (*((n_opexp *)((*theta->args)[1]))->args)[0];
	  if (coef->value < 0.) 
	    { 
	      coef->value *= -1.;
	      if ((exfun->f->opty == sine) || (exfun->f->opty == tane))
		sign *= -1.;
	      phi *= -1.;
	    }
	}
      // now arg is of form +unknown + phi. Now restrict range of phi
      phi -= 2*M_PI * floor(phi/(2*M_PI));	// angles defined mod 2 Pi
      if (phi >= M_PI)				// now restrict 0<=phi<Pi
	{					// by trig (ang + Pi) =
	  phi -= M_PI;				//   +- trig (ang)
	  if ((exfun->f->opty == sine) || (exfun->f->opty == cose))
	    sign *= -1.;
	}
      if (phi >= M_PI/2)		// now restrict 0 <= phi < Pi/2 using
	{				// sin (ang + 90) = cos (ang)
	  phi -= M_PI/2;			// cos (ang + 90) = -sin (ang)
	  if (exfun->f->opty == sine) exfun->f = &cosef; // but this 
	  else if (exfun->f->opty == cose) 	// doesn't work for tan,
	    {					// so leave phi unchanged 
	      exfun->f = & sinef;		//  for tan.
	      sign *= -1.;
	    }
	  if (exfun->f->opty == tane) phi += M_PI/2;
	}
      ((numvalexp *)(*theta->args)[0])->value = phi;
    } // end of if theta is plus starting with numval
  if ((theta->op->opty == multe) &&
      ((*theta->args)[0]->etype == numval) &&
      (((numvalexp *)((*theta->args)[0]))->value < 0))
    {
      ((numvalexp *)(*theta->args)[0])->value *= -1.;
      if ((exfun->f->opty == sine) || (exfun->f->opty == tane))
	sign *= -1.;
    }
  exfun->arg = theta;
  eqnumsimp(exfun->arg,true);
  if (sign == 1.) {
    ex = exfun; 
    DBG( cout << "Trigsimp returning " << ex->getInfix() << endl;);
    return; }
  n_opexp *exnop = new n_opexp(&mult);
  exnop->MKS.put(0,0,0,0,0); // REMOVE after fixing constructor
  numvalexp * tempnv = new numvalexp(-1);
  tempnv->MKS.put(0,0,0,0,0);
  exnop->addarg(tempnv);
  exnop->addarg(exfun);
  ex = exnop;
  DBG( cout << "Trigsimp returning " << ex->getInfix() << endl;);
  return;
}
