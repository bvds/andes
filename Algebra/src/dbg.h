// dbg.h  defines for debug printing control
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
#include "standard.h"

#ifdef WITHDBG
#define MORE            0x1	/* show more detail using DBGFM(...) */

// Start with the high-level stuff and work our way down:
#define SOLVEEQS        0x2	/* solve equations in canoneqf */
#define CHKEQS          0x4	/* check delete no-var eqs in newcheck */
#define NEWCKEQSOUT     0x8     /* check delete no-var eqs in newcheck */ 
#define QSRT           0x10	/* in qsrtexpr */
#define SETVAR         0x20	/* in setvardimens */
#define SUBST          0x40	/* in substin and also subexpin    */
#define DOEQCHK        0x80 /* do checking of expr and vector<expr*> args*/
#define EQNUM          0x80	/* print entry expr in eqnumsimp */
#define SOLVKV        0x100	/* in solveknownvar */
#define LEAKCHK       0x100	/* in ioprobwy */
#define READLOGS      0x200	/* in ioprobwy */
#define GETEQS        0x200 /* in getall, could be in geteqs if needed */
#define INTFL         0x400 /*show eqs @ intermediate points in flatten */
			/* if MORE,  show equation on entry/exit to flatten */
#define FUFPLS       0x1000	/* in fixupforpls */
#define INDY         0x1000	/* in indyset */
#define INDYEMP      0x2000	/* in indysgg */
#define PLSAS        0x2000	/* in purelin */
#define VALANDER     0x2000	/* in valander */
#define PLSHA        0x4000	/* output high precision in purelin */
#define GETSOLS      0x4000	/* in getnewsols and getclipsols */
#define CHKSOL       0x8000	/* in checksol */
#define DIMCHK       0x8000	/* in dimenchk */
#define PLUSSORT    0x10000	/* in plussort */
#define NUMFACT     0x20000	/* in numfactorsof */
#define UNITS       0x20000	/* in unitabr */
#define FACTOUT     0x40000	/* in factorout */
#define CLEANUP     0x80000	/* entry and exit from cleanup */
#define NLSOLV     0x100000	/* in nlsolv */
#define MAKEVARL   0x200000	/* in makevarl (just setvarbools) */
#define ORDUNK     0x400000	/* in ordunknowns */
#define SLVTRIG    0x800000	/* in solvetrig */
#define NORMEX    0x1000000	/* in normexpr */
#define MOREEX    0x2000000	/* in moreexpr */
#define EQUALEQ   0x4000000	/* in equaleqs */
#define EXPRDB    0x8000000	/* in expr.cpp */
#define ISPOS    0x10000000	/* in ispos.cpp */
#define LINONEV  0x20000000	/* in powonev and slvlinonev */
#define POLY     0x40000000	/* in polysolve */
#define RATEQ    0x80000000	/* in rationalize NOTE same as SLVTRIG!*/ 


#define DBGF(FLAG,A) {if (dbglevel & FLAG) \
        {cout << __FILE__ << ","<< __LINE__<<":  "; A;}}
#define DBGFM(FLAG,A) {if ((dbglevel & FLAG) && (dbglevel & MORE)) \
	{cout << __FILE__ << ","<< __LINE__<<":  "; A;}}
#define EQCHK(A) {if (dbglevel & DOEQCHK) \
   { vector<int> *vchk = (vector<int>*) NULL; \
     if (!treechk(A,vchk)) throw(string("Treechk bombs on ") +A->getInfix());}}
#define VEQCHK(A) {if (dbglevel & DOEQCHK) \
	{ vector<int> *vchk = new vector<int>;\
	unsigned int kc; \
	for (kc = 0; kc < A->size(); kc++) treechk((*A)[kc],vchk); \
	if (!listchk(vchk)) throw(string("Treechk bombs on " \
	     + (*A)[0]->getInfix() +"..." + (*A)[A->size()-1]->getInfix()));}}
#else
#define DBGF(FLAG,A)
#define DBGFM(FLAG,A)
#define EQCHK(A)
#define VEQCHK(A)
#endif

LZ_EXTERN_SPEC unsigned long dbglevel LZ_INIT_INT_SPEC;
LZ_EXTERN_SPEC unsigned long dbgnum LZ_INIT_INT_SPEC;
