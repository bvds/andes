// dbg.h  defines for debug printing control
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved
#include "standard.h"

#ifdef WITHDBG
const int EQNUM   =	  0x04;	/* print entry expr in eqnumsimp */
const int FILEGET =       0x08;	/* getting the input file troubles in maple2 */
const int SOLVKV  =       0x10;	/* in solveknownvar */
const int LEAKCHK  =      0x10;	/* in ioprobwy */
const int READLOGS =      0x20;	/* in ioprobwy */
const int GETEQS  =       0x20; /* in getall, could be in geteqs if needed */

const int INTFL   =       0x40;	/* show eqs @ intermediate points in flatten */
const int ENTFLP  =       0x80;	/* announce entry to flatten */
			/* if MORE,  show equation on entry/exit to flatten */
const int NEWCKEQSOUT =  0x100;	/* check delete no-var eqs in newcheck */
const int CHKEQS =       0x200;	/* check delete no-var eqs in newcheck */
const int QSRT    =      0x400;	/* in qsrtexpr */
const int SETVAR  =      0x400;	/* in setvardimens */
const int SUBST   =      0x800;	/* in substin and also subexpin    */
const int FUFPLS  =     0x1000;	/* in fixupforpls */
const int INDY    =     0x1000;	/* in indyset */
const int INDYEMP =     0x2000;	/* in indysgg */
const int PLSAS   =     0x2000;	/* in purelin */
const int VALANDER =    0x2000;	/* in valander */
const int PLSHA   =     0x4000;	/* output high precision in purelin */
const int GETSOLS =     0x4000;	/* in getnewsols and getclipsols */
const int CHKSOL  =     0x8000;	/* in checksol */
const int DIMCHK  =     0x8000;	/* in dimenchk */
const int PLUSSORT =   0x10000;	/* in plussort */
const int NUMFACT =    0x20000;	/* in numfactorsof */
const int UNITS   =    0x20000;	/* in unitabr */
const int FACTOUT =    0x40000;	/* in factorout */
const int CLEANUP =    0x80000;	/* entry and exit from cleanup */
const int OUTSOL  =   0x100000;	/* in checksol, normal output offline */
const int NLSOLV  =   0x200000;	/* in nlsolv */
const int MAKEVARL =  0x200000;	/* in makevarl (just setvarbools) */
const int ORDUNK  =   0x400000;	/* in ordunknowns */
const int SLVTRIG =   0x800000;	/* in solvetrig */
const int NORMEX  =  0x1000000;	/* in normexpr */
const int MOREEX  =  0x2000000;	/* in moreexpr */
const int EQUALEQ =  0x4000000;	/* in equaleqs */
const int EXPRDB  =  0x8000000;	/* in expr.cpp */
const int ISPOS   = 0x10000000;	/* in ispos.cpp */
const int LINONEV = 0x20000000;	/* in powonev and slvlinonev */
const int POLY    = 0x40000000;	/* in polysolve */
const int RATEQ   =   0x800000;	/* in rationalize NOTE same as SLVTRIG!*/ 
const int MORE    =	  0x01;	/* in more detail */
const int DOEQCHK =       0x02; /* do checking of expr and vector<expr*> args*/


#define DBGF(FLAG,A) {if ((dbglevel & FLAG) != 0) {cout << __LINE__<<": "; A;}}
#define DBGFM(FLAG,A) {if (((dbglevel & FLAG) != 0) && MORE) \
	{cout << __LINE__<<": "; A;}}
#define EQCHK(A) {if (dbglevel & DOEQCHK) \
   { vector<int> *vchk = (vector<int>*) NULL; \
     if (!treechk(A,vchk)) throw(string("Treechk bombs on ") +A->getInfix());}}
#define VEQCHK(A) {if (dbglevel & DOEQCHK) \
	{ vector<int> *vchk = new vector<int>;\
	int kc; \
	for (kc = 0; kc < A->size();kc++) treechk((*A)[kc],vchk); \
	if (!listchk(vchk)) throw(string("Treechk bombs on " \
	     + (*A)[0]->getInfix() +"..." + (*A)[A->size()-1]->getInfix()));}}
#else
#define DBGF(FLAG,A)
#define DBGFM(FLAG,A)
#define EQCHK(A)
#define VEQCHK(A)
#endif

LZ_EXTERN_SPEC int dbglevel LZ_INIT_INT_SPEC;
LZ_EXTERN_SPEC int dbgnum LZ_INIT_INT_SPEC;
