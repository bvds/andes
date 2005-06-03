// dbg.h  defines for debug printing control
// Copyright (C) 2001 by Joel A. Shapiro -- All Rights Reserved
#include "standard.h"

#ifdef WITHDBG
// Start with the high-level stuff and work our way down:
#define SOLVEEQS        0x1	/* solve equations in canoneqf */
#define CHKEQS          0x2	/* check delete no-var eqs in newcheck */
#define NEWCKEQSOUT     0x4     /*check delete no-var eqs in newcheck */ 
#define QSRT            0x8	/* in qsrtexpr */
#define SETVAR         0x10	/* in setvardimens */
#define SUBST          0x20	/* in substin and also subexpin    */
#define MORE           0x20	/* in more detail */
#define DOEQCHK        0x40 /* do checking of expr and vector<expr*> args*/
#define EQNUM          0x80	/* print entry expr in eqnumsimp */
#define SOLVKV        0x100	/* in solveknownvar */
#define LEAKCHK       0x100	/* in ioprobwy */
#define READLOGS      0x200	/* in ioprobwy */
#define GETEQS        0x200 /* in getall, could be in geteqs if needed */
#define INTFL         0x400 /*show eqs @ intermediate points in flatten */
#define ENTFLP        0x800	/* announce entry to flatten */
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
#define OUTSOL     0x100000	/* in checksol, normal output offline */
#define NLSOLV     0x200000	/* in nlsolv */
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
#define RATEQ      0x800000	/* in rationalize NOTE same as SLVTRIG!*/ 


#define DBGF(FLAG,A) {if ((dbglevel & FLAG) != 0) \
        {cout << __FILE__ << ","<< __LINE__<<":  "; A;}}
#define DBGFM(FLAG,A) {if (((dbglevel & FLAG) != 0) && MORE) \
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
