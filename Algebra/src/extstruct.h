//////////////////////////////////////////////////////////////////////////////
// extstruct.h -- global and external declarations of global structures 
// Copyright (C) 2001 by ????????????????????????????? -- All Rights Reserved.
// Author(s)
//	Joel A. Shapiro
//  Linwood H. Taylor (lht) <lht@lzri.com>
// Modified:
//	??	?????	- (jas) -- created
//	27 February 2001 - modified for maintenance ease and port to dll
//					environment
//////////////////////////////////////////////////////////////////////////////
#include "standard.h"

//////////////////////////////////////////////////////////////////////////////
#ifdef _WINDOWS
#pragma warning (disable: 4786)
#endif
#include <vector>
#include <string>
#include "expr.h"
//////////////////////////////////////////////////////////////////////////////
#define KILLMINUS true
#ifdef IAmMain
#define INITDO(A) A
#else
#define INITDO(A)
#endif
//////////////////////////////////////////////////////////////////////////////
const int STUDEQSZ = 40;	/* number of actual student equation slots */
const int HELPEQSZ = 70;	/* number of slots including implicit eqs */
// This is a bit larger than machine epsilon in case there is
// some accumulated roundoff error
#include <float.h>
#define RELERR (32*DBL_EPSILON)

//////////////////////////////////////////////////////////////////////////////
// globals defined here
// in the vectors below, indices of *:* match, and indices of &^& match
//////////////////////////////////////////////////////////////////////////////
// list of the canonical variables					&^&
LZ_EXTERN_SPEC std::vector<physvar*>* canonvars LZ_INIT_PTR_SPEC;
// list of the student defined variables				*:*
LZ_EXTERN_SPEC std::vector<string*> studvars; 
// list of expressions in canonical variables
// equal to the corresponding student var				*:*
LZ_EXTERN_SPEC std::vector<expr*> studvarvals;
// the canonical var corres. to the studvar				*:*
LZ_EXTERN_SPEC std::vector<physvar*> studvarcanon;
// list of the canonical equations
LZ_EXTERN_SPEC std::vector<binopexp*>*	canoneqf INITDO(= 0L);
// list of parameter artificial assignments
LZ_EXTERN_SPEC std::vector<binopexp*>*	paramasgn INITDO(= 0L);
// list of the student equations, by slot, 
// as function(canonical vars) = 0
LZ_EXTERN_SPEC std::vector<binopexp*> studeqf INITDO((HELPEQSZ,0L));
// list of the student equations, by slot, orig 
LZ_EXTERN_SPEC std::vector<string*> studeqsorig INITDO((HELPEQSZ,0L));
// solution eqn of the canonical vars (expr)	       			&^&
LZ_EXTERN_SPEC std::vector<binopexp*>* solsexpr LZ_INIT_PTR_SPEC;
// solution values of the canonical vars (dbls)				&^&
LZ_EXTERN_SPEC std::vector<double>* numsols LZ_INIT_PTR_SPEC; 
LZ_EXTERN_SPEC int numpasses LZ_INIT_INT_SPEC;

#ifdef UNITENABLE
LZ_EXTERN_SPEC std::vector<string> * constnames LZ_INIT_PTR_SPEC;
// names of named constants						&#&
LZ_EXTERN_SPEC std::vector<numvalexp*>* constnumvals LZ_INIT_PTR_SPEC;
// values of the named constants					&#&
#endif

//////////////////////////////////////////////////////////////////////////////
// end of file exstruct.h
// Copyright (C) 2001 by ????????????????????????????? -- All Rights Reserved.
//////////////////////////////////////////////////////////////////////////////
