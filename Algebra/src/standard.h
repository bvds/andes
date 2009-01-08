#ifndef _H_STANDARD_H_
#define _H_STANDARD_H_
//////////////////////////////////////////////////////////////////////////////
// standard.h -- standard stuff used in most applications
// Author(s): Linwood H. Taylor <lht@lzri.com>
// Modified:
//			31 January 2001 - lht -- created
//			19 February 2001 - lht -- modified for additional functionality
/// Modifications by Brett van de Sande, 2005-2008
// Copyright 2009 by Kurt Vanlehn and Brett van de Sande
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
/////////////////////////////////////////////////////////////////////////////

#if !defined(__CYGWIN__) && defined(_WINDOWS)
#include <windows.h> // Windows Header Files:
#endif

typedef unsigned char byte; // assumes 8 bit chars
typedef unsigned int uint; // assumes 32 bit ints
typedef unsigned short word; // assumes 16 bit shorts

#ifdef IAmMain
	#define LZ_EXTERN_SPEC
	#define LZ_INIT_PTR_SPEC = 0L
	#define LZ_INIT_INT_SPEC = 0
	#ifdef LZDBG
		fstream lzLog("lzLog.log", std::ios::out);
	#endif
#else // ifndef IAmMain
	#define LZ_EXTERN_SPEC extern
	#define LZ_INIT_PTR_SPEC
	#define LZ_INIT_INT_SPEC
	#ifdef LZDBG
		extern fstream lzLog;
	#endif
#endif // ndef IAmMain

//////////////////////////////////////////////////////////////////////////////
// End of file standard.h
// Copyright (C) 2001 by ????????????????????????????? -- All Rights Reserved.
//////////////////////////////////////////////////////////////////////////////
#endif // ndef _H_STANDARD_H_
