#ifndef _H_STANDARD_H_
#define _H_STANDARD_H_
//////////////////////////////////////////////////////////////////////////////
// standard.h -- standard stuff used in most applications
// Copyright (C) 2001 by ????????????????????????????? -- All Rights Reserved.
// Author(s): Linwood H. Taylor <lht@lzri.com>
// Modified:
//			31 January 2001 - lht -- created
//			19 February 2001 - lht -- modified for additional functionality
//////////////////////////////////////////////////////////////////////////////

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
