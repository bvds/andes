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
#define WIN32_LEAN_AND_MEAN		// Exclude rare-used stuff from Windows headers
#ifdef __CYGWIN__
#else
#ifdef _WINDOWS
#include <windows.h> // Windows Header Files:
#endif
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

#define LZ_SAFE_DELETE(p) if (p) delete p; p = 0L

#ifdef LZDBG
#define Log(s) if (lzLog) lzLog << s;
#define Logn(s) if (lzLog) lzLog << s << std::endl;
#else // ifndef LZDBG
#define Log(s)
#define Logn(s)
#endif // ndef LZDBG

#define oLog(s)
#define oLogn(s)

//////////////////////////////////////////////////////////////////////////////
// End of file standard.h
// Copyright (C) 2001 by ????????????????????????????? -- All Rights Reserved.
//////////////////////////////////////////////////////////////////////////////
#endif // ndef _H_STANDARD_H_
