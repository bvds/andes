#ifndef _H_LRDCSTD_H_
#define _H_LRDCSTD_H_
//////////////////////////////////////////////////////////////////////////////
// lrdcstd.h -- standard stuff used in most applications
// Copyright (C) 2001 by ????????????????????????????? -- All Rights Reserved.
// Author(s): Linwood H. Taylor <lht@lzri.com>
// Modified:
//			31 January 2001 - lht -- created
//			19 February 2001 - lht -- modified for additional functionality
//			13 March 2001 - lht -- adding independance functionality
//////////////////////////////////////////////////////////////////////////////
#ifdef WIN32
  #ifndef _CONSOLE
    #define WIN32_LEAN_AND_MEAN		// Exclude rare-used stuff from Windows headers
    #include <windows.h> // Windows Header Files:
  #endif // ndef _CONSOLE
#endif // def WIN32
//////////////////////////////////////////////////////////////////////////////
typedef unsigned char byte; // assumes 8 bits
typedef unsigned short word; // assumes 16 bits
typedef unsigned int uint; // assumes 32 bits

//////////////////////////////////////////////////////////////////////////////
#ifdef LRDC_DBG
  #include <iostream>
	using namespace std;
	#define dLog(s) Log(s)
	#define dLogn(s) Logn(s)
  #define Log(s) std::cout << s
  #define Logn(s) std::cout << s << std::endl
#else // ifndef LRDC_DBG
	#define dLog(s) s
	#define dLogn(s) s
  #define Log(s)
  #define Logn(s)
#endif // ndef LRDC_DBG

#define odLog(s) s
#define odLogn(s) s
#define oLog(s)
#define oLogn(s)

//////////////////////////////////////////////////////////////////////////////
// support for global declaration and definition -- LRDC_MAIN should be
//	defined at the head of one and only one .cpp file (usually the one that
//	contains the 'main' function. This'll ensure that there is only one copy
//  of globals and all others references are actual externs
#ifdef LRDC_MAIN
  #define LRDC_EXTERN_SPEC
  #define LRDC_INIT_INT_SPEC = 0
  #define LRDC_INIT_PTR_SPEC = 0L
#else // ifndef LRDC_MAIN
  #define LRDC_EXTERN_SPEC extern
  #define LRDC_INIT_INT_SPEC
  #define LRDC_INIT_PTR_SPEC
#endif // ndef LRDC_MAIN

//////////////////////////////////////////////////////////////////////////////
// have to do this every time you delete something so might as well macro it
//	this does require making sure that an unallocated pointer is inited 0L
#define LRDC_SAFE_DELETE(p) if (p) { delete p; p = 0L; }

//////////////////////////////////////////////////////////////////////////////
// following pragma shuts off the warning that has the message/meaning:
//		(identifier was truncated to '255' characters in the debug information)
//////////////////////////////////////////////////////////////////////////////
#pragma warning (disable: 4786)

//////////////////////////////////////////////////////////////////////////////
// End of file lrdcstd.h
// Copyright (C) 2001 by ????????????????????????????? -- All Rights Reserved.
//////////////////////////////////////////////////////////////////////////////
#endif // ndef _H_LRDCSTD_H_
