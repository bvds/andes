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
#include <windows.h> // Windows Header Files:
#endif // def WIN32
//////////////////////////////////////////////////////////////////////////////
typedef unsigned char byte; // assumes 8 bits
typedef unsigned short word; // assumes 16 bits
typedef unsigned int uint; // assumes 32 bits

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
