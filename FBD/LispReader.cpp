// LispReader.cpp: implementation of the CLispReader class.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "fbd.h"
#include "LispReader.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CLispReader::CLispReader(FILE* fp)
:m_fp(fp)
{
}

CLispReader::~CLispReader()
{
	// !!! Need to hold on to allocated LispObjs and free them when done !!!	
}

// read next token
// returns character code for type of token read 
//  fills in strValue for atoms
//   EOF  end of file
//   (    Left-paren
//   )    Right paren
//    "   String
//    S   symbol
//    |   Vbar-delimited symbol
// 
int CLispReader::GetToken(CString& strResult)
{
    CString strToken;
	int nType;

   // skip white space. We don't handle comments
   int ch;
   while ((ch=getc(m_fp)) != EOF && isspace(ch))
	   ;
  
   switch (ch)
   {
   case EOF: 
	   nType = EOF;
	   break;

   case '(': 
	   strToken = '(';
	   nType = '(';
	   break;

   case ')': 
	    strToken = ")";
		nType = ')';
		break;

   case '"': // quote delimited string
	   // scan string until \"
	   while ((ch=getc(m_fp)) != EOF && ch != '"') 
	   {
		   if (ch == '\\')  // backslash escapes next char
		   {
			   ch = getc(m_fp);
			   if (ch == EOF) 
				   break; // out of while loop
		   }
		   strToken += ch;
	   }
	   nType = '\"';
	   break;
   
   case '|':  // vbar delimited symbol
	   // scan token until '|'
	   while (ch=getc(m_fp) != EOF && ch != '|') {
		   strToken += ch;
	   }
	   nType = '|';
	   break;
   
   // anything else: aggregate into symbol until break character
   default:
	   do {
			strToken += ch;
	   } while ( (ch=getc(m_fp))!=EOF && !isspace(ch) 
		          && ch != ')' && ch != '(' && ch!= '"');
	   // Push back terminating character so it will get
	   // processed. (needless but harmless for space or EOF).
	   ungetc(ch, m_fp);
	   
	   nType = 'A';   // for atomic symbol
	   break;
   }

   strResult = strToken;
   return nType;
}



CLispReader::Obj* CLispReader::GetObject()
{
   CString strToken;
   int nType = GetToken(strToken);

   // might have hit EOF:
   if (nType == EOF) 
	   return NULL;

   if (nType != '(') {
	   TRACE("GetObject: Atom %s\n", strToken);
	   return new Atom(strToken, nType); // need to hold onto it for deletion!
   }
    // else we've got an lparen:
    TRACE("GetObject: BEGIN LIST\n");
    List* pList = new List();	  // need to hold onto it for deletion!
	Obj* pObj;
	while  ((pObj = GetObject()) 
		    && ! (pObj->IsAtom() && ((Atom*)pObj)->m_nType == ')')){
		 pList->Append(pObj);
	}
	if (! pObj) { // hit EOF without list end
		// throw error?
	}
	TRACE("GetObject: END LIST\n");
	return pList;
}




