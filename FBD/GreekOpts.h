// GreekOpts.h
//
// Implements common utility functions for rendering text which may contain 
// Greek symbols tagged by "$".
//
#ifndef GREEKOPTS_INCLUDED
#define GREEKOPTS_INCLUDED 1

class CGreekText
{
public:
	static CRect	DrawText(CDC* pDC, CRect rcText, CString str);
	
protected:
	enum TEXTTYPE		// type of a parsed part of the text:
	{ 
		IS_PLAINTEXT,
		IS_GREEKTEXT,
		// might add more, e.g. to handle bold and italics.
	};
	static CRect	DrawPart(CDC* pDC, CString str, CRect rcText, TEXTTYPE nType);
};

#endif GREEKOPTS_INCLUDED
