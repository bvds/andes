//GreekOpts.cpp
#include "stdafx.h"
#include "fbd.h"
#include "GreekOpts.h"

// Render mixed Greek and default font text into given rect. 
// RETURNS: rect actually occupied by drawn text.
//
// Greek alphabet characters tagged with $, e.g $a for alpha.
//
// NB: Uses DrawText to do work. Breaks words with DT_WORDBREAK flag.
// Took out original NOCLIP flag so can break in middle of word if it 
// it doesn't fit.
// !!!No way to escape a dollar sign to actually use it in the text.
CRect CGreekText::DrawText(CDC* pDC, CRect rcText, CString str)
{
	CRect rcDrawn;
	if (!str.IsEmpty() && str.Find('$') != -1)
	{
		// Parse out the $ and draw the greek letter following
		while (str.Find('$') != -1)
		{
			int nFmtCharPos	= str.Find('$');
			CString strNoGrk = str.Left(nFmtCharPos);
			if (!strNoGrk.IsEmpty())
			{
				rcDrawn = DrawPart(pDC, strNoGrk, rcText, IS_PLAINTEXT);
				// Adjust rcText to hold location of next sub-part. 
				rcText.left = rcDrawn.right;
				// !!! Drawing of a long piece could have wrapped around 
				// right edge of rcText onto subsequent line(s). We would
				// have to adjust formatting rect position to end of this line
				// to be at pos to append Greek letter. Hard to determine where 
				// this is if DrawText rect result has width of wider line above
				// shorter overflow line - might have to break lines ourselves to
				// measure overflow piece. Note also have to compute total rcText of
				// all pieces to return (mainly so caller can get height used)
	/* This not enough to solve problem:	
				if (rcDrawn.Height() > rcText.Height()) {
					// assume original rcText height is expected line height (not 
					// height of some larger box to draw into.)
					CSize sizeOverflow(0, rcDrawn.Height() - rcText.Height());
					rcText += sizeOverflow; // adjusts height, leaves width
				} 
	*/
			}

			str = str.Mid(nFmtCharPos + 1);
			CString strGrk = str.Left(1);
			if (!strGrk.IsEmpty())
			{
				rcDrawn = DrawPart(pDC, strGrk, rcText, IS_GREEKTEXT);
				rcText.left = rcDrawn.right;
			}

			str = str.Mid(1);
		}
	}
	if (!str.IsEmpty())
		rcText = DrawPart(pDC, str, rcText, IS_PLAINTEXT);

	return rcText;
}

// Worker routine draws text piece in a single font. Returns rect used.
CRect CGreekText::DrawPart(CDC* pDC, CString str, CRect rcText, TEXTTYPE nType)
{
	CFont fntGreek;				// temp for drawing this piece only (if Greek).
	CFont* pOldFont = NULL;

	if (nType == IS_GREEKTEXT)
	{
		// Create the symbol variant of current DC font
		CFont* pFont = pDC->GetCurrentFont();
		TEXTMETRIC tm;
        pDC->GetTextMetrics(&tm);

		if (pFont)
		{
			LOGFONT lf;
			pFont->GetLogFont(&lf);
			lf.lfHeight = tm.tmHeight;
			lf.lfWeight = tm.tmWeight;
			strcpy(lf.lfFaceName, "Symbol");        
			VERIFY(fntGreek.CreateFontIndirect(&lf));  // create the font

			pOldFont = pDC->SelectObject(&fntGreek);
		}
	}

	ASSERT(!str.IsEmpty());

	// First call calcs bounding rect, second does the drawing.
	pDC->DrawText(str, -1, rcText, DT_CALCRECT|DT_WORDBREAK  );
	pDC->DrawText(str, -1, rcText, /*DT_NOPREFIX|*/DT_WORDBREAK   /*|DT_NOCLIP*/);

	if (pOldFont != NULL)
		pDC->SelectObject(pOldFont);

	return rcText;
}

