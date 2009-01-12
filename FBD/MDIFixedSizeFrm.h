#if !defined(AFX_MDIFIXEDSIZEFRAME_H__42BF6086_F1A2_11D2_807C_F206984FCFB0__INCLUDED_)
#define AFX_MDIFIXEDSIZEFRAME_H__42BF6086_F1A2_11D2_807C_F206984FCFB0__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// MDIFixedSizeFrame.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CMDIFixedSizeFrame frame
//
// MDI Child Frame class w/customized style to prevent resizing border.
// Suitable for containing fixed size form views.

class CMDIFixedSizeFrame : public CMDIChildWnd
{
	DECLARE_DYNCREATE(CMDIFixedSizeFrame)
protected:
	CMDIFixedSizeFrame();           // protected constructor used by dynamic creation

// Attributes
public:

// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CMDIFixedSizeFrame)
	protected:
	virtual BOOL PreCreateWindow(CREATESTRUCT& cs);
	//}}AFX_VIRTUAL

// Implementation
protected:
	virtual ~CMDIFixedSizeFrame();

	// Generated message map functions
	//{{AFX_MSG(CMDIFixedSizeFrame)
		// NOTE - the ClassWizard will add and remove member functions here.
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_MDIFIXEDSIZEFRAME_H__42BF6086_F1A2_11D2_807C_F206984FCFB0__INCLUDED_)
