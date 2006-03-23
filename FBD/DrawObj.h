///////////////////////////////////////////////////////////////////////////
//
// CDrawObj: Base type of all diagram drawing objects:
//
// This class defines the protocol each drawing object must implement. The design
// is based on the MFC DRAWCLI sample. The base class
// provides a default implementation of most methods (but not drawing) 
// which is appropriate for an object contained within a rectangular position 
// box. Commonly used fields such as name and status are provided in the base class 
// as a convenience, even though not all derived classes will need them.
//
// Although this is really an abstract base class -- we do not expect to
// create instances of generic DrawObjs -- we cannot declare functions pure virtual 
// because it contains serializable member data, and MFC serialization support 
// requires the ability to construct an instance. (An MS KB article discusses this.)
//
// In the DRAWCLI model, a few functions take a view parameter, so this
// class has dependencies on the view class. These functions require the view
// to provide methods for coordinate transformations, and a notification callbacks
// fired on invalidation or deletion of objects. They also require the view
// to understand the update hints sent via the document (see FBDDoc.h for list).
///////////////////////////////////////////////////////////////////////////
class CEXInfo;		//	struct which contains Example explain info
class CFBDView;		// Type of view our graphics display into
class CFBDDoc;		// Type of containing document
class CVariable;
class CDrawObj;

#include <afxcoll.h>

typedef CTypedPtrList<CObList, CDrawObj*> CDrawObjList;
typedef CTypedPtrList<CObList, CCheckedObj*> CChkObjList;

class CDrawObj : public CObject
{
protected:
	DECLARE_SERIAL(CDrawObj);
  	CDrawObj();
    
// Constructors
public:
	CDrawObj (const CRect& position);
	CDrawObj (const CRect& position, const CString& label);
	void Serialize(CArchive& ar);
	
// Attributes
	CRect	 m_position;	// rect defining object position on logical page
	CFBDDoc* m_pDocument;	// back pointer to containing parent document
	CString  m_strId;		// generated unique id (for helpsys interactions)
	CString  m_strName;		// user's label, or other associated text

// Flag values for distinguishing object types:
#define TEACHER_OBJECT 	0	// created by author (assume part of the problem)
#define STUDENT_OBJECT	1	// created by student (part of the solution).
	WORD	 m_flag;		// whether object created by teacher or student

	// dependencies:  (used for vectors + axes only):
	CDrawObjList m_Angles;	// list of dependent angle objects, if any. 
	void UpdateAngles();
							
	// for checkable entries (really belongs in CCheckedObj subclass):
	Status	 m_status;		// status as checked by help system
	CStringList m_errors;	// list of specific errors (for dialog)

	// Associated self-explanation information, if any.
	CEXInfo* m_pEXInfo;		
	CEXInfo* GetEXInfo() {return m_pEXInfo;}

	// For checking candidate definitions against existing defs:
	virtual BOOL IsValid();
	virtual BOOL HasSameName(CDrawObj* pObj);
	virtual BOOL HasSameDef(CDrawObj* pObj);
	virtual BOOL HasSameDef(CVariable* pVar);
	virtual BOOL HasSameDir(CDrawObj* pObj);

// Operations
	// For drawing object
    virtual void Draw(CDC* pDC);
    virtual void DrawMasked(CDC* pDC);		// Draws in example mode
    enum TrackerState { normal, selected, active };
	virtual void DrawSelectState(CDC* pDC, TrackerState state);
    virtual void Invalidate();
    virtual COLORREF StatusColor();
    virtual CRect GetBoundingBox();
    
	// for selecting object 
    virtual int HitTest(CPoint point, CFBDView* pView, BOOL bSelected);
    virtual BOOL Intersects(const CRect& rect);
	// objects may retain info about which part was selected (e.g. pos, neg x axis):
	virtual int GetHit();
    virtual void SetSelectedPart(int hit);
	virtual int GetSelectedPart();

    // for managing resize handle grabs	
    virtual int GetHandleCount();
    virtual CPoint GetHandle(int nHandle);
    virtual CRect GetHandleRect(int nHandleID, CFBDView* pView);
    virtual HCURSOR GetHandleCursor(int nHandle);
    
	// for moving or resizing objects
    virtual void MoveTo(const CRect& position, CFBDView* pView = NULL);
    virtual void MoveHandleTo(int nHandle, CPoint point, CFBDView* pView = NULL);
    	
    // standard editing operations:
    virtual void Delete();
    virtual BOOL OnEditProperties();
    virtual BOOL CanEditProperties();
    virtual BOOL CanDuplicate();
    virtual CDrawObj* Clone();
    	
    // Common to directed objects
    virtual int GetDirection();				// calculate drawn direction.
	virtual int GetDirection(CPoint ptFrom, CPoint ptTo);
    void SetDirection(int degreesUser, CFBDView* pView = NULL); // update drawing dir

   	// Common to labelled objects. Returns FALSE if no label
   	virtual BOOL GetLabelRect(CRect& posLabel) { return FALSE; } // default is none
    
	// For self-explain interface: where to place explain button.
	virtual CPoint GetBtnPos(CRect btnPos);

  	// Utility used for diagnostics + indexing diagram objs by type name
   	virtual void GetTypeName(CString& strType);
    
  	// Fetching Displayable/Printable property definition strings
  	virtual CString GetDef();
  	virtual CString GetPrintDef();
	
	// Get standard label prefix (used by definition dialog)
	virtual CString GetLabelPrefix();

	// For testing for certain attributes:
	virtual BOOL IsComponent() { return FALSE; }
	virtual BOOL HasComponents() { return FALSE; }//common to certain vectors and variables
	virtual BOOL InVarList() { return FALSE; }//not in list unless checked obj

	// Copy properties from another object, used to apply from dialog defs.
	virtual void UpdateObj(CDrawObj* pObj);

	// For recording/recreating state to/from log line:
	virtual void LogEntry();
	virtual BOOL SetFromLogStr(LPCTSTR pszStr);
protected:
	// For use by derived classes: get base class info from object's log line
	CString GetDrawObjLogPart();
	LPCTSTR ParseDrawObjLogPart(LPCTSTR pszStr); // returns unparsed rest
public:

#ifdef _DEBUG
	/* virtual void AssertValid() const; */
   	virtual void Dump(CDumpContext& dc) const;
#endif
};

// Units used in our graphic objects: 
const int nLUsPerInch  = 96;	// number of logical units per logical inch  

//////////////////////////////////////////////////////////////////////////////////
//
// General Graphics objects.
//
// These objects are independent of our application and could be made part of a 
// general-purpose graphics library. 
//
// In Andes, they are used mainly by author to create the problem graphics. Some may 
// also be used by student in solving problem.
//
//////////////////////////////////////////////////////////////////////////////////
    
//
// CDrawRect -- graphic object defined by a bounding rect.
//
// Used for rectangles, rounded rects, ellipses, lines and arcs
// with line and fill graphics attributes.
//
// Overloaded for use to mark answer boxes in Andes, signalled by special
// prefix "Answer-" in the object id. !!! Should be changed to use special objs
//
class CDrawRect: public CDrawObj
{
protected:
   	DECLARE_SERIAL(CDrawRect);
   	CDrawRect();
public:
   	enum Shape { rectangle, roundRect, ellipse, line, arc, arc2, polygon };
   	CDrawRect(Shape shape, const CRect& position);
   	virtual void Serialize(CArchive& ar);
  
   	Shape m_nShape;			// type of graphic shape this is.
   
  	// graphic attributes for drawing 
   	BOOL m_bPen;			// set if logpen specified, else use default
   	LOGPEN m_logpen;
   	BOOL m_bBrush;			// set if logbrush specified, else use default
   	LOGBRUSH m_logbrush;
    
  	virtual void Draw(CDC* pDC);
   	virtual BOOL CanEditProperties() { return TRUE; }
   	virtual BOOL OnEditProperties();
   	virtual BOOL CanDuplicate();
   	virtual CDrawObj* Clone();
   	/*
   	virtual void SetLineColor(COLORREF color);
   	virtual void SetFillColor(COLORREF color);
   	*/
	virtual void GetTypeName(CString& strType) const;
   
   	// Special id prefix signals use for answer box
   	static const char c_szAnswerPrefix[];
   	BOOL IsAnswerBox() { return m_strId.Left(strlen(c_szAnswerPrefix)) == c_szAnswerPrefix; };
    
#ifdef _DEBUG
   	/* virtual void AssertValid() const; */
 	virtual void Dump(CDumpContext& dc) const;
#endif
};
    
//
// Polygons: These inherit from CDrawRect only because they have the same
// graphics attributes (brush and pen). Subtype flag changes to Bezier curves,
// which use same representation (array of points).
//
class CDrawPoly : public CDrawRect
{
protected:
   	DECLARE_SERIAL(CDrawPoly);
   	CDrawPoly();
public:
   	CDrawPoly(const CRect& position, BOOL bBezier = FALSE);
  
// Attributes
   	int m_nPoints;			// number of points in polygon
   	CPoint* m_points;		// dynamically allocated array of points
   	BOOL m_bBezier;			// set to draw as Bezier curve
   
// Operations
 	void AddPoint(const CPoint& point, CFBDView* pView = NULL);
   	BOOL RecalcBounds(CFBDView* pView = NULL);
  
// Implementation
public:
   	virtual ~CDrawPoly();
   	virtual void Serialize(CArchive& ar);
   	virtual void Draw(CDC* pDC);
   	virtual void MoveTo(const CRect& position, CFBDView* pView = NULL);
   	virtual int GetHandleCount();
   	virtual CPoint GetHandle(int nHandle);
   	virtual HCURSOR GetHandleCursor(int nHandle);
   	virtual void MoveHandleTo(int nHandle, CPoint point, CFBDView* pView = NULL);
   	virtual BOOL Intersects(const CRect& rect);
   	virtual CDrawObj* Clone();
	virtual void GetTypeName(CString& strType);
   
protected:
   	int m_nAllocPoints;				// size of allocated
   	void DrawCurve(CDC* pDC);		// helper to draw curves
};
    
//
// Groups: set of DrawObjs batched into a group
// 
class CGroup: public CDrawObj
{
protected:
   	DECLARE_SERIAL(CGroup);
public:
  	CGroup();					// groups are constructed empty; populate w/AddObj
   	virtual void Serialize(CArchive& ar);
    
// Attributes:
public:
   	CDrawObjList m_objects;		// list of contained objects
    	
// Operations:
   	void AddObj(CDrawObj* pObj);// use to add elements to group
   	virtual void Draw(CDC* pDC);
   	virtual void MoveHandleTo(int nHandle, CPoint point, CFBDView* pView);
   	virtual void MoveTo(const CRect& position, CFBDView* pView);
   	virtual BOOL Intersects(const CRect& rect);
   	virtual int HitTest(CPoint point, CFBDView* pView, BOOL bSelected);
   	virtual void Delete();
   	virtual BOOL CanDuplicate();
   	virtual CDrawObj* Clone();
#ifdef _DEBUG
   	/* virtual void AssertValid() const; */
   	virtual void Dump(CDumpContext& dc) const;
#endif
}; 
    
//
// CLabel -- Block of text.   
// Currently label objs are always sized to fit text. May be multi-line.
// 
// (Class name is misleading, since not just for labels. But this name is used in 
// persistent data, so can't easily be changed.)
//
class CLabel: public CDrawObj		
{
// constructors:
protected:
   	DECLARE_SERIAL(CLabel);
  	CLabel();
public:
	CLabel(const CPoint& ptTopLeft);
   	CLabel(const CPoint& ptTopLeft, LPCSTR pszText, LOGFONT* pLogFont = NULL);
   	virtual void Serialize(CArchive& ar);
 
// Attributes:
   	// Flags for Andes Example mode, see below.
   	BOOL m_bMask;				// this object used as custom mask to hide others					
   	BOOL m_bVisible;			// leave text visible even in example mode
   	BOOL m_bHint;

// operations:
   	virtual void Draw(CDC* pDC);
   	virtual void DrawSelectState(CDC* pDC, TrackerState state);
   	virtual BOOL CanEditProperties() { return TRUE; }
   	virtual BOOL OnEditProperties();
   	virtual void  DrawMasked(CDC* pDC);
#ifdef _DEBUG
   	/* virtual void AssertValid() const; */
   	virtual void Dump(CDumpContext& dc) const;
#endif

// implementation
	CFont m_font;			// cached GDI font to draw this object
	void RecalcExtent();
	void SerializeFontInfo(CArchive& ar);
};
    
// 
// For historical reasons, the text object was overloaded in Andes to support
// example study mode. Flags in effect define three subtypes of CLabel in that mode:
//
// 1. Visible text (m_bVisible = TRUE) -- drawn normally, visible if
//        not hidden behind something else. Used to label example parts, eqns
//
// 2. Example text  (m_bVisible = FALSE) -- Example text item, drawn masked
//        unless user is selecting. Now obsolete.
//
// 3. Custom masks (m_bMask = TRUE) -- used only to cover a masked region
//        of the problem, for example, the free body diagram. Makes use
//        of the m_bVisible field at run-time to record whether up or
//        down. Note a visible mask hides the region beneath it, an
//        invisible mask does nothing.
//                                         
// Should have just created different subclasses for these types.
// In particular there is no good reason masks should be represented by 
// subtypes of text objects, It was done that way simply so that all special 
// treatment for example mode could be confined to the text object class.
//
// Added later: we now have broken out example text items into a subclass CEXText
// which carries further data to support for the self-explanation interface. We
// no longer use subtype 2; rather CLabels are used for always-visible background text, 
// while CEXText is reserved for coverable text. This will be enforced by the
// authoring dialogs when creating new examples. However, still need to deal
// with old files which have example text in CLabels w/m_bVisible = FALSE.
   

//
// CDrawPicture -- Embedded image imported from external file.
//
// This is implemented via the Picture object supported by the OLE libraries, which encapsulates either
// a bitmap, metafile, or icon image behind the IPicture COM interface. Most importantly for us, the OLE 
// library code knows how to load image data from .gif and .jpeg format files. 
// Note an OLE picture object is NOT a full embedded OLE item. It is just a COM object providing an IPicture
// (and IPersistStream) interface only.
//
class CDrawPicture : public CDrawObj
{
// Constructors:
protected:
	CDrawPicture();					// used by serialization only.
  	DECLARE_SERIAL(CDrawPicture);
public:
	// Class method to create new instance from file:
	static CDrawPicture* CreateFromFile(LPCTSTR pszPathName);
	virtual void Serialize(CArchive& ar);

// Attributes:
	// Can get width/height in doc units from m_position.

// Operations
public:
   	virtual void Draw(CDC* pDC);
	virtual void DrawSelectState(CDC* pDC, TrackerState state);
 
// Implementation
   	virtual ~CDrawPicture();
	IPicture* m_pPicture;		// IPicture interface on the OLE Picture object
protected:
	HGLOBAL m_hFileData;		// original file data in global memory block
	int     m_cbFileSize;		// size of file data in bytes.
};
 

//
// OLE Embedded objects. 
//
// COleDrawItem is our COleClientItem derivative provided to interface with the 
// MFC OLE container support. It is our handle on the embedded OLE object. We override 
// its virtual functions to respond to OLE notifications from the embedded
// server that are passed to us by the MFC framework during in-place activation
//
// COleDrawObj is an "Adaptor" class that wraps an OLE item inside a DrawObj interface. 
// The two objects each contain pointers to each other, and should generally function as
// a single unit. They can't be combined without multiple inheritance, however,
// which is inconvenient at best within the MFC framework.
//
// In Andes, OLE objects are used mainly to import graphics from other programs. Currently
// we only allow in-place activation in author mode, although that would have to change
// if we wanted students to view videos, say. There might be other uses for them
// down the road, e.g. to write our own servers to run embedded in the workbench.
// 
class COleDrawItem;    // Our COleClientItem derived class to represent an OLE item
    
class COleDrawObj : public CDrawObj
{
protected:
  	DECLARE_SERIAL(COleDrawObj);
 	COleDrawObj();
    
public:
  	COleDrawObj(const CRect& position);
    
// Implementation
public:
   	virtual void Serialize(CArchive& ar);
   	virtual void Draw(CDC* pDC);
  	virtual CDrawObj* Clone(CFBDDoc* pDoc);
   	virtual void OnOpen(CFBDView* pView);
   	virtual void MoveTo(const CRect& positon, CFBDView* pView = NULL);

   	virtual BOOL CanEditProperties() { return TRUE; }
   	virtual BOOL OnEditProperties();
   	virtual void Delete();
   	virtual ~COleDrawObj();
   
   	static BOOL c_bShowItems;	// class-wide flag: whether items should be shown
   
   	COleDrawItem* m_pClientItem;	// the contained client item
   	CSize m_extent; // "natural" extent of item from server in OLE's HIMETRIC unit
};
    
class COleDrawItem : public COleClientItem
{
   	DECLARE_SERIAL(COleDrawItem)
   
// Constructors
public:
   	COleDrawItem(CFBDDoc* pContainer = NULL, COleDrawObj* pDrawObj = NULL);
	// Note: pContainer is allowed to be NULL to enable IMPLEMENT_SERIALIZE
	//  IMPLEMENT_SERIALIZE requires the class have a constructor with
	//  zero arguments.  Normally, OLE items are constructed with a
	//  non-NULL document pointer.
    
// Attributes
public:
   	CFBDDoc* GetDocument()
   		{ return (CFBDDoc*)COleClientItem::GetDocument(); }
   	CFBDView* GetActiveView()
		{ return (CFBDView*)COleClientItem::GetActiveView(); }
  
   	COleDrawObj* m_pDrawObj;    // back pointer to OLE draw object
   
// Operations
   	BOOL UpdateFromServerExtent();
    
// Implementation
public:
   	~COleDrawItem();
#ifdef _DEBUG
   	virtual void AssertValid() const;
   	virtual void Dump(CDumpContext& dc) const;
#endif
   	virtual void Serialize(CArchive& ar);
   	virtual void OnGetItemPosition(CRect& rPosition);
    
protected:
   	virtual void OnChange(OLE_NOTIFICATION wNotification, DWORD dwParam);
   	virtual BOOL OnChangeItemPosition(const CRect& rectPos);
   	virtual void OnDeactivateUI(BOOL bUndoable);
   	virtual void OnActivate();
};
    
    
//////////////////////////////////////////////////////////////////////////////////
    
// 
// CCheckedObj: Common base class for student entries linked to Help System.
//
// Manages protocol for checking objects and interpreting results. 
//
// No member data here since we already put status field into generic base (though it
// really belongs here). Adding member data would complicate serializing older files, 
// since this new class was inserted into the middle of the object hierarchy. Note we
// still need to provide a standard DrawObj-type constructor so derived classes can 
// use C++ initializer list syntax to pass args to DrawObj constructor.
// 
// It might be worthwhile to make this a Mixin class, rather than a CDrawObj derivative,
// so could use it elsewhere, e.g. for equations. MFC is based on single-inheritance,
// but it allows the use of multiple inheritence for mixins not derived from CObject. 
// Its run-time type info won't recognize the presence of the mixin, however. See TN.
// 

class CCheckedObj : public CDrawObj
{
   	DECLARE_DYNAMIC(CCheckedObj);
   	CCheckedObj();							// needed because we have a non-default ctor
public:

//operations
   	CCheckedObj(const CRect& position);		// needed to pass init parm up to CDrawObj constructor
   	virtual void CheckObject() {}			// Check current object status; no-op in base
   	virtual void NotifyDelete();			// Notify help system object has been deleted
   	virtual void NotifyDelete(CString& strName); // notify delete via old name
   	virtual void NotifyChange(CString& strOldName);	// Update help system on some change.
   
	virtual CDialog* GetPropertyDlg()			// return initialized type-specific dialog
   			{ return NULL; }				// no default dialog in base
	virtual BOOL InVarList()			
			{ return TRUE; }//all checked obj's in list unless overrided
   
	virtual BOOL OnEditProperties();		// Edit props and notify help system of changes
   	
	virtual void UpdateVarNames(CString strOldName = "");
	virtual void RemoveVarName(CString strName);
	virtual void RemoveVarNames(CString strName);
	// Following helpers are static so can be used by other objs for common tasks:
   	// Split help system result string at optional ! into return value and command
   	static void SplitResult(LPCTSTR pszResult, CString& strReturn, CString& strCmd);
 	// Split result, set status variable from T or NIL, and execute attached command.
   	static void ApplyStatus(LPCTSTR pszResult, Status& rStatus, CStringList& strList);
	// parse possible list of dialog box errors
	static void ParseResult(CString& strResult, CStringList& strErrs);
   
	// Following is 1-arg method for drawobjs
	virtual void ApplyStatus(LPCTSTR pszResult); 

};
    
    
//////////////////////////////////////////////////////////////////////////////////////
//
// Diagram objects mainly used for qualitative problem solving
//
//////////////////////////////////////////////////////////////////////////////////////
    
//
// Choices: Student-checkable items for use in multiple choice questions.
// 
// Each choice is represented by a single CChoice object. This object
// is only used to hold the data representing the item in the problem and its value.
// It does not draw itself in student mode. When run in student mode, each view 
// is responsible for creating an interface to allow the student to check or
// uncheck a choice, presumably by creating appropriate child window controls.
// 
// Currently, the interface interprets a CGroup object that contains 
// CChoices among its top-level elements as defining a group of mutually 
// exclusive items. Ungrouped choice items are treated as non-exclusive.
// (!!! Probably should create CChoiceGroup class, since help system likely wants 
// to associate answer index values with questions)..
//
class CChoice: public CDrawObj	
{
protected:
   	DECLARE_SERIAL(CChoice);
   	CChoice();
public:
   	CChoice(const CRect& position);
   	virtual void Serialize(CArchive& ar);
   	virtual void Draw(CDC* pDC);
   	virtual void DrawSelectState(CDC* pDC, TrackerState state);
   	virtual BOOL CanEditProperties(); 
   	virtual BOOL OnEditProperties();
   
   	BOOL	m_bCorrect;		// this choice is correct
   	BOOL	m_bChosen;		// this choice is checked
	// back pointer to cointaining group?
};
  
   
//
// Document Areas -- marked out distinguished rectangular regions of the page
//
// Used by authors to delimit areas of the problem page for particular purposes, esp. 
// Motion Diagrams. Also intended be used to define masked regions for example mode,
// although that is not yet implemented or used.
// The type of area is coded as a string in the m_strName member. Flag specifies
// whether the box border is drawn (as for motion diagram boxes) or not.
// 
// Currently CDocAreas are just used for their rectangle and label so the Workbench can 
// identify certain areas when we need to, like defining a hot-spot. Nothing prevents 
// the user from drawing over the boundary, Therefore they function like boxes for 
// separate parts of a problem drawn on a handout page.
// 
// Eventually we probably must change our diagram representation from a flat list of 
// objects to a hierarchical, tree-structured one, by supporting a general notion of
// object containment. Then we could allow sub-diagrams as nested  objects. In that case, 
// the DocArea objects could be extended to function as true graphic object containers, 
//
   class CDocArea: public CDrawObj
{
protected:
   	DECLARE_SERIAL(CDocArea);
   	CDocArea();
   
public:
   	CDocArea(const CRect& position);
   	~CDocArea();
// attributes
   	BOOL m_bBorder;				// True => draw border around area.
// String constants used for type names;
  	static const char* c_szMotion;
//Implementation
public:
   	virtual void Serialize(CArchive& ar);
   	virtual void Draw(CDC* pDC);
   	virtual BOOL CanEditProperties(); 
 	virtual BOOL OnEditProperties();
};

//////////////////////////////////////////////////////
// CHyperLnk: 
///////////////////////////////////////////////////////
#define ID_JUMP		0
#define ID_POPUP	1
#define ID_HELPCALL 2

class CHyperLnk: public CDrawObj
{//constructors
protected:
   	DECLARE_SERIAL(CHyperLnk);
   	CHyperLnk();
public:
   	CHyperLnk(const CRect& position, LPCSTR pszText);
   	virtual void Serialize(CArchive& ar);
protected:
	void SerializeFontInfo(CArchive& ar);
    
// Attributes:
   /* Changed to use CDrawObj ID field
   	CString m_strId;					// Id for help system
*/
public:
	CString m_strLink;
	CFont m_font;
	int m_nType;
	LONG m_posBeg;
	LONG m_posEnd;

	void SetLinkType(char tag);

//operations
	virtual void HyperActivate(CPoint point, CView* pView);
	virtual void  Draw(CDC* pDC);
   	virtual BOOL OnEditProperties();
	virtual int HitTest(CPoint point, CFBDView* pView, BOOL bSelected);

//implementation
#ifdef _DEBUG
   	/* virtual void AssertValid() const; */
  	virtual void Dump(CDumpContext& dc) const;
#endif
};


////////////////////////////////////////////////////////////////////////////////////
// Example study objects
////////////////////////////////////////////////////////////////////////////////////
// 
// CEXText: Example text subclass of CLabel for example study item. Has additional 
// fields for explain menu 
//
class CEXText: public CLabel
{
protected:
   	DECLARE_SERIAL(CEXText);
   	CEXText();
public:
	virtual CPoint GetBtnPos(CRect btnPos);
   	CEXText(const CRect& position, LPCSTR pszText);
   	virtual void Serialize(CArchive& ar);
    virtual void GetTypeName(CString & strType);

    // Attributes:
   	CString m_strMenu;					// Explain menu name
   /* Changed to use CDrawObj ID field
   	CString m_strId;					// Id for help system
*/
	
   	virtual void  Draw(CDC* pDC);
   	virtual BOOL  OnEditProperties();
#ifdef _DEBUG
   	/* virtual void AssertValid() const; */
  	virtual void Dump(CDumpContext& dc) const;
#endif
};
    
/////////////////////////////////////////////////////////////////////////////
// Informational data class
typedef CList<int, int> CIntList;
    
class CEXInfo : public CObject// Example information structure
{
public:    
   	CEXInfo();
	DECLARE_DYNCREATE(CEXInfo);

   	CString m_planItemPos;//selected plan item's position
	CDrawObjList m_relations;//list of related items.
   	
	int m_rule;
   	int m_rule1;
   	CString m_strRule;
   	CString m_strRule1;
   	int m_CrrctIntLst[4];//template selection list
   	int m_CrrctIntLst1[4];//template selection list
  
	BOOL m_bExplained;//whether or not the item has been explained
	CIntList m_checkList;//when Cristina sends a hint, this is 
    						//the list of menu items the user must 
    						//complete
    #ifdef _DEBUG
public:
	/* virtual void AssertValid() const; */
   	virtual void Dump(CDumpContext& dc) const;
#endif
};

////////////////////////////////////////////////////////////////////////////////
//
// Variables: student defined labels for physical quantities
//
// These aren't really used as drawn objects at all (though they could be). 
// we put them under CCheckedObj (hence under DrawObj) in the class hierarchy 
// as a kind of easy hack because they share  so much functionality with the DrawObjs. 
// For example, a vector variable must be comparable to a drawn vector and uses the 
// same dialog for definitions. The common functionality is mainly what must be exposed
// to the drawobjdlg's for supporting its property editing protocol.
//
// Currently we just have one fat type-tagged object with enough slots to hold 
// the attributes of any definable object. The slots may be differently used
// for different quantities, e.g. m_strAgent may hold a second body in some cases,
// m_strForceType may not hold a force type in some cases, etc.
//
// !!! The Variable implementation is not object oriented, is a mass of 
// type-dependent switches, a pain to extend and should be redone.
//
class CVariable : public CCheckedObj
{
//constructors
protected:
	DECLARE_SERIAL(CVariable);
	CVariable();

public:
	void Serialize(CArchive& ar);

//attributes
public:
	int m_nType;				// Quantity type code (ID_VARIABLE_ADD*)

	CString m_strObject;		// principal body
	CString m_strTime;			// principal time
	CString m_strForceType;		// force type name or other subtype 
	CString m_strAgent;			// force agent, or second body or time
	
	// derived from above:
	CString m_strQuantName;		// quantity type name as string
	CString m_strDef;			// full definition string

	CString m_strValue;			// Given value expression, empty if unknown

	
	// For vector variables: equivalent of m_bAngular flag to denote angular form is
	// coded by type in m_nType. Angular type ids and vector type ids in parallel ranges. 
	BOOL IsAngularVector()
		{ return (m_nType >= ID_VARIABLE_ANGVECTOR_FIRST
			      && m_nType <= ID_VARIABLE_ANGVECTOR_LAST); };
	BOOL IsLinearVector()		// NB: doesn't include force, only counterparts of angvecs
		{ return (m_nType >= ID_VARIABLE_LINVECTOR_FIRST
			      && m_nType <= ID_VARIABLE_LINVECTOR_LAST); };
	void SetAngular(BOOL bAngular) 
		{ 
		  if (bAngular && IsLinearVector())
			m_nType = ID_VARIABLE_ANGVECTOR_FIRST + (m_nType-ID_VARIABLE_LINVECTOR_FIRST); 
		  else if (! bAngular && IsAngularVector())
			m_nType = ID_VARIABLE_LINVECTOR_FIRST + (m_nType-ID_VARIABLE_ANGVECTOR_FIRST);
		};		
//operations	
public:
	CString GetCheckCmd();
	virtual CDialog*	GetPropertyDlg();
	virtual void		CheckObject();
	virtual CString		GetDef();
	virtual CString		GetLabelPrefix();
	virtual CDrawObj*	Clone();

	virtual BOOL HasSameDef(CDrawObj* pObj);
	virtual BOOL HasSameDef(CVariable* pVar);

	virtual BOOL IsValid();//checking for uniqueness, valid label, etc.
	virtual void UpdateObj(CDrawObj* pObj);//called from dialog to update object props

	virtual BOOL	HasComponents();//is a vector variable??
	virtual void	UpdateVarNames(CString strOldName = "");
	virtual void	RemoveVarNames(CString strOldName);

	virtual BOOL SetFromLogStr(LPCTSTR pszStr);
	virtual void LogEntry();

// Class data:

	// WORK: special agent string used in case of net work:
	static const char* c_szAllForces;
	static const char* c_szNCForces;
};
    
// helper for graphics calculations
extern int Round(double f);

