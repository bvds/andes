// VarView.cpp : implementation file
//

#include "stdafx.h"
#include "fbd.h"
#include "FBDDoc.h"
#include "FBDObj.h"
#include "history.h"
#include "helpifc.h"
#include "Mainfrm.h"
#include "TabView.h"
#include "EQView.h"
#include "MyMenu.h"
#include "VarView.h"
#include "AngleDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif


/////////////////////////////////////////////////////////////////////////////
// CVarView
 
// m_quants: table of information on definable quantities.
//
// *EVERY* definable variable quantity type, whether vector or scalar,
// must have an entry in the quantity table m_quants. The m_quants table is built dynamically, 
// getting initial entries from the static table of  compiled-in quantity info below (varIDTbl), 
// and loading further entries from a tab-delimited external file to allow for extensibility.
// 
// Entry needed for all quants to map integer VARIABLE_ADD* command IDs to string ids used in logging type args
// to variable-related commands and entries. Call these the "quant-type-id strings". quant-type-id 
// strings should not contain spaces for ease of parsing these arguments in log file lines. 
// The quant-type-id strings are now also sent to the helpsystem to identify the quantity in API calls, so
// should match the helpsys quantity id. (In past, a few of them had to be converted before sending.)
//
// For quantities using the generic variable dialog, entries must also include the "quant-type names" which 
// are stored in the "Value" slot of the var object and used in forming the quantity definition strings. 
// Because definition strings are shown in the varview, quant-type names should be friendly and may 
// contain spaces. quant-type-id strings are for internal use in logging and communication w/helpsys.
// Vars using special dialogs, like energy,radius, angle, set the quant-type names in their own dialogs.
// (Though plan to change them to get that info from this table as well.) 
//

// Note struct for dynamically loaded quantities uses CStrings to simplify issues about copying strings.
typedef struct
{	int	id;					// integer quantity code, maybe generated at runtime
	CString strTypeId;		// quantity typeid w/o spaces. Used in logging cmd and entry and helpsys API
	CString strPrefix;		// variable name prefix
	// following optional, only used if needed to build a dialog box:
	CString strValue;		// human-readable quantity type name -- may contain spaces.
	CString strDlgValue;    // quantity type prefix to show in dialog, if different from strValue.
	CString strSpec;		// dialog field spec
} QuantInfo;
	
typedef CTypedPtrArray<CPtrArray, QuantInfo*> CQuantpArray;
static CQuantpArray m_quants;

// for scalars using compiled-in dialog, dialog-locating code relies on static, compiled-in integer ids.
// Now ALL scalar quants are listed in the external file (mainly so we can control order of all quantities)
// this map is used to find fixed compiled-in ids where needed when loading scalar quant info from external file,
// instead of assigning a dynamically-generated id for these.
static struct { 
	int nId;	
	char* szTypeId;
} staticIds[] =  {
//{ ID_VARIABLE_ADDRADIUS,        "radius" },             // old WB quant id for this.
//{ ID_VARIABLE_ADDRADIUS,        "revolution-radius" },  // new quant id 
{ ID_VARIABLE_ADDSPEED,			"speed"  },
{ ID_VARIABLE_ADDANGLE,			"angle"  },
{ ID_VARIABLE_ADDNRG,			"energy" }, 
{ ID_VARIABLE_ADDCURRENT,		"current" },
{ ID_VARIABLE_ADDVOLTAGE,		"voltage" },
{ ID_VARIABLE_ADDRESISTANCE,	"resistance" },
{ ID_VARIABLE_ADDCAPACITANCE,	"capacitance"	},
{ ID_VARIABLE_ADDTIME,          "duration"},
{ ID_VARIABLE_ADDPROBABILITY,   "probability"}, 
{ ID_VARIABLE_ADDTIMECONSTANT,   "time-constant"},
};
const int numStaticIds ARRAY_SIZE(staticIds);

static int LookupStaticId(CString& strTypeId)
{
	for (int i = 0; i < numStaticIds; i++) 
		if (strTypeId.CompareNoCase(staticIds[i].szTypeId) == 0)
			return staticIds[i].nId;

	return -1;
}

// AddQuant: add a quantity record to the list of quantities
// nID of -1 means generate a new dynamic id for it.
// compiled-in static quantity id codes are in the range ID_VARIABLE_ADDFIRST to ID_VARIABLE_MAX_STATIC
// dynamic ids are assigned from the range ID_VARIABLE_MAX_STATIC+1  to ID_VARIABLE_ADDLAST
// VarView dispatches all commands in range ID_VARIABLE_ADDFIRST to ID_VARIABLE_ADDLAST, even though not
// all ids in the range will be used.
void CVarView::AddQuant(int nID, CString strTypeId, CString strPrefix, CString strValue, 
						CString strDlgValue, CString strSpec)
{
	static int idCounter = ID_VARIABLE_MAX_STATIC;

	QuantInfo * pQuant = new QuantInfo();
	pQuant->strTypeId = strTypeId;
	pQuant->strPrefix = strPrefix;
	if (! strValue.IsEmpty()) 
		pQuant->strValue = strValue;
	else {  
		// no friendly name specified, just use TypeId, changing -'s to spaces
		pQuant->strValue = strTypeId;
		pQuant->strValue.Replace("-", " ");
	}
	if (! strDlgValue.IsEmpty())
		pQuant->strDlgValue = strDlgValue;
	else // no alternate dialog quant name specified, just use strValue.
		pQuant->strDlgValue = pQuant->strValue;
	pQuant->strSpec = strSpec;
	if (nID >= ID_VARIABLE_ADDFIRST && nID <= ID_VARIABLE_MAX_STATIC) {
		pQuant->id = nID;
	} else { 
		// see if we have a compiled-in id for this quantity (other code is relying on it)
		pQuant->id = LookupStaticId(strTypeId);
		if (pQuant->id <= 0) {
			// generate new dynamic id, updating counter
			pQuant->id = ++idCounter;
		}
	}
	m_quants.SetAtGrow(m_quants.GetSize(), pQuant);  
}

extern int split(const CString& str, const CString& Sep, CStringArray& result);

// Load quantity info from tab-delimited table file
// This appends these newer quantities to the info in hardcoded tables
void CVarView::LoadQuantInfo(LPCSTR pszPathName)
{
	CStdioFile fileSrc(pszPathName, CFile::modeRead);
	CString strLine;
	while (fileSrc.ReadString(strLine)) {
		if (strLine.IsEmpty()) continue;
		CStringArray strFields;
		int nFields = split(strLine, "\t", strFields);
		ASSERT(nFields >= 5);
		AddQuant(-1, strFields[0], strFields[1], strFields[2], strFields[3], strFields[4]);
	}
}

// static quantity info compiled into workbench, and for vector quantities
// this struct uses C-style strings to load from table initializers

typedef struct
{	int	id;				// integer quantity code
	char* szTypeId;		// quantity typeid w/o spaces. Used in logging cmd and entry and helpsys API
	char* szPrefix;		// variable name prefix
	// following optional, only used if needed to build a dialog box:
	char* szValue;      // human-readable quantity type name (may contain spaces), lower case
	char* szDlgValue;   // value prefix shown in dialog, if different from szValue
	char* szSpec;		// dialog spec
} QuantTblEntry;

// Vector quantities are represented by CVector's when drawn. This info only applies for CVariable 
// vector quantities. These are only produced when defining vector quantities for nsh quantity choice.
// Since these quantity type ids are sent to help system then, they should match helpsystems. Unclear 
// if these typeids have any other use, though perhaps are logged during nsh quantity selection. 
// (CVectors use their own type ids for vector logging/entry recording, which may not 
// match these exactly, but don't think any problem arises from that.)
//
// Note prefixes for vector quants not yet fetched from here -- see CVector::GetLabelPrefix
// Possible problem for vectors with angular/linear variants: types represented differently. E.g.
// ID_VARIABLE_ADDANGVELOCITY CVariable type corresponds to VECTOR_VELOCITY CVector w/m_bAngular=TRUE
// but could change to get this info from here as well.  
static QuantTblEntry vectorQuants[] = 
{
// vector quantities:
{ ID_VARIABLE_ADDFORCE,          "force",			"F", },
{ ID_VARIABLE_ADDVELOCITY,       "velocity",		"v", },
{ ID_VARIABLE_ADDACCELERATION,   "acceleration",	"a", },
{ ID_VARIABLE_ADDDISPLACEMENT,   "displacement",	"d", },
{ ID_VARIABLE_ADDMOMENTUM,		 "momentum",		"p", },
{ ID_VARIABLE_ADDANGACCELERATION,"ang-acceleration", "$a", },
{ ID_VARIABLE_ADDANGVELOCITY,	 "ang-velocity",	 "$w", },
{ ID_VARIABLE_ADDANGDISPLACEMENT,"ang-displacement", "$q", },
{ ID_VARIABLE_ADDANGMOMENTUM,	 "ang-momentum",	 "L", },
{ ID_VARIABLE_ADDRELPOS,         "position",		"r",  },
{ ID_VARIABLE_ADDRELVEL,         "relative-vel",	"v", },
{ ID_VARIABLE_ADDTORQUE,         "torque",			"$t", },
{ ID_VARIABLE_ADDEFIELD,         "E-field",			"E", },
{ ID_VARIABLE_ADDBFIELD,         "B-field",			"B", },
{ ID_VARIABLE_ADDIMPULSE,        "impulse",         "J", },
{ ID_VARIABLE_ADDUNITVECTOR,     "unit-vector",     "n", },
{ ID_VARIABLE_ADDMAGDIPOLE,      "mag-dipole",      "$m", },
{ ID_VARIABLE_ADDELECDIPOLE,     "elec-dipole",      "p", },
};
const int numVectors ARRAY_SIZE(vectorQuants);

// scalars will start just after all vectors entered in full m_quants table:
#define FIRST_SCALAR_INDEX numVectors

// InitQuantTable builds the m_quants table
void CVarView::InitQuantTable()
{
	// first init vectors from static table
	for (int i = 0;  i < numVectors; ++i) {
		QuantTblEntry* pEnt = &vectorQuants[i];
		AddQuant(pEnt->id, pEnt->szTypeId, pEnt->szPrefix, pEnt->szValue, pEnt->szDlgValue, pEnt->szSpec);
	}
	// then add scalars dynamically from external file
	try {
		LoadQuantInfo(g_strAndesDir + "kb/" + "scalars.tsv");
		// also try loading feature sets here
		LoadFeatureSets(g_strAndesDir + "kb/" + "features.tsv");
	} 
	catch (CFileException* pEx) {
		pEx->ReportError();
		pEx->Delete();
	}
/*
	TRACE("Loaded quantity table:\n");
	for (int j=0; j < m_quants.GetSize(); j++) {
		QuantInfo* pQuant = m_quants[j];
		TRACE("[%2d] ", j);
		TRACE("typeid=%s ", pQuant->strTypeId);
		TRACE("id=%d ", pQuant->id);
		TRACE("value=%s ", pQuant->strValue);
		TRACE("spec=%s\n", pQuant->strSpec);
	}
*/
}

// for accessing quantity information:

CString CVarView::LookupTypeId(int nID)		// integer type code -> strTypeId
{
	CString str;
	for (int i = 0; i < m_quants.GetSize(); i++) {
		if (m_quants[i]->id == nID)
			return m_quants[i]->strTypeId;
	}
	return str;
}

int CVarView::LookupId(CString strTypeId)  // strTypeId -> integer type code
{
	// for backwards compatibility: convert old typeIds to new (may be in old logs or solutions)
	if (strTypeId == "distance-travelled") strTypeId = "distance-traveled";
	else if (strTypeId == "compression-distance") strTypeId = "compression";
	// can now have angular-frequency !
	//else if (strTypeId.Find("angular-") != -1)
	//	strTypeId.Replace("angular-", "ang-");

	for (int i = 0; i < m_quants.GetSize(); i++) {
		if (strcmpi(m_quants[i]->strTypeId, strTypeId) == 0)
			return m_quants[i]->id;
	}
	return -1;
}

int CVarView::ValueToId(CString strValue)  // strValue (human-readable) -> integer type code
{
	// for backwards compatibility: convert old strValues to new (may be in scalar vars in old solutions)
	if (strValue == "distance travelled") strValue = "distance traveled";
	else if (strValue == "rate of current change") strValue = "rate of change of current";
	
	int nResult = -1;
	for (int i = 0; i < m_quants.GetSize(); i++) {
		if (strcmpi(m_quants[i]->strValue, strValue) == 0) {
			// Sometimes have duplicate "friendly" names, e.g. "power" for different
			// power quantities. Prefer to return a hit that is appropriate to this problem
			if (HasFeature(m_quants[i]->strTypeId)) {
				return m_quants[i]->id;	// return it immediately
			} else {
				// save as backup return if no problem-appropriate hit is found
				nResult = m_quants[i]->id;
			}
		}
	}
	return nResult;
}

CString CVarView::LookupPrefix(int nID)
{
	CString str;
	for (int i = 0; i < m_quants.GetSize(); i++) {
		if (m_quants[i]->id == nID)
			return m_quants[i]->strPrefix;
	}
	return str;
}

CString CVarView::LookupStrValue(int nID)
{
	CString str;
	for (int i = 0; i < m_quants.GetSize(); i++) {
		if (m_quants[i]->id == nID)
			return m_quants[i]->strValue;
	}

	return str;
}

CString CVarView::LookupDlgValue(int nID)
{
	CString str;
	for (int i = 0; i < m_quants.GetSize(); i++) {
		if (m_quants[i]->id == nID)
			return m_quants[i]->strDlgValue;
	}

	return str;
}

CString CVarView::LookupSpec(int nID)
{
	CString str;
	for (int i = 0; i < m_quants.GetSize(); i++) {
		if (m_quants[i]->id == nID)
			return m_quants[i]->strSpec;
	}

	return str;
}

// for allow sets of variables to be associated with feature names
typedef struct {
	CString strName;				// feature name
	CStringList strFeatureList;		// list of features associated
} FeatureSet;

typedef CTypedPtrArray<CPtrArray, FeatureSet*> CFeaturePArray;
static CFeaturePArray m_featureSets;

void CVarView::AddFeatureSet(CString strName, CString strFeatureList)
{
	FeatureSet* pSet = new FeatureSet();
	pSet->strName = strName;
	SplitStr(strFeatureList, pSet->strFeatureList, "; ");

	m_featureSets.SetAtGrow(m_featureSets.GetSize(), pSet);
}

void CVarView::LoadFeatureSets(LPCTSTR pszPathName)
{
	CStdioFile fileSrc(pszPathName, CFile::modeRead);
	CString strLine;
	CStringArray strFields;
	while (fileSrc.ReadString(strLine)) {
		if (strLine.IsEmpty()) continue;
		int nFields = split(strLine, "\t", strFields);
		ASSERT(nFields >= 2);
		AddFeatureSet(strFields[0], strFields[1]);
	}
}


static BOOL FeatureSetContains(CString& strFeatureSet, CString& strFeature)
{
	for (int s = 0; s < m_featureSets.GetSize(); s++) {
		CString strSetName = m_featureSets[s]->strName;
		if (strSetName.CompareNoCase(strFeatureSet)==0) {
			return m_featureSets[s]->strFeatureList.Find(strFeature) != NULL;
		}
	}
	return FALSE;
}

BOOL CVarView::HasFeature(CString strTypeId)
{
	// check if it's in feature set specified for this problem
	CFBDDoc* pDoc = theApp.GetCurrentProblem();
	if (! pDoc) return TRUE; // no info to disable
	// for each feature in doc...
	for (POSITION pos = pDoc->m_strFeatures.GetHeadPosition(); 
	     pos != NULL;) 
	{
		CString strDocFeature = pDoc->m_strFeatures.GetNext(pos);
		// if doc feature is this quantid, enable it
		if (strDocFeature.CompareNoCase(strTypeId)==0)
			return TRUE;
	
		// if docFeature is actually a set, see if set includes variable
		if (FeatureSetContains(strDocFeature, strTypeId))
			return TRUE;
	}
	return FALSE;
}

// add scalar variable commands to given menu from table
void CVarView::AddScalarVars(CMenu* pMenu, BOOL bQuantityChoice)
{
	for (int i = FIRST_SCALAR_INDEX; i < m_quants.GetSize(); i++)
	{
		// hackety hack -- if building all quantity menu, leave out angle
		if (bQuantityChoice && (strcmp(m_quants[i]->strTypeId, "angle") == 0))
			continue;

		if (HasFeature(m_quants[i]->strTypeId))
		{
			CString strValue = m_quants[i]->strValue;
			strValue.SetAt(0, toupper(strValue[0]));
			pMenu->AppendMenu(MF_STRING, m_quants[i]->id, strValue);
		}
	}
}

IMPLEMENT_DYNCREATE(CVarView, CListViewEx)

CVarView::CVarView()
{
	//{{AFX_DATA_INIT(CVarView)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
	m_bEnabled = TRUE;

	// Don't extend selection the whole width. This option defined
	// in CListViewEx; this overrides default set in base class constructor.
	m_bClientWidthSel = FALSE;
}

CVarView::~CVarView()
{
	m_VarList.RemoveAll();
}

void CVarView::DoDataExchange(CDataExchange* pDX)
{
	CListView::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CVarView)
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CVarView, CListViewEx)
	//{{AFX_MSG_MAP(CVarView)
	ON_COMMAND(ID_VARIABLE_MODIFY, OnVariableModify)
	ON_UPDATE_COMMAND_UI(ID_VARIABLE_MODIFY, OnUpdateVariableModify)
	ON_UPDATE_COMMAND_UI(ID_VARIABLE_DELETE, OnUpdateVariableDelete)
	ON_COMMAND(ID_HELP_WHATSWRONG, OnVariableWhatswrong)
	ON_UPDATE_COMMAND_UI(ID_HELP_WHATSWRONG, OnUpdateVariableWhatswrong)
	ON_WM_CREATE()
	ON_WM_CONTEXTMENU()
	ON_NOTIFY_REFLECT(LVN_ITEMCHANGED, OnItemchanged)
	ON_UPDATE_COMMAND_UI(ID_VARIABLE_ADDTIME, OnUpdateVariableAddtime)
	ON_UPDATE_COMMAND_UI(ID_EDIT_DELETE, OnUpdateEditDelete)
	ON_COMMAND(ID_EDIT_DELETE, OnVariableDelete)
	ON_WM_LBUTTONDBLCLK()
	ON_UPDATE_COMMAND_UI(ID_VARIABLE_ADDANGLE, OnUpdateVariableAddangle)
	ON_COMMAND(ID_VARIABLE_SOLVEFOR, OnVariableSolvefor)
	ON_UPDATE_COMMAND_UI(ID_VARIABLE_SOLVEFOR, OnUpdateVariableSolvefor)
	ON_COMMAND(ID_VARIABLE_DELETE, OnVariableDelete)
	//}}AFX_MSG_MAP
	ON_COMMAND_RANGE(ID_VARIABLE_ADDFIRST, ID_VARIABLE_ADDLAST, OnVariableNew)
	ON_UPDATE_COMMAND_UI_RANGE(ID_VARIABLE_ADDFIRST, ID_VARIABLE_ADDLAST, OnUpdateVariableNew)

END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CVarView diagnostics

#ifdef _DEBUG
void CVarView::AssertValid() const
{
	CListView::AssertValid();
}

void CVarView::Dump(CDumpContext& dc) const
{
	CListView::Dump(dc);
}

CFBDDoc* CVarView::GetDocument() // non-debug version is inline
{
	ASSERT(((CChildFrame*)GetParentFrame())->m_pDoc->IsKindOf(RUNTIME_CLASS(CFBDDoc)));
	return ((CChildFrame*)GetParentFrame())->m_pDoc;
}

#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CVarView message handlers

int CVarView::OnCreate(LPCREATESTRUCT lpCreateStruct) 
{
	lpCreateStruct->style |=LVS_REPORT| LVS_SHAREIMAGELISTS 
		|LVS_SINGLESEL |LVS_ALIGNLEFT /*|/LVS_NOCOLUMNHEADER*/;

	if (CListViewEx::OnCreate(lpCreateStruct) == -1)
		return -1;

	return 0;
}

void CVarView::OnInitialUpdate() 
{
	// Do base class initialization
	CListViewEx::OnInitialUpdate();

	// Create state image list for control. Images used to show status of variables.
	m_imgStatus.Create(IDR_STATUS, 13, 1, RGB(255, 255, 255));
	GetListCtrl().SetImageList (&m_imgStatus, TVSIL_STATE);
	
	// Create columns
	GetListCtrl().InsertColumn(0, "Name", LVCFMT_LEFT, 100, 0);		// variable Label
	GetListCtrl().InsertColumn(1, "Definition", LVCFMT_LEFT, 200, 1);// definition
	// remaining columns used for vectors only:
	GetListCtrl().InsertColumn(2, "Dir", LVCFMT_LEFT,  60, 2);// dir var
	GetListCtrl().InsertColumn(3, "X-Comp", LVCFMT_LEFT, 60, 3);	// x-component
	GetListCtrl().InsertColumn(4, "Y-Comp", LVCFMT_LEFT, 60, 4);	// y-component
	// z-comp column only shown for problems that may use z-axis vectors
	if (GetDocument()->UseZAxis()) {	
		GetListCtrl().InsertColumn(5, "Z-Comp", LVCFMT_LEFT, 60, 5);// z-component
	}

	// Insert predefined time points
	if (! GetDocument()->m_strTimes.IsEmpty() )
		InsertTimeVariables();

	// Populate list with any variables from document
	PopulateList();
}

void CVarView::InsertTimeVariables()
{
	POSITION pos = GetDocument()->m_strTimes.GetHeadPosition();
	while (pos != NULL)
	{
		CString strTime = GetDocument()->m_strTimes.GetNext(pos);
		CString strName;
		CString strDef;
		int eqPos = strTime.Find('=');
		if (eqPos < 0){
			return;
		}
		strName = strTime.Left(eqPos);
		strName.TrimRight();//space will screw up solve-for
		strDef = strTime.Mid(eqPos + 2);//=space
		int index = GetListCtrl().GetItemCount();
		SizeToFit(1, strName);
		GetListCtrl().InsertItem(index, strName);//insert and set status 
		GetListCtrl().SetItemText(index, 1, strDef);
		GetListCtrl().SetItemState(index, INDEXTOSTATEIMAGEMASK(5), LVIS_STATEIMAGEMASK);
	}
}


void CVarView::PopulateList()
{
	CFBDDoc* pDoc = GetDocument();
	
	// initialize list from m_Variables list contained in document
	if (!pDoc->m_Variables.IsEmpty()){
		POSITION pos = pDoc->m_Variables.GetHeadPosition();
		while (pos != NULL){
			CVariable* pVar = pDoc->m_Variables.GetNext(pos);
			//if (pVar->m_flag == STUDENT_OBJECT) // named student entry 
				InsertListItem(pVar);
		}
	}

	// Insert drawing object variables from m_objects list contained in document
	if (! pDoc->GetObjects()->IsEmpty()) {
		POSITION pos = pDoc->GetObjects()->GetHeadPosition();
		while (pos != NULL) {
			CDrawObj* pObj = pDoc->GetObjects()->GetNext(pos);
			if (pObj->m_flag == TEACHER_OBJECT)
				continue;
			if (pObj->IsKindOf(RUNTIME_CLASS(CCheckedObj))){
				CCheckedObj* pChObj = (CCheckedObj*)pObj;
				InsertListItem(pChObj);	
			}
		}
	}
}

void CVarView::InsertListItem(CCheckedObj * pObj)
{
	// now ignore system (body) objects in variable view
	if (pObj->IsKindOf(RUNTIME_CLASS(CSystem)))
		return;

	// add object to var list
	if (m_VarList.Find(pObj) == NULL)
		m_VarList.AddTail(pObj);
	
	CString strName = pObj->m_strName;
	CString strDef;

	if (pObj->IsKindOf(RUNTIME_CLASS(CAxes))) {
		CString strXLabel = "x";
		if (((CAxes*)pObj)->m_nIndex > 0) 
			strXLabel.Format("x%d", ((CAxes*)pObj)->m_nIndex);
		strName = strXLabel;
		strDef = "axis";
	} 
	else if ( (pObj->IsKindOf(RUNTIME_CLASS(CVariable))) 
		|| (pObj->IsKindOf(RUNTIME_CLASS(CRadius))) 
			|| (pObj->IsKindOf(RUNTIME_CLASS(CAngle))) 
				/*|| (pObj->IsKindOf(RUNTIME_CLASS(CSystem)))*/ )
	{
		strDef = pObj->GetDef();

		// For variables: append given value if specified
		if (pObj->IsKindOf(RUNTIME_CLASS(CVariable)) 
			&& ! ((CVariable*)pObj)->m_strValue.IsEmpty() ) {
			strName += "=" + ((CVariable*)pObj)->m_strValue;
		}
	}
	else if (pObj->HasComponents())//vectors only(!components)  (variables taken care of above)
	{
		CString str;
		str = pObj->GetDef();
		strDef.Format("magnitude of the %s", str);
		// for unit vectors 
	}
	else // not an object we are interested in
		return;

	// For system "x", associated mass variable is named "mx".
/*	if (pObj->IsKindOf(RUNTIME_CLASS(CSystem)))
		strName = "m" + strName; */
	
	// Adjust column widths to fit. Fitting name more important, so do last
	SizeToFit(1, strDef);
	SizeToFit(0, strName);
	
	// Insert or modify item in list
	int index = FindIndex(pObj);
	if (index == -1) //	not already in list
	{
		index = GetListCtrl().GetItemCount();
		GetListCtrl().InsertItem(index, strName);
	}
	else
		GetListCtrl().SetItemText(index, 0, strName);

	GetListCtrl().SetItemText(index, 1, strDef);
	GetListCtrl().SetItemState(index, INDEXTOSTATEIMAGEMASK(pObj->m_status+1), LVIS_STATEIMAGEMASK);
	GetListCtrl().SetItemData(index, (DWORD)pObj);//set data = obj pointer

	// Update component names, if needed
	if (pObj->HasComponents() /* && GetDocument()->IsAxesDrawn() */)
		UpdateComponents(pObj);
	// update axis direction, if needed
	if (pObj->IsKindOf(RUNTIME_CLASS(CAxes)))
		UpdateAxisDir(pObj);
	// update angle variable direction, if needed
	if (pObj->IsKindOf(RUNTIME_CLASS(CVariable)) &&
	    ((CVariable*)pObj)->m_nType == ID_VARIABLE_ADDANGLE) 
		UpdateAngleVarDir(pObj);
}



void CVarView::OnUpdate(CView* pSender, LPARAM lHint, CObject* pHint) 
{
	switch (lHint)
	{
	case HINT_DELETE_VARIABLE:			// Deleted a defined variable
		{
			if (pSender != this){//iterate the list and delete the hint var from it
				CVariable* pVar = (CVariable*) pHint;
				DeleteListItem(pVar);
			}
		}
		break;
	case HINT_DELETE_SELECTION:			// Deleted one or more drawing objects	
		{
			CDrawObjList* pList = (CDrawObjList*) pHint;
			POSITION pos = pList->GetHeadPosition();
			while (pos != NULL)
			{
				CDrawObj* pObj = pList->GetNext(pos);
				if (!pObj->InVarList())//checkedobjs in var list, drawobjs not
					continue;
				if  ( (pObj->m_flag == TEACHER_OBJECT) 
					&& (!pObj->IsKindOf(RUNTIME_CLASS(CVariable))) )
					return;
				CCheckedObj* pChObj = (CCheckedObj*)pObj;
				if (pChObj->IsKindOf(RUNTIME_CLASS(CAxes)))
				{//remove from varlist
					POSITION pos = m_VarList.Find(pObj);
					if (pos != NULL)
						m_VarList.RemoveAt(pos);
					UpdateAllComponents();
					DeleteListItem(pChObj);
				}
				else if (pObj->IsComponent())
				{
					CVector* pComp = (CVector*)pObj;
					CVector* pCompOfObj = FindCompOf(pComp->m_strCompOf);
					if (pCompOfObj == NULL)
						return;
					POSITION pos = pCompOfObj->m_Comps.Find(pComp);
					if (pos != NULL)
						pCompOfObj->m_Comps.RemoveAt(pos);
					UpdateComponents(pCompOfObj);
					// remove from var list 
					if (pos = m_VarList.Find(pObj))
						m_VarList.RemoveAt(pos);
				}
				else
					DeleteListItem(pChObj);//removes from varlist
			}
			
		}
		break;
	case HINT_UPDATE_DRAWOBJ:			// Wrote through modifications to a CDrawObj 
		{
			CDrawObj* pObj = (CDrawObj*) pHint;
			if (!pObj->InVarList())
				return;

			if  ( (pObj->m_flag == TEACHER_OBJECT) 
				&& (!pObj->IsKindOf(RUNTIME_CLASS(CVariable))) )
				return;
			
			if (pObj->IsKindOf(RUNTIME_CLASS(CCheckedObj)))
			{
				CCheckedObj* pObj = (CCheckedObj*)pHint;
			
				if (pObj->IsKindOf(RUNTIME_CLASS(CSystem)))
				{
				/*
					CVariable* pVar = GetDocument()->GetMatchingVar(pObj, FALSE);//bMatchDef
					if (pVar != NULL)
					{
						DeleteListItem(pObj);
						return;
					}
				*/
					return; // ignore, now drawn bodies are of no interest to us.
				}
				
				if (pObj->IsKindOf(RUNTIME_CLASS(CAxes)))
				{//add to varlist
					if (m_VarList.Find(pObj) == NULL)
						m_VarList.AddTail(pObj);
					// new: add/update axis var line with direction
					InsertListItem(pObj);
					// and add component vars if new:
					UpdateAllComponents();
				}
				else if (pObj->IsComponent())
				{
					CVector* pComp = (CVector*)pObj;
					CVector* pCompOfObj = FindCompOf(pComp->m_strCompOf);
					if (pCompOfObj != NULL)
						UpdateComponents(pCompOfObj);
				}
				else
				{
					InsertListItem(pObj);
				}
				CVariable* pBadVar = CheckInVarList(pObj);
				if (pBadVar && (pBadVar != pObj))//don't want to delete obj we are editing
				{								//until finished
					DeleteListItem(pBadVar);//removes from view's m_varList
					GetDocument()->RemoveVariable(pBadVar);//deletes variable and removes from list
					pBadVar->NotifyDelete();
					pBadVar->Delete();
				}
			}
			
		}
		break;
		case HINT_UPDATE_TEMPOBJ:		// Submitted edits of tempobj buffer, not yet written through
		{
			CChkObjList* pList = (CChkObjList*)pHint;
			if (pList->IsEmpty())
				return;
			
			CCheckedObj* pRealObj = pList->GetHead();
			CCheckedObj* pTempObj = pList->GetTail();
			if (!pRealObj->InVarList())
				return;
			
			if  ( (pRealObj->m_flag == TEACHER_OBJECT) 
				&& !pRealObj->IsKindOf(RUNTIME_CLASS(CVariable)) )
				return;

			if (pRealObj->IsKindOf(RUNTIME_CLASS(CSystem)))
				return;  // now ignore systems (bodies) in variable view

			if (pRealObj->IsKindOf(RUNTIME_CLASS(CAxes)))
			{//add to varlist
				if (m_VarList.Find(pRealObj) == NULL)
					m_VarList.AddTail(pRealObj);
				UpdateAllComponents();
			}
			else if (pRealObj->IsComponent())
			{
				CVector* pComp = (CVector*)pTempObj;
				CVector* pCompOfObj = FindCompOf(pComp->m_strCompOf);
				UpdateComponents(pCompOfObj);
			}
			else {
				UpdateListItem(pRealObj, pTempObj);
			}

		}
		break;
	
	case HINT_TUTOR_MODE:
		EnablePane(! (DWORD) pHint);
		break;

	default:
		break;
	}
}
//
// Enable/Disable whole view
void CVarView::EnablePane(BOOL bEnable)
{
	// Save state for quick access in command enablers
	m_bEnabled = bEnable;
	EnableWindow(bEnable);
	// Set background color appropriately for text item drawing
	// (ListViewEx could do this itself if disabled, but evidently doesn't)
	GetListCtrl().SetTextBkColor(bEnable ? ::GetSysColor(COLOR_WINDOW) : 
								GetSysColor(COLOR_3DFACE));
}

CVariable* CVarView::CheckInVarList(CCheckedObj* pObj)
{
	POSITION pos = m_VarList.GetHeadPosition();
	while (pos != NULL)
	{
		CCheckedObj* pExistObj = m_VarList.GetNext(pos);
		//if one is variable and one isn't, return matching var
		if ( (pExistObj->IsKindOf(RUNTIME_CLASS(CVariable))) &&
			 (!pObj->IsKindOf(RUNTIME_CLASS(CVariable))) && (pObj->HasSameDef( ((CVariable*)pExistObj))) &&
			 pObj->HasSameName(pExistObj)  )
		{
			return (CVariable*) pExistObj;
		}//vice versa
		else if ( (!pExistObj->IsKindOf(RUNTIME_CLASS(CVariable))) &&
			 (pObj->IsKindOf(RUNTIME_CLASS(CVariable))) && (pExistObj->HasSameDef( ((CVariable*)pObj) )) 
			 && pExistObj->HasSameName(pObj))
		{
			return (CVariable*) pObj;
		}
		else if (pExistObj->IsKindOf(RUNTIME_CLASS(CVariable)) &&
			pObj->IsKindOf(RUNTIME_CLASS(CVariable)) &&
				pExistObj->HasSameName(pObj) && (pExistObj->m_status == statusError) )
		{
			return (CVariable*) pExistObj;
		}
	}
	return NULL;

}

void CVarView::OnVariableNew(UINT nID) 
{
	// Log the operation, with variable type
	CString str = LookupTypeId(nID);
	ASSERT(! str.IsEmpty());	// must find quant-type id for logging
	if (str.IsEmpty()) return;  // release-mode: fail if type not in table
	LogEventf(EV_NEW_VARIABLE, "%s", str);

	// add a new variable of appropriate type to document
	CVariable* pVar = new CVariable();
	pVar->m_nType = nID;
	CFBDDoc* pDoc = GetDocument();
	pDoc->AddVariable(pVar);
	
	// collect definition with variable's dialog
	if (pVar->OnEditProperties() != IDOK)
	{ 
		//dialog cancelled 
		DeleteListItem(pVar);		//removes from view's m_varList	
		pDoc->RemoveVariable(pVar); //deletes variable and removes from list
		pVar->NotifyDelete();
		pVar->Delete();
	}
	else // dialog OK'd
	{
		CVariable* pBadVar = CheckInVarList(pVar);
		//existing drawobj with same def, delete var
		if (pBadVar)
		{
				DeleteListItem(pBadVar);//removes from view's m_varList
				GetDocument()->RemoveVariable(pBadVar);//deletes variable and removes from list
				pBadVar->NotifyDelete();
				pBadVar->Delete();
		}
	}
}

void CVarView::OnUpdateVariableNew(CCmdUI* pCmdUI) 
{
	// unnecessary
	// whole variable menu is disabled when within a qualitative problem
/*	if (GetDocument()->m_nProblemType == PROB_QUAL) 
		pCmdUI->Enable(FALSE);
	else 
		pCmdUI->Enable(TRUE);*/
	pCmdUI->Enable(m_bEnabled);
}

void CVarView::OnVariableModify() 
{
	// get selected object
	int index = GetListCtrl().GetNextItem(-1, LVNI_ALL|LVNI_SELECTED);
	if (index == -1) return;	// shouldn't happen
	LONG dwData = GetListCtrl().GetItemData(index);
	ASSERT(dwData != NULL);
	ASSERT_KINDOF(CCheckedObj, (CObject*) dwData);
	CCheckedObj* pChObj = (CCheckedObj*)dwData;

	LogEventf(EV_MODIFY_VARIABLE, "%s", pChObj->m_strName);	// include name for reader

	if (pChObj->OnEditProperties() == IDOK)//Added to the list when send the UpdateObject hint
	{

		if (pChObj->IsKindOf(RUNTIME_CLASS(CVariable)))
		{
			CVariable* pBadVar = CheckInVarList((CVariable*)pChObj);
			//existing drawobj with same def, delete var
			if (pBadVar)
			{
				DeleteListItem(pBadVar);//removes from view's m_varList
				GetDocument()->RemoveVariable(pBadVar);//deletes variable and removes from list
				pBadVar->NotifyDelete();
				pBadVar->Delete();
				return;
			}
		}
	}
	else
	{
		pChObj->CheckObject();
	}
	GetDocument()->UpdateAllViews(NULL, HINT_UPDATE_DRAWOBJ, pChObj);
}

void CVarView::OnUpdateVariableModify(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable(GetListCtrl().GetNextItem(-1, LVNI_ALL|LVNI_SELECTED) != -1
					&& m_bEnabled);
}

void CVarView::OnVariableDelete() 
{
	// get selected object
	int index = GetListCtrl().GetNextItem(-1, LVNI_ALL|LVNI_SELECTED);
	if (index == -1) return;	// shouldn't happen
	LONG dwData = GetListCtrl().GetItemData(index);
	ASSERT(dwData != NULL);
	ASSERT_KINDOF(CCheckedObj, (CObject*) dwData);
	CCheckedObj* pChObj = (CCheckedObj*)dwData;

	LogEventf(EV_DELETE_VARIABLE, "%s", pChObj->m_strName);// include name for reader

	if (pChObj->IsKindOf(RUNTIME_CLASS(CVariable))){
		CVariable* pVar = (CVariable*)pChObj;
		GetDocument()->UpdateAllViews(this, HINT_DELETE_VARIABLE, pVar);
		GetDocument()->RemoveVariable(pVar);//remove from m_variables list
	}else{
		GetDocument()->UpdateAllViews(NULL, HINT_UPDATE_WINDOW, NULL);
		GetDocument()->Remove(pChObj);//remove from m_objects list
	}

	DeleteListItem(pChObj);//removes from view's m_varList
	pChObj->NotifyDelete();//Notify help system
	pChObj->Delete();
	
}

void CVarView::OnUpdateVariableDelete(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable(GetListCtrl().GetNextItem(-1, LVNI_ALL|LVNI_SELECTED) != -1
					&& m_bEnabled);
}

void CVarView::OnVariableWhatswrong() 
{
	// get selected object
	int index = GetListCtrl().GetNextItem(-1, LVNI_ALL|LVNI_SELECTED);
	if (index==-1){
		theApp.DoWarningMessage("Nothing selected!");
		return;
	}
	LONG dwData = GetListCtrl().GetItemData(index);
	ASSERT(dwData != NULL);
	ASSERT_KINDOF(CCheckedObj, (CObject*) dwData);
	CCheckedObj* pChObj = (CCheckedObj*)dwData;
	
	LogEventf(EV_VARIABLE_WHATSWRONG, "%s", pChObj->m_strName); // name for reader

	// For bug in Andes 6.0.*, Fall 2001: Use STR2ARG to protect against 
	// empty m_strName. Happens in case of axes, now shown on varlist 
	LPCTSTR pszResult =	HelpSystemExecf("(Why-wrong-object %s %s) ", 
			                 STR2ARG(pChObj->m_strName), pChObj->m_strId );

	// Ask frame to show result in hint window
	theApp.GetMainFrame()->ShowHint(pszResult, WhatsWrong);
}

void CVarView::OnUpdateVariableWhatswrong(CCmdUI* pCmdUI) 
{

	int index = GetListCtrl().GetNextItem(-1, LVNI_ALL|LVNI_SELECTED); 
	if ((index != -1) && 
		(GetListCtrl().GetItemState(index, LVIS_STATEIMAGEMASK) == INDEXTOSTATEIMAGEMASK(3)) )

		pCmdUI->Enable((theApp.m_wHelpFlags & fWhatsWrong) // make sure enabled in app
		                && m_bEnabled);
	else
		pCmdUI->Enable(FALSE);
}


void CVarView::OnContextMenu(CWnd* pWnd, CPoint point) 
{
	LogEventf(EV_VARIABLE_MENU, "%d %d", point.x, point.y);
	// CG: This function was added by the Pop-up Menu component
	CMenu menu;
	VERIFY(menu.LoadMenu(IDR_POPUP_VARVIEW));
	CMenu* pPopup = menu.GetSubMenu(0);
	ASSERT(pPopup != NULL);

	CGreekMenu mnuGreek;
	mnuGreek.Attach(pPopup->GetSafeHmenu());


	CPoint local = point;	// point for this message comes 
							//in screen coords
	ScreenToClient(&local);
	LVHITTESTINFO lvhti;// Clear the subitem text the user clicked on.
	lvhti.pt = local;
	GetListCtrl().SubItemHitTest(&lvhti);

	m_strVar.Empty();
	CString strHitVar;
	if (lvhti.iSubItem != 1)
		strHitVar = GetListCtrl().GetItemText(lvhti.iItem, lvhti.iSubItem);

	// if item showing "var=value" eqn, split out var name on lhs.
	int posEq;
	if ((posEq = strHitVar.Find("=")) != -1)
		strHitVar = strHitVar.Left(posEq);
	
	if (!strHitVar.IsEmpty())
	{
		m_strVar.Format("Solve for %s", strHitVar);
		UINT nID = ID_VARIABLE_SOLVEFOR;
		mnuGreek.ChangeMenuItem(nID, (LPCTSTR)m_strVar);
	}
	CVarMenu mnuVar;
	mnuVar.CreatePopupMenu();
	mnuVar.AttachProblemMenu(theApp.GetDocument()->m_wConcept);
	mnuGreek.ModifyMenu(2, MF_BYPOSITION|MF_POPUP, (UINT)mnuVar.m_hMenu, "&Add New Variable");

	CWnd* pWndPopupOwner = this;
	while (pWndPopupOwner->GetStyle() & WS_CHILD)
		pWndPopupOwner = pWndPopupOwner->GetParent();

	mnuGreek.TrackPopupMenu(TPM_LEFTALIGN | TPM_RIGHTBUTTON, point.x, point.y,
		pWndPopupOwner);	
	
}

int CVarView::FindIndex(CCheckedObj* pObj)
{
	int nList = GetListCtrl().GetItemCount();
	for (int i=(nList-1); i>=0; i--)
	{
		LONG dwData = GetListCtrl().GetItemData(i);
		CCheckedObj* pChObj = (CCheckedObj*)dwData;
		if (pObj == pChObj)
			return i;
	}
	return -1;
}

int CVarView::FindLabel(LPCTSTR pszLabel) // return index of item w/given label
{
	int nItems = GetListCtrl().GetItemCount();
	for (int i=0 ; i < nItems; i++)
	{
		CString strLabel =  GetListCtrl().GetItemText(i, 0);
		if (strLabel == pszLabel)
			return i;
	}
	return -1;
}

void CVarView::UpdateAllComponents()
{
	
	POSITION pos = m_VarList.GetHeadPosition();
	while (pos!=NULL)
	{
		CCheckedObj* pObj = m_VarList.GetNext(pos);
		ASSERT(pObj->m_flag != TEACHER_OBJECT);

		if (pObj->HasComponents())
		{
			UpdateComponents(pObj);
//			pObj->UpdateVarNames();
		}
	}

}

void CVarView::UpdateComponents(CCheckedObj * pObj)
{
	CString strx, stry, strz;
	
	if (IsAxesDrawn())
	{
		strx.Format("%s_x", pObj->m_strName);
		stry.Format("%s_y", pObj->m_strName);
		strz.Format("%s_z", pObj->m_strName);

		// look for drawn components linked to vector -- might have different name
		if (pObj->IsKindOf(RUNTIME_CLASS(CVector)) && 
					(!((CVector*)pObj)->m_Comps.IsEmpty()))
		{
			POSITION pos = ((CVector*)pObj)->m_Comps.GetHeadPosition();
			while (pos != NULL)
			{
				CDrawObj* pCompObj = ((CVector*)pObj)->m_Comps.GetNext(pos);
				CVector* pComp = (CVector*)pCompObj;
				if (pComp->m_strCompDir.Find('X') != -1)
					strx = pComp->m_strName;
				else if (pComp->m_strCompDir.Find('Y') != -1)
					stry = pComp->m_strName;
				// no drawn z components yet, but doesn't hurt to be safe:
				else if (pComp->m_strCompDir.Find('Z') != -1)
					strz = pComp->m_strName;
			}
		}
	}

	int index = FindIndex(pObj);
	
	// update component var columns
	GetListCtrl().SetItemText(index, 3, strx);
	GetListCtrl().SetItemText(index, 4, stry);
	if (GetDocument()->UseZAxis()) {
		GetListCtrl().SetItemText(index, 5, strz);
	}

	// also update direction var column on vector here
	if (pObj->IsKindOf(RUNTIME_CLASS(CVector))) {
		CString strDir = GetVecDirString((CVector*)pObj);
		
		// adjust column width to ensure dir var and value are visible
		// Note string contains non-printing Greek letter tags e.g. $qFw
		CString strWidth = strDir;
		strWidth.Remove('$');	// omit tags for better width estimate
		SizeToFit(2, strWidth);

		GetListCtrl().SetItemText(index, 2, strDir);
	}
}

static const char* szDeg = "\260"; // degree sign character in octal

// Get string to show for direction variable.
// String is phiV | thetaV = num | ?
CString CVarView::GetVecDirString(CVector *pVec)
{
	CString strVar, strDir, strResult;
	if (pVec->IsZAxisVector()) {
		strVar = "$j" + pVec->m_strName;
		strDir = pVec->m_nZDir == ZDIR_OUTOF ? "0\260":
						  (pVec->m_nZDir == ZDIR_INTO ? "180\260" : "?");
						  
	} else if (!pVec->IsZeroMag()) {
		strVar = "$q" + pVec->m_strName;
		strDir = pVec->UnknownDir() ? "?" : pVec->m_strOrientation + szDeg;
	}

	if (! strVar.IsEmpty())
		strResult = strVar + "=" + strDir;
	return strResult;
}

void CVarView::UpdateAxisDir(CDrawObj *pObj)
{
	ASSERT_KINDOF(CAxes, pObj);
	CAxes* pAxes = (CAxes*) pObj;

	CString strXLabel = "x";
	if (pAxes->m_nIndex > 0)
		strXLabel.Format("x%d", pAxes->m_nIndex);
	CString strVar = "$q" + strXLabel;
	CString strDir;
	strDir.Format("%d%s", pAxes->GetDirection(), szDeg);
	CString strDef = strVar + "=" + strDir;

	// adjust column width to ensure dir var and value are visible
	// Note string contains non-printing Greek letter tag e.g. $qx
	CString strWidth = strDef;
	strWidth.Remove('$');	// omit tags for better width estimate
	SizeToFit(2, strWidth);

	GetListCtrl().SetItemText(FindIndex(pAxes), 2, strDef);
}

void CVarView::UpdateAngleVarDir(CCheckedObj *pObj)
{
	ASSERT_KINDOF(CVariable, pObj);
	if (!pObj->IsKindOf(RUNTIME_CLASS(CVariable))) return;
	CVariable* pVar = (CVariable*) pObj;
	ASSERT(pVar->m_nType == ID_VARIABLE_ADDANGLE);
	if (! (pVar->m_nType == ID_VARIABLE_ADDANGLE)) return;

	CString strDir = "?";
	int nDegrees = CAngleDlg::GetAngleBetween(pVar->m_strObject, pVar->m_strAgent);
	if (nDegrees != -1) // not unknown
		strDir.Format("%d%s", nDegrees, szDeg);

	CString strDef = pVar->m_strName + "=" + strDir;

	// adjust column width to ensure dir var and value are visible
	// Note string contains non-printing Greek letter tag e.g. $qx
	// This goes in column zero, not in dir field, because variable *is*
	// an angle, not a direction associated with it somehow
	CString strWidth = strDef;
	strWidth.Remove('$');	// omit tags for better width estimate
	SizeToFit(0, strWidth);

	GetListCtrl().SetItemText(FindIndex(pVar), 0, strDef);
}

CVector* CVarView::FindCompOf(CString strCompOf)
{
	POSITION pos = m_VarList.GetHeadPosition();
	while (pos != NULL) 
	{
		CCheckedObj* pObj = m_VarList.GetNext(pos);
		if (!pObj->IsKindOf(RUNTIME_CLASS(CVector)))//only looking at vectors
			continue;
		CVector* pVec = (CVector*)pObj;
		if (!pVec->HasComponents())//only want vectors with components
			continue;
		if (_stricmp(pVec->m_strName, strCompOf)==0)//case insensitive
		{
			return pVec;
		}
		
	}
	return NULL;

}

BOOL CVarView::DispatchEvent(EventID nEvent, LPCTSTR pszArgs)
{	
	CString strArg;
	CString strIndex;
	char szName[80]; // should be long enough
	int index;
	int nId;
	CVariable* pVar;
	switch(nEvent)
	{
	case EV_SELECT_VARIABLE:{
		// newer version (> 7/9/99) msg includes label arg
		// more reliable to select by name if we have it, since index may change
		if (sscanf(pszArgs, "%d %s", &index, szName) == 2) {
			int iName = FindLabel(szName);
			if (iName != -1)
				index = iName;		
		} else { // older version has only index
			strArg = (CString)pszArgs;
			strIndex = strArg.SpanIncluding("0123456789");
			index = atoi(strIndex);
		}

		GetListCtrl().SetFocus();
		if (index < 0 || index >= GetListCtrl().GetItemCount()) 
			return FALSE;
		GetListCtrl().SetItemState(index, LVIS_SELECTED, LVIS_SELECTED);
		Invalidate();
		}break;

	case EV_NEW_VARIABLE:
		strArg = pszArgs;
		if (strArg.IsEmpty()) {
			// bug in Andes 7.0.2 - 3: power variable type id was not logged 
			// (quant-type id string entry for power was missing from table)
			// think this was only omission, so if no arg, just assume it's power.
			nId = ID_VARIABLE_ADDPOWER;
		} else
			nId = LookupId(strArg);
		
		OnVariableNew(nId);
		break;

	case EV_MODIFY_VARIABLE:
		// newer version (> 7/9/99) includes label arg; could verify selection
		OnVariableModify();
		break;

	case EV_DELETE_VARIABLE:
		// newer version (> 7/9/99) includes label arg; could verify selection
		OnVariableDelete();
		break;

	case EV_VARIABLE_WHATSWRONG:
		// newer version (> 7/9/99) includes label arg; could verify selection
		OnVariableWhatswrong();
		break;

	case EV_VAR_ENTRY: // recreate an initial variable entry
		pVar = new CVariable();//create a new variable
		if (! pVar->SetFromLogStr(pszArgs) )
			return FALSE;
		//Add new variable to variable list (cheat: bypass AddVariable, it generates id)
		GetDocument()->m_Variables.AddTail(pVar);
		pVar->m_pDocument = GetDocument();
		// need to update views
		GetDocument()->UpdateAllViews(NULL, HINT_UPDATE_DRAWOBJ, pVar);
		break;
	}
	return TRUE;
}

// move the demo mode pointer to an object:
void CVarView::PointToObject(LPCTSTR pszObjID)
{
	int nIndex;
	// arg should be variable name
	char szVarName[128];
	if (sscanf(pszObjID, "%s", szVarName) != 1)
			return;
	int nCount = GetListCtrl().GetItemCount();
	for (nIndex = 0; nIndex < nCount; nIndex++){
		LONG dwData = GetListCtrl().GetItemData(nIndex);
		CCheckedObj* pObj = (CCheckedObj*)dwData;
		if (pObj->m_strName == szVarName){
			CRect rcItem;
			GetListCtrl().GetItemRect(nIndex, &rcItem, LVIR_LABEL);
			ClientToScreen(&rcItem);
			// Set pointer, to left, bottom
			theApp.GetMainFrame()->MovePointerTo(rcItem.left, rcItem.bottom, CPtrWnd::UpRight);
			return;
		}
	}
	return;
}


void CVarView::OnItemchanged(NMHDR* pNMHDR, LRESULT* pResult) 
{
	NM_LISTVIEW* pNMLV = (NM_LISTVIEW*)pNMHDR;
	int itemIndx = pNMLV->iItem;
	//gets called twice on selection, I think
	if ( (pNMLV->uChanged & LVIF_STATE) &&		// notification includes state change and
		 (pNMLV->uNewState & LVIS_SELECTED) &&  // new state has selected bit
		 ! (pNMLV->uOldState & LVIS_SELECTED) ) // old state lacked selected bit
	{
		// include label in log message for readability and robustness, since 
		// indices may change as new variables are added (e.g. time points)
		CString strLabel = GetListCtrl().GetItemText(itemIndx, 0);
		LogEventf(EV_SELECT_VARIABLE, "%d %s", itemIndx, strLabel);
	}
	*pResult = 0;
}

void CVarView::DeleteListItem(CCheckedObj * pObj)
{
	int index = FindIndex(pObj);
	if (index >= 0)
		GetListCtrl().DeleteItem(index);

	POSITION pos = m_VarList.Find(pObj);
	if (pos != NULL)
		m_VarList.RemoveAt(pos);


}

void CVarView::SizeToFit(int nCol, CString str)
{
	CRect rect;
	GetClientRect(&rect);
//	TRACE("view rect %d\n", rect.right);
	int nListWidth = 0;

	//Make sure column wide enough
	const int iconWidth = 20;
	int nStrWidth = GetListCtrl().GetStringWidth(str);
	if (nCol == 0)	nStrWidth += iconWidth;
	int nColWidth = GetListCtrl().GetColumnWidth(nCol);

	if (nStrWidth > nColWidth)
	{//find total list Width
		int nTotWidth = 0;
		for (int i = 0; i<4; i++)
			nTotWidth = nTotWidth + GetListCtrl().GetColumnWidth(i);
		//nGrowRoom is visible width - total width
		int nGrowRoom = rect.right - nTotWidth;
		if (nGrowRoom > 0)
		{//set columnwidth to max possible while remaining visible
			GetListCtrl().SetColumnWidth(nCol, min(nStrWidth, (nColWidth + nGrowRoom)));
		}
	}

}

void CVarView::OnUpdateVariableAddtime(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable(!GetDocument()->m_strTimes.IsEmpty() && m_bEnabled);
}

void CVarView::OnUpdateEditDelete(CCmdUI* pCmdUI) 
{
	if (GetListCtrl().GetNextItem(-1, LVNI_ALL|LVNI_SELECTED)== -1)
		pCmdUI->Enable(FALSE);
	else
		pCmdUI->Enable(m_bEnabled);
	
}


void CVarView::OnLButtonDblClk(UINT nFlags, CPoint point) 
{
	if (GetListCtrl().GetNextItem(-1, LVNI_ALL|LVNI_SELECTED)!= -1)
		OnVariableModify();	
	CListViewEx::OnLButtonDblClk(nFlags, point);
}




BOOL CVarView::IsAxesDrawn()
{
	POSITION pos = m_VarList.GetTailPosition();
	while (pos != NULL){
		CDrawObj* pObj = m_VarList.GetPrev(pos);
		if (pObj->IsKindOf(RUNTIME_CLASS(CAxes)))
			return TRUE;
	}
	return FALSE;	
}

void CVarView::OnUpdateVariableAddangle(CCmdUI* pCmdUI) 
{
	int nSides = 0;
	if (!m_VarList.IsEmpty())
	{
		POSITION pos = m_VarList.GetHeadPosition();
		while (pos != NULL)
		{
			CCheckedObj* pObj = m_VarList.GetNext(pos);
			if (pObj->IsKindOf(RUNTIME_CLASS(CAxes)))
			{
				pCmdUI->Enable(m_bEnabled); // check view enabled
				return;
			}
			else if ( (!pObj->IsKindOf(RUNTIME_CLASS(CVariable))) && pObj->HasComponents())
			{
				nSides++;
			}
			if (nSides > 1)
			{
				pCmdUI->Enable(m_bEnabled); // check view enabled
				return;
			}

		}
	}

	// else don't have two things to define angle between:
	pCmdUI->Enable(FALSE);
	return;
}

void CVarView::OnVariableSolvefor() 
{
	CEQView* pView = theApp.GetEQView();
	if (pView != NULL)
	{
		int nSp = m_strVar.ReverseFind(' ');
		CString strHitVar = m_strVar.Mid(nSp+1);
		pView->SolveFor(strHitVar);
	}
	
}

void CVarView::OnUpdateVariableSolvefor(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable(HelpSystemIsConnected() && !m_strVar.IsEmpty() && m_bEnabled);
	
}

// Handle submit of "temp" copy being edited in dialog box, whose state now 
// may differ from underlying "real" obj being displayed.
// Just like InsertListItem, but here we update displayed state from TempObj, 
// while keeping RealObj as the associated object in the item data.
void CVarView::UpdateListItem(CCheckedObj * pRealObj, CCheckedObj * pTempObj)
{
	if (pTempObj->IsKindOf(RUNTIME_CLASS(CSystem)))
		return;

	// add to var list
	if (pRealObj)
	{
		if (m_VarList.Find(pRealObj) == NULL)
			m_VarList.AddTail(pRealObj);

		// for variables, update type code in case changed angular/linear in dlg
		if (pRealObj->IsKindOf(RUNTIME_CLASS(CVariable))) {
			ASSERT_KINDOF(CVariable, pTempObj);
			((CVariable*)pRealObj)->m_nType = ((CVariable*)pTempObj)->m_nType;
		}
	}

	CString strName = pTempObj->m_strName;
	CString strDef;

	if ( (pTempObj->IsKindOf(RUNTIME_CLASS(CVariable))) 
		|| (pTempObj->IsKindOf(RUNTIME_CLASS(CRadius))) 
			|| (pTempObj->IsKindOf(RUNTIME_CLASS(CAngle))) 
				/*|| (pTempObj->IsKindOf(RUNTIME_CLASS(CSystem)))*/ )
	{
		strDef = pTempObj->GetDef();
	}
	else if (pTempObj->HasComponents())//vectors only(!components)  (variables taken care of above)
	{
		CString str;
		str = pTempObj->GetDef();
		strDef.Format("magnitude of the %s", str);
	}
	else
		return;
	
	// For system "x", associated mass variable is named "mx".
/*	if (pTempObj->IsKindOf(RUNTIME_CLASS(CSystem)))
		strName = "m" + strName; */

	// Adjust column widths to fit
	SizeToFit(0, strName);
	SizeToFit(1, strDef);
	
	// Insert or modify item in list
	int index = FindIndex(pRealObj);
	if (index == -1)	//not already in list
	{
		index = GetListCtrl().GetItemCount();
		GetListCtrl().InsertItem(index, strName);
	}
	else
		GetListCtrl().SetItemText(index, 0, strName);

	GetListCtrl().SetItemText(index, 1, strDef);
	GetListCtrl().SetItemState(index, INDEXTOSTATEIMAGEMASK(pTempObj->m_status+1), LVIS_STATEIMAGEMASK);
	GetListCtrl().SetItemData(index, (DWORD)pRealObj);//set data = obj pointer

	// Update component names
	if (pTempObj->HasComponents() /* && GetDocument()->IsAxesDrawn() */)
		UpdateComponents(pTempObj);
}




