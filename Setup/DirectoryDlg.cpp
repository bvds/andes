// DirectoryDlg.cpp : implementation file
//

#include "stdafx.h"
#include "setup.h"
#include "InstDirPg.h"
#include "DirectoryDlg.h"


#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CDirectoryDlg dialog


CDirectoryDlg::CDirectoryDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CDirectoryDlg::IDD, pParent)
{
	m_pParent = (CInstDirPg*)pParent;
	//{{AFX_DATA_INIT(CDirectoryDlg)
	m_strPath = _T("");
	//}}AFX_DATA_INIT
}


void CDirectoryDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CDirectoryDlg)
	DDX_Control(pDX, IDC_INSTALL_PATH, m_ctrlPath);
	DDX_Control(pDX, IDC_DIRECTORY_TREE, m_ctrlDirTree);
	DDX_Text(pDX, IDC_INSTALL_PATH, m_strPath);
	DDV_MaxChars(pDX, m_strPath, 255);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CDirectoryDlg, CDialog)
	//{{AFX_MSG_MAP(CDirectoryDlg)
	ON_NOTIFY(TVN_ITEMEXPANDING, IDC_DIRECTORY_TREE, OnItemexpandingDirectoryTree)
	ON_NOTIFY(TVN_SELCHANGED, IDC_DIRECTORY_TREE, OnSelchangedDirectoryTree)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CDirectoryDlg message handlers

BOOL CDirectoryDlg::OnInitDialog() 
{
	CDialog::OnInitDialog();
	
	m_imgDrives.Create(IDR_DRIVES, 16, 1, RGB(255, 0, 255));
	m_ctrlDirTree.SetImageList(&m_imgDrives, TVSIL_NORMAL);
	InitDirTree();
	InitSelection(FALSE);
	m_ctrlPath.SetWindowText(m_strPath);

	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

int CDirectoryDlg::InitDirTree()
{
	int nPos = 0;
	int nDrivesAdded = 0;
	CString strDrive = "?:\\";

	DWORD dwDriveList = ::GetLogicalDrives();

	while (dwDriveList){
		if (dwDriveList & 1){
			strDrive.SetAt(0, 0x41 + nPos);
			if (AddDriveNode (strDrive))
				nDrivesAdded++;
		}
		dwDriveList >>= 1;
		nPos++;
	}
	return nDrivesAdded;
}


BOOL CDirectoryDlg::AddDriveNode(CString& strDrive)
{
	CString string;
    HTREEITEM hItem;
    static BOOL bFirst = TRUE;

    UINT nType = ::GetDriveType ((LPCTSTR) strDrive);

    switch (nType) {

    case DRIVE_REMOVABLE:
        hItem = m_ctrlDirTree.InsertItem (strDrive, 1,
            1);
        m_ctrlDirTree.InsertItem ("", 3,
            3, hItem);
        break;

    case DRIVE_FIXED:
        hItem = m_ctrlDirTree.InsertItem (strDrive, 0,
            0);
        SetButtonState (hItem, strDrive);

        // If this is the first fixed disk, select and expand it
        if (bFirst) {
            m_ctrlDirTree.SelectItem (hItem);
            m_ctrlDirTree.Expand (hItem, TVE_EXPAND);
            bFirst = FALSE;
        }
        break;

    case DRIVE_REMOTE:
        hItem = m_ctrlDirTree.InsertItem (strDrive, 0,
            0);
        SetButtonState (hItem, strDrive);
        break;

    case DRIVE_CDROM:
        hItem = m_ctrlDirTree.InsertItem (strDrive, 2,
            2);
        m_ctrlDirTree.InsertItem ("", 3,
            3, hItem);
        break;

    case DRIVE_RAMDISK:
        hItem = m_ctrlDirTree.InsertItem (strDrive, 0,
            0);
        SetButtonState (hItem, strDrive);
        break;

    default:
        return FALSE;
    }

    return TRUE;
}

BOOL CDirectoryDlg::SetButtonState (HTREEITEM hItem, CString& strPath)
{
    HANDLE hFind;
    WIN32_FIND_DATA fd;
    BOOL bResult = FALSE;

    CString string = strPath;
    if (string.Right (1) != "\\")
        string += "\\";
    string += "*.*";

    if ((hFind = ::FindFirstFile ((LPCTSTR) string, &fd)) ==
        INVALID_HANDLE_VALUE)
	{
		DWORD e = GetLastError();

        return bResult;
	}

    do {
        if (fd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) {
            CString strCmp = (LPCTSTR) &fd.cFileName;
            if ((strCmp != ".") && (strCmp != "..")) {
                m_ctrlDirTree.InsertItem ("", 3,
                    3, hItem);
                bResult = TRUE;
                break;
            }
        }
    } while (::FindNextFile (hFind, &fd));
	
	::FindClose (hFind);
    return bResult;
}

CString CDirectoryDlg::GetPathFromNode (HTREEITEM hItem)
{
    CString strResult = m_ctrlDirTree.GetItemText (hItem);

    HTREEITEM hParent;
    while ((hParent = m_ctrlDirTree.GetParentItem (hItem)) != NULL) {
        CString string = m_ctrlDirTree.GetItemText (hParent);
        if (string.Right (1) != "\\")
            string += "\\";
        strResult = string + strResult;
        hItem = hParent;
    }
    return strResult;
}

int CDirectoryDlg::AddDirectories (HTREEITEM hItem, CString& strPath)
{
    HANDLE hFind;
    WIN32_FIND_DATA fd;
    HTREEITEM hNewItem;

    int nCount = 0;

    CString string = strPath;
    if (string.Right (1) != "\\")
        string += "\\";
    string += "*.*";
	//find each file through wildcard search
    if ((hFind = ::FindFirstFile ((LPCTSTR) string, &fd)) ==
        INVALID_HANDLE_VALUE) {
        if (m_ctrlDirTree.GetParentItem (hItem) == NULL)
            m_ctrlDirTree.InsertItem ("", 3,
                3, hItem);
        return 0;
    }

    do {
        if (fd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) {
            CString strCmp = (LPCTSTR) &fd.cFileName;
            if ((strCmp != ".") && (strCmp != "..")) {
                hNewItem =
                    m_ctrlDirTree.InsertItem ((LPCTSTR) &fd.cFileName,
                    3, 4, hItem);

                CString strNewPath = strPath;
                if (strNewPath.Right (1) != "\\")
                    strNewPath += "\\";

                strNewPath += (LPCTSTR) &fd.cFileName;
                SetButtonState (hNewItem, strNewPath);
                nCount++;
            }
        }
    } while (::FindNextFile (hFind, &fd));

    ::FindClose (hFind);
	//Sort children as we go so whole directory tree in abc order
	m_ctrlDirTree.SortChildren(hItem);
    return nCount;
}

void CDirectoryDlg::DeleteFirstChild (HTREEITEM hParent)
{
    HTREEITEM hItem;
    if ((hItem = m_ctrlDirTree.GetChildItem (hParent)) != NULL)
        m_ctrlDirTree.DeleteItem (hItem);
}

void CDirectoryDlg::DeleteAllChildren (HTREEITEM hParent)
{
    HTREEITEM hItem;
    if ((hItem = m_ctrlDirTree.GetChildItem (hParent)) == NULL)
        return;

    do {
        HTREEITEM hNextItem = m_ctrlDirTree.GetNextSiblingItem (hItem);
        m_ctrlDirTree.DeleteItem (hItem);
        hItem = hNextItem;
    } while (hItem != NULL);
}

void CDirectoryDlg::OnItemexpandingDirectoryTree(NMHDR* pNMHDR, LRESULT* pResult) 
{
	NM_TREEVIEW* pNMTreeView = (NM_TREEVIEW*)pNMHDR;
	HTREEITEM hItem = pNMTreeView->itemNew.hItem;
	CString string = GetPathFromNode(hItem);
		
	*pResult = 0;

	if (pNMTreeView->action == TVE_EXPAND){
		DeleteFirstChild(hItem);
		//Directories added as needed
		if (AddDirectories(hItem, string) == 0)
			*pResult = 1;
	}
	else{//collapsing tree
		//remove children if can't see them
		DeleteAllChildren(hItem);
		if (m_ctrlDirTree.GetParentItem(hItem) == NULL)
			m_ctrlDirTree.InsertItem("", 3, 4, hItem);
		else
			SetButtonState(hItem, string);
	}

}

void CDirectoryDlg::OnSelchangedDirectoryTree(NMHDR* pNMHDR, LRESULT* pResult) 
{
	NM_TREEVIEW* pNMTreeView = (NM_TREEVIEW*)pNMHDR;
	//Make sure text in edit window reflects chosen nodes
	CString strPath = GetPathFromNode(pNMTreeView->itemNew.hItem);
	m_ctrlPath.SetWindowText(strPath);
	*pResult = 0;
}

BOOL CDirectoryDlg::InitSelection(BOOL bCheck)
{
	//First time called, we are not checking directory & drive
	//(bCheck = FALSE); we know that the default drive can be written 
	//to and that the default directory will need to be created
	CString strPath = m_strPath;
	
	CString strDrive, strTestDir;
	int strlen = strPath.GetLength();
	if (strPath[strlen-1] != '\\')
		strPath = strPath + "\\";//enforce ending backslash
	int sep;
	sep = strPath.Find("\\");
	strDrive = strPath.Left(sep + 1);//c:\  includes backslash
	
	HTREEITEM hItem = m_ctrlDirTree.GetRootItem();
	hItem = ExpandNode(strDrive, hItem);//try to expand that drive
	
	if (bCheck ){//user okayed choice, we need to check their selection
		//hItem = NULL, Not a valid drive ->CDRom's are Read-only
		if ((hItem == NULL) || (GetDriveType(strDrive)==DRIVE_CDROM)){
			CString msg;
			msg.Format(IDS_INVALID_DRIVE, strDrive);
			AfxMessageBox(msg);
			return FALSE;
		}
		else if (strcmp(strDrive, strPath)==0){//to see if we have write 
		//access to chosen drive root directory, try to create test directory
		//if this function fails, we don't have write access to this root;
			CString strTestDir = strPath + "emd";//try creating "emd" dir
			BOOL bCD = CreateDirectory(strTestDir, NULL);
			int i = 0;
			if (!bCD){//directory could not be created
				//check if by some chance, already exists
				while ( GetLastError() == ERROR_ALREADY_EXISTS){
					CString str;
					str.Format("emd%d", i);
					strTestDir = strPath + str;
					i++;
					SetLastError(0);//reset last error
					//try creating emd0, emd1, emd2 etc.
					bCD = CreateDirectory(strTestDir, NULL);
				}
				if (!bCD){//it couldn't be created for some other reason
					CString msg;
					msg.Format(IDS_INVALID_DRIVE, strDrive);
					//Notify user of invalid drive
					AfxMessageBox(msg);
					return FALSE;
				}
			}
			RemoveDirectory(strTestDir); //remove this unneccessary directory
			return TRUE;
		}
	}
	CString dirPath = strPath.Left(sep);
	CString strRest = strPath.Right(strlen - sep);//strlen = length of 
						//string before the last backslash was added
	sep = strRest.Find("\\");
	while (sep != -1){
		CString strDir = strRest.Left(sep);
		hItem = m_ctrlDirTree.GetChildItem(hItem);
		//Chosen drive/directory initially selected
		hItem = ExpandNode(strDir, hItem);
		dirPath = dirPath + "\\" + strDir;
		if (bCheck && (hItem == NULL)){
			CString msg;
			//Warn user that the directory they have selected does not
			//exist and will need to be created
			msg.Format(IDS_NOSUCH_DIR, m_strPath);
			if (AfxMessageBox(msg, MB_OKCANCEL)==IDOK){
				while (sep != -1){
					//create directory/subdirectories
					if (!CreateDirectory(dirPath, NULL)){
						CString msg;
						msg.Format(IDS_DIR_FAIL, m_strPath);
						AfxMessageBox(msg);
						return FALSE;
					}
					strlen = strRest.GetLength();
					strRest = strRest.Right(strlen - sep - 1);
					sep = strRest.Find("\\");
					strDir = strRest.Left(sep);
					dirPath = dirPath + "\\" + strDir;

				}
				return TRUE;
			}
				
			else
				return FALSE;
		}
		strlen = strRest.GetLength();
		strRest = strRest.Right(strlen - sep - 1);
		sep = strRest.Find("\\");
	}
	
	return TRUE;

}

HTREEITEM CDirectoryDlg::ExpandNode(CString strDir, HTREEITEM hItem)
{
	//expand nodes corresponding to 
	//selected installation directories
	while (hItem != NULL){
		CString str = m_ctrlDirTree.GetItemText(hItem);
		str.MakeLower();
		strDir.MakeLower();
		if (str == strDir){
			m_ctrlDirTree.SelectItem(hItem);
			m_ctrlDirTree.Expand(hItem, TVE_EXPAND);
			return hItem;
		}
		hItem = m_ctrlDirTree.GetNextSiblingItem(hItem);
	}
	return NULL;
}



void CDirectoryDlg::OnOK() 
{
	m_ctrlPath.GetWindowText(m_strPath);
	if (!InitSelection(TRUE))//Check if valid installation direcotyr
		return;
	m_pParent->m_strInstDir = m_strPath;
	CDialog::OnOK();
}

