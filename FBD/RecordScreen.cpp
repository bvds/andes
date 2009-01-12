// Include libraries.
#include "stdafx.h"
#include <windows.h>
#include <atlbase.h>
#include <comdef.h>
#include "C:\WMSDK\WMEncSDK9\include\wmencode.h"

// Define the screen capture properties.
#define WMSCRNCAP_ENTIRESCREEN      CComBSTR("Screen")
/*
#define WMSCRNCAP_CAPTUREWINDOW     CComBSTR("CaptureWindow")
#define WMSCRNCAP_WINDOWLEFT        CComBSTR("Left")
#define WMSCRNCAP_WINDOWTOP         CComBSTR("Top")
#define WMSCRNCAP_WINDOWRIGHT       CComBSTR("Right")
#define WMSCRNCAP_WINDOWBOTTOM      CComBSTR("Bottom")
#define WMSCRNCAP_FLASHRECT         CComBSTR("FlashRect")
#define WMSCRNCAP_WINDOWTITLE       CComBSTR("WindowTitle")
*/

// Declare variables.
static IWMEncoderApp* pEncoderApp = NULL;
static IWMEncoder* pEncoder = NULL;
static IWMEncSourceGroupCollection* pSrcGrpColl = NULL;
static IWMEncSourceGroup* pSrcGrp = NULL;
static IWMEncSource* pSrc = NULL;
static IWMEncVideoSource2* pSrcVid = NULL;
static IWMEncSource* pSrc2 = NULL;
static IWMEncAudioSource* pSrcAud = NULL;
static IPropertyBag* pPropertyBag = NULL;
static CComVariant varValue;
static IWMEncProfileCollection* pProColl = NULL;
static IWMEncProfile* pPro = NULL;
static IWMEncFile* pFile = NULL;

BOOL BeginRecordScreen(LPCSTR pszOutputPath, LPCTSTR pszProfileName, LPCTSTR pszProfileDir=NULL)
{
	HRESULT hr = S_OK;
	// Initialize the COM library and retrieve a pointer to an IWMEncoder interface.
	hr = CoInitialize(NULL);
/*
	if ( SUCCEEDED( hr ) )
	{
		hr = CoCreateInstance(CLSID_WMEncoder,
			NULL,
			CLSCTX_INPROC_SERVER,
			IID_IWMEncoder,
			(void**) &pEncoder);
	}
*/
	// Create the Encoder App object
	if ( SUCCEEDED( hr ) )
	{
		hr = CoCreateInstance( CLSID_WMEncoderApp,
							   NULL,
							   CLSCTX_LOCAL_SERVER,
							   IID_IWMEncoderApp,
							   (void**) (&pEncoderApp));
	}
	// Display the predefined user interface (UI).
	//if ( SUCCEEDED( hr ) )
	//{
	//	hr = pEncoderApp->put_Visible(VARIANT_TRUE);
	//}

	// Acquire an IWMEncoder interface.
	if ( SUCCEEDED( hr ) )
	{
		hr = pEncoderApp->get_Encoder(&pEncoder);
	}


	// Retrieve the source group collection.
	if ( SUCCEEDED( hr ) )
	{
		hr = pEncoder->get_SourceGroupCollection(&pSrcGrpColl);
	}

	// Add a source group to the collection.
	if ( SUCCEEDED( hr ) )
	{
		hr = pSrcGrpColl->Add(CComBSTR("SG_1"), &pSrcGrp);
	}

	// Add a video source to the source group.
	if ( SUCCEEDED( hr ) )
	{
		hr = pSrcGrp->AddSource(WMENC_VIDEO, &pSrc);
	}
	if ( SUCCEEDED( hr ) )
	{
		hr = pSrc->QueryInterface(IID_IWMEncVideoSource2, (void**)&pSrcVid);
	}
	if ( SUCCEEDED( hr ) )
	{
		hr = pSrcVid->SetInput(CComBSTR("ScreenCap://ScreenCapture1"));
	}

	// Retrieve a pointer to the property bag.
	if ( SUCCEEDED( hr ) )
	{
		hr = pSrcVid->QueryInterface(IID_IPropertyBag, (void**)&pPropertyBag);
	}
	// Set full screen capture.
	{
		varValue = true;
		if ( SUCCEEDED( hr ) )
		{
			hr = pPropertyBag->Write( WMSCRNCAP_ENTIRESCREEN, &varValue );
		}
	}

	// Add Audio Source to the source group
	if ( SUCCEEDED( hr ) )
	{
		hr = pSrcGrp->AddSource(WMENC_AUDIO, &pSrc2);
	}
	if ( SUCCEEDED( hr ) )
	{
		hr = pSrc2->QueryInterface(IID_IWMEncAudioSource, (void**)&pSrcAud);
	}
	if ( SUCCEEDED( hr ) )
	{
		hr = pSrcAud->SetInput(CComBSTR("Device://Default_Audio_Device"));
	}

	// Find specified profile and set it into source group.
	{
		long lCount = 0;
		int i;

		if ( SUCCEEDED( hr ) )
		{
			hr = pEncoder->get_ProfileCollection(&pProColl);
		}
		// set custom profile directory if specified, and refresh list from there
		if (SUCCEEDED( hr ) && pszProfileDir ) {
			hr = pProColl->put_ProfileDirectory(CComBSTR(pszProfileDir));
			if (SUCCEEDED( hr )) {
				hr = pProColl->Refresh();
			}
			else TRACE("Couldn't set WME custom profile directory\n");
		}

		// Loop through the profiles and find the one we want
		if ( SUCCEEDED( hr ) )
		{
			hr = pProColl->get_Count(&lCount);
		}
		if ( SUCCEEDED( hr ) ) 
		{
			 for (i=0; i<lCount; i++)
			{
				CComBSTR bstrName = NULL;
				if ( SUCCEEDED( hr ) )
				{
					hr = pProColl->Item(i, &pPro);
				}
				if ( SUCCEEDED( hr ) )
				{
					hr = pPro->get_Name(&bstrName);
				}
				if ( SUCCEEDED( hr ) ) {
				   TRACE("Checking profile %s\n", CString(bstrName));
				   if (_wcsicmp(bstrName, CComBSTR(pszProfileName))==0) {
				   		TRACE("Found desired profile\n");
						hr = pSrcGrp->put_Profile(CComVariant(pPro));
						break;
				   }
				}   
			 }
		}
	} // end set profile


	// Specify a file in which to save encoded content.
	if ( SUCCEEDED( hr ) )
	{
		hr = pEncoder->get_File(&pFile);
	}
	if ( SUCCEEDED( hr ) )
	{
		hr = pFile->put_LocalFileName(CComBSTR(pszOutputPath));
	}

	// Start the encoder
	if ( SUCCEEDED( hr ) )
    {
        hr = pEncoder->PrepareToEncode(VARIANT_TRUE);
    }
	if ( SUCCEEDED( hr ) )
    {
        hr = pEncoder->Start();
	}
	// return success or failure
	return SUCCEEDED(hr);
}

void EndRecordScreen()
{
	if (pEncoder)
		pEncoder->Stop();

		// Release pointers.
		if ( pFile )
		{
			pFile->Release();
			pFile = NULL;
		}	
		if ( pProColl )
		{
			pProColl->Release();
			pProColl = NULL;
		}
		if ( pPro )
		{
			pPro->Release();
			pPro = NULL;
		}

		if ( pSrcGrpColl )
		{
			pSrcGrpColl->Release();
			pSrcGrpColl = NULL;
		}
		if ( pSrcGrp )
		{
			pSrcGrp->Release();
			pSrcGrp = NULL;
		}
		if ( pSrcVid )
		{
			pSrcVid->Release();
			pSrcVid = NULL;
		}
		if ( pSrc )
		{
			pSrc->Release();
			pSrc = NULL;
		}
		if ( pSrcAud )
		{
			pSrcAud->Release();
			pSrcAud = NULL;
		}
		if ( pSrc2 )
		{
			pSrc2->Release();
			pSrc2 = NULL;
		}
		if ( pPropertyBag )
		{
			pPropertyBag->Release();
			pPropertyBag = NULL;
		}
		if ( pEncoder )
		{
			pEncoder->Release();
			pEncoder = NULL;
		}
		if ( pEncoderApp )
		{
			pEncoderApp->Release();
			pEncoderApp = NULL;
		}
}
