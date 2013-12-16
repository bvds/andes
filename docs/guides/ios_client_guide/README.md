# iOS Sencha Space Client Guide

The Sencha Space Client application is available from 
the Apple iTunes App Store. 

The Space Client application presents  
and quickly move between the apps in the Space Client. 

This guide describes how to use the Space Client with iOS devices. For 
use with Android devices, see the
<a href="#!/android_client_guide">Android Sencha Space Client</a> guide.

This guide assumes you have received an organization name and
password from your Space Manager administrator.

## Requirements

Sencha Space Client supports iOS version 6.0 and later.

Installing the user client requires 5 MB to download to the device,
with the installed size requiring approximately 35 MB on the device. 
Use of Space Client requires access to the Internet.

## Installing the iOS Space Client

<b>Note</b>: Even though the Space Client app is free, iTunes requires
that you have an Apple ID, which requires that you supply a credit 
card or PayPal information.

To install:
<ol>
<li>Click the App Store icon on your device:
{@img iOSAppStoreIcon.png}
</li>
<li>Search for <b>sencha space</b> - the app appears in search as follows. 
Click the cloud icon to install Space Client:
{@img iOSsearch.png}
</li>
<li>Enter your AppleID password:
{@img iOSAppIDPrompt.png}
</li>
<li>Click OK to ensure that you are 17 years old or older:
{@img iOSAgeRelatedMaterial.png}
</li>
<li>Click <b>Open</b> to launch Space Client:
{@img iOSOpenSpace.png}
</li>
</ol>

## Logging Into the Space Client

Type the name of your organization:

{@img iOS_1.png}

Supply your username and password provided by your 
Space Manager administrator, and click <b>Next</b> or click <b>return</b>.

{@img iOS_2.png}

If prompted, specify a personal identification number (PIN), which you will enter
when you return to the Space Client after exiting to use other device functions.
If a QWERTY keyboard displays, enter text consisting of at least 4 characters,
one of which must be a number, a letter, and a special character.

The minimum length of the PIN is set by your Space Manager administrator:

{@img iOS_3.png}

### Reset Your Password

To reset your password, click the question mark icon:

{@img QuestionMark.png}

Enter your email address 
and click <b>Reset Password</b> or click <b>return</b>. 

{@img ResetPassword.png}

Information about how to reset your password is sent to the email
address you provide. To return to the previous screen, click the left arrow. 

## Existing Login PIN Prompt

When you have already started a Space session and exit the client, when
you return, you may be prompted for the organization and/or for the PIN
that was previously set:

{@img SpaceClientPINprompt.png}

## Add an Organization

You can specify the name of another organization. An organization name 
can be a project name, company name, institution name, etc. 
Your Space administrator provices you with the organization name.

Click <b>Organization</b>, type the name, and click <b>Next</b> or <b>return</b>.
To return to the previous screen, click the left arrow.

{@img OrganizationPrompt.png}

## Space Client Controls

The starting controls for the Space Client are:

{@img Space_Client_TopBar.png}

<ul>
<li><b>Apps</b> - View available apps.</li>
<li><b>Controls</b>:
{@img Controls.png}
	<ul><ul>
		<ul>
		<li>Star - Add app to Favorites</li>
		<li>Left Arrow - Previous app view</li>
		<li>Windows - Show reduced sub window view of app</li>
		<li>Right Arrow - Next app view</li>
		<li>Round Arrow - Refresh app view</li>
		</ul>
	</ul></ul>
</li>
</ul>

The reduced sub window view appears as:

{@img SubWindow.png}


## Viewing Apps

The list of apps appears as follows (your list of apps may differ):

{@img Space_Client_AppsList.png}

<ol>
<li>Click the app icon to view the app</li>
<li>Click the Sencha icon to view <a href="#Settings">Settings</a></li>
<li>Click <b>Switch Organizations</b> to log in with a new organization name</li>
</ol>

## Refreshing Apps

To get the latest versions of the apps from the app server, navigate to the 
app list and swipe down to refresh the apps.

{@img ClientIOSappRefresh.png}

<a name="Settings"></a>
## Settings 

Sencha Space provides the following settings:

<ol>
<li><b>Close</b> - Return to previous screen</li>
<li><b>Change PIN</b> - Specify your current PIN, a new PIN, and confirm the new PIN</li>
<li><b>Remove Organization</b> - Deletes all data you may have 
stored while using the current 
organization and prompts you to log in again with a new organization name. This
prompt displays a confirmation prompt to be sure you want to delete all app
data:
{@img RemoveOrganizationAlert.png}
</li>
<li><b>Legal</b> - View Space attribution copyright 
information and terms of service</li>
</ol>

{@img SpClientSettings.png}

## Change PIN

Specify your PIN and new PIN, and click <b>Next</b>. 
Confirm the new PIN and click <b>Done</b>.

{@img ChangePINscreens.png}




