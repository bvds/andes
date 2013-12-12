# Android Sencha Space Client Guide

The Sencha Space Client application is available from 
the <a href="http://play.google.com">Google Play Store</a>. 

The Space Client application enables you to view available apps 
and quickly move between the apps in the Space Client. 

This guide describes how to use the Space Client with Android devices. 
For use with iOS devices, see the
<a href="#!/ios_client_guide">iOS Sencha Space Client</a> guide.

This guide assumes you have received an organization name and
password from your Space Manager administrator.

## Requirements

The Sencha Space Client supports Android version 2.3.3 or later.

Installing the user client requires 5 MB to download to the device,
with the installed size requiring approximately 35 MB on the device. 
Use of Space Client requires access to the Internet.

## Installing Sencha Space Client for Android

To install Sencha Space Client for Android:
<ol>
<li>Click the <b>Play Store</b> app on your Android device or browse to
the <a href="http://play.google.com">Google Play Store</a>:
{@img AndroidPlayStoreIcon.png}
</li>
<li>Search for <code>sencha space</code> - the app appears in search as:
{@img SenchaSpaceInPSsearch.png}
</li>
<li>Tap the icon to open. Click <b>Install</b>:
{@img PlayStoreListing.png}
</li>
</ol>

## Logging Into the Space Client

The prompts you see to log in depend on whether this is your first log in
or if you are returning after exiting the app.

### First Log In

<ol>
<li>Click the Android <b>Apps</b> button and click <b>Space</b>:
{@img AppsToSpace.png}</li>
<li>Type your organization name and click <b>Next</b>:
{@img Android1.png}
</li>
<li>Type your email address and password, and click <b>Log In</b>.<br> 
If you don't know your password, click <b>Forgot password?</b>:
{@img Android2.png}
</li>
<li>If a PIN prompt displays, type your PIN, confirm it, 
and click <b>Continue</b>. The PIN type
is set by the Space Manager Administrator:
<ul>
<li>If you see a number keypad, your PIN must 
be at least 4 digits in length.</li>
<li>If you're prompted with a full QWERTY keyboard, the PIN is
set so that you enter a text string at least 4 characters in length 
and must contain at least
one number, a letter, and a special character.</li>
{@img Android3.png}
</li>
</ol>

### Returning Log In

If you exit and return in less than 5 minutes, there's no prompt. (The
length of time is set by your Space Manager administrator.)

If you were asked to create a PIN when you first logged in, when you return,
the Space Client prompts you to enter your PIN:

{@img AndroidReturn1.png}


### Reset Your Password

If you don't remember your password, click <b>Forgot password?</b>.

Enter your email address 
and click <b>Reset Password</b> or click <b>return</b>. 
Information about how to reset your password is sent to the email
address you provide. To return to the previous screen, click the left arrow. 

{@img AndroidResetPassword.png}


## Add an Organization

The name of your organization, such as a project name, company name, 
institution name, etc. 
An organization name is provided to you when Sencha sends you mail confirming your 
participation in Sencha Space. 

Click the <b>Organization</b>, type the name, and click <b>Next</b> or <b>return</b>.
To return to the previous screen, click the left arrow.

{@img OrganizationPrompt.png}

## Using Sencha Space

The starting controls for the Sencha Space User Client application are:

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
Confirm the new PIN and click <b>Continue</b>.

{@img ChangePINscreens.png}




