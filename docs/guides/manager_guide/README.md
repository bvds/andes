# Sencha Space Manager Guide 

The Sencha Space Manager application lets you manage user and group
access to web apps that run on a Sencha Space Client. Sencha hosts the Space
Manager app at
[http://manage.space.sencha.com](http://manage.space.sencha.com).

The Space Manager application enables secure access to an enterprise's
authentication and authorization server. 

<b>Space Manager Browser Support</b>

The Manager application works on:

<ul>
<li>Chrome (any version)</li>
<li>Safari (any version)</li>
<li>Firefox version 22 and later</li>
<li>Internet Explorer version 10 and later</li>
</ul>

<b>Client Support</b>

Users obtain their Sencha Space Client apps from the Apple App Store, 
Google Play, or BlackBerry World Apps market.


## Sencha Space Features

The Manager Application side menu contains these buttons:

<table border="1" style="width: 600px">
<tr><th>Icon</th><th>Description</th></tr>
<tr><td>{@img IconHome.png}</td>
  <td><a href="#OverviewTab">Overview</a> - Monitors organization Space usage and user 
  device and location</td></tr>
<tr><td>{@img IconApplications.png}</td>
  <td><a href="#AppsTab">Applications</a> - Lists, adds, and
removes applications</td></tr>
<tr><td>{@img IconDevices.png}</td><td><a href="#DevicesTab">Devices</a> - Lists,
assigns device ownership, blocks a device, and wipes Space Client info 
from a device</td></tr>
<tr><td>{@img IconUsers.png}</td>
  <td><a href="#UsersTab">Users</a> - Lists, adds, and removes user access to Space</td></tr>
<tr><td>{@img IconGroups.png}</td>
  <td><a href="#GroupsTab">Groups</a> - Lists, adds, and removes group access to Space</td></tr>
<tr><td>{@img IconVPN.png}</td>
  <td><a href="#VPNServicesTab">VPN Services</a> - Manages VPN services</td></tr>
<tr><td style="vertical-align: top">{@img IconReports.png}</td>
<td><a href="#ReportsTab">Reports</a> - Provides interactive reports for:
<ul>
<li>Last Seen - Number of users logging in. Lets you filter by duration, 
date range, platform OS, platform version, and whether the device is 
owned by the user or the organization. Also lists recent logins.</li>
<li>Platform Distribution - Pie chart by device platform with filters for 
date range, platform OS, platform version, and whether the device is owned 
by the user or the organization. Also lists current devices.</li>
<li>App Launches - Number of app launches with filters for 
date range, platform OS, platform version, and whether the device is owned 
by the user or the organization. Also lists the available apps by usage,
with columns for when the app was created (made available to your organization).</li>
</ul>
</td></tr>
<tr><td style="vertical-align: top">{@img IconSettings.png}</td>
  <td><a href="#SettingsTab">Settings</a> - Provides settings for:
<ul>
<li>General - Specify organization name, organization code, email method, and billing address.</li>
<li>Security Policy - Specify user session duration, PIN length, 
and maximum attempts before lock out.</li>
<li>Authentication - Specify authentication method, Security Assertion Markup Language (SAML) 
configuration, and Identity Provider (IDP) information.</li>
<li>Subscription - Change your Space subscription when your organization 
adds more users, devices per user, or apps.</li>
<li>Billing - Specify your organization's contact and billing address.</li>
</ul>
</td></tr>
</table>

The top menu contains these buttons:

<table border="1" style="width: 600px">
<tr><th>Icon</th><th>Description</th></tr>
<tr><td>{@img Space_Ad_AdminMenu.png}</td>
<td><a href="#AdminMenu">Admin Menu</a> - Edit the admin profile 
or log out of Sencha Space</td></tr>
<tr><td>{@img Space_Ad_SenchaIcon.png}</td>
<td><a href="#SenchaLogMenu">Sencha Log Menu</a> - View a log of your recent
actions.</td></tr>
</table>

<p><b>Buttons</b>:</p>

These buttons provide additional controls on tabs:

<table border="1" style="width: 600px">
<tr><th>Icon</th><th>Description</th></tr>
<tr><td>{@img Space_Ad_PlusIcon.png}</td>
<td>Plus Icon - Add an entry</td></tr>
<tr><td valign="top">{@img Space_Ad_CircleArrowIcon.png}<br>Or:<br>{@img ButtonRefreshReport.png}</td>
<td>Refresh - Refreshes the current screen</td></tr>
<tr><td>{@img Space_Ad_TrashCanIcon.png}</td>
<td valign="top">Trash Can - Click the checkbox for an item and click the trash can
button. The result differs by the Space Manager tab:
<ul>
<li>Applications tab - Removes an application from use by all users.</li>
<li>Devices tab - Wipes the device, which removes all Sencha
Space information from the mobile device.</li>
<li>Users tab - Removes a user from Sencha Space.</li>
<li>Groups tab - Removes a group (does not delete the users in a group).</li>
<li>VPN Services tab - Removes a VPN service.</li>
</ul></td></tr>
<tr><td>{@img ButtonSearch.png}</td><td>Search current tab.</td></tr>
</table>


<a name="OverviewTab"></a>
## Overview

The Overview tab lets you monitor system usage. 
To scroll the list of devices, click in the 
list and drag your mouse up or down. You can refresh 
your view of the screen by clicking the list and dragging
the list downward.

The chart shows user activity for the current month:

{@img Space_Ad_Overview.png}

This tab lists:

 - Created users - Number of Space users who can participate
 - Active users - Number of Space users currently viewing Sencha Space
 - Opens - Indicates when a user starts a Space-managed app
 - Apps - Number of web apps available for use on Sencha Space Clients
 - Users - Number of created users
 - Groups - Number of enterprise groups
 - Active Devices<br>Lists:
   - Device. The device running Sencha Space Client and whether the device 
     is simulated or actual. 
   - User. User's email address and the Sencha Space Client version.
   - Last Access. When the user last accessed the Sencha Space Client and the user's location. 

## Disable Access to a Lost Device

If a user loses a mobile device, you can disable Space access to the device
by clicking the red X icon: 

{@img Space_Ad_BlockUserButton.png}

You are prompted to be sure you want to immediately terminate the user's 
session.

Blocking a user's device sends this message to the Space Client:

<pre><b>Error</b> This device has been blocked indefinitely by an administrator.</pre>

To reactivate the device:
<ol>
<li>text</li>
<li>text</li>
<li>text</li>
</ol>
 (1) and 
click the <b>Devices</b> tab (2). Click the blue check icon (3):

{@img SpaceMgrAddBackDevice.png}

Space Manager prompts to be sure you want to unblock the device.

<b>Note</b> Blocking the device only affects the one device. The user
can access the Space Client from another device.


<a name="AppsTab"></a>
## Applications Tab

The Applications tab lists all apps that can run on a Sencha Space Client. Click an
application to [view more information](#appinfo) about the application. 
You can give each user a different set of applications within your organization. 

The first application you set for a user 
becomes the first screen they see when they log into the Space Client. As administrator,
you can use a web page as the starting application, for example, as a welcome screen
with ways for users to get help or learn how to use their client.

{@img SpaceMgrApplications.png}

The Applications tab lists:

 - Controls - Add an application, refresh the list, delete an application, and search applications.
 - Name - Application name
 - Members - Number of users who are permitted to use the application
 - Modified - Last modification date of the application

<a name="appinfo"></a>
### Application Information Menu

View the HTML web applications that can run on a Sencha Space Client app. 
The web applications can be served using HTTP or HTTPS. 

{@img SpaceMgrAppInfo.png}

Info tab:

Lists application information:

 - URL - Where the application is served from
 - Description - Application's description

Groups tab:

Lists group information about the application:

 - Name - Groups for which this application is assigned 
 - Members - Members in the group who use the application
 - Modified - When the application was last modified

#### Add an App

The Add an App menu lets you bring a web app into the Space environment
so that the app can be accessed by Space Clients. 

{@img SpaceMgrAddApp.png}

 - App Name - Specify the name of the app as it will appear on the 
   Applications tab and on the Space Client.
 - App URL - The URL of the app. This can be an HTTP or HTTPS address.
 - Icon URL - The URL of the icon for the app. This can be an HTTP or HTTPS address.
 - Invokes - List the apps that this app can invoke. Separate app names with commas. 
 This field is case sensitive.
 - Description - Description of the app. This description appears on the Applications 
   tab and in the Space Client.

<a name="DevicesTab"></a>
## Devices Tab

Lists, assigns device ownership, blocks a device, and wipes Space Client info 
from a device.

{@img DevicesTab.png}

Tasks:

 - Click the Refresh arrow to update the devices list.
 - Click a checkbox and the trash can icon to wipe out all Space Client data on a device.
 - Search for a device.

Lists:

 - Device - Device name
 - User - Device's owner - This name is set on the <a href="#UsersTab">Users tab</a>
 - Last Access - Last use of Space Client by the device
 - Ownership - How you designate the device - can by Unknown, Employee, or Corporate

### Device Info

Click a device to view and edit device information. This menu also lets you
wipe the Space Client organization data from a device.

{@img DeviceInfo.png}

 - Device - Device type taken from the device's data provided in the connection
 - Client - Space Client version number
 - Ownership - How you classify the device - set this value by clicking **Edit**
 - Blocked - Whether or not this device is currently blocked

### Edit Device Info

Edit device information to change the ownership, block a device, or wipe the device 
by removing the organizational data from the Space Client on the device.

{@img DeviceInfoEdit.png}

 - Device - Device type taken from the device's data provided in the connection.
 - Client - Space Client version number.
 - Ownership - Set as needed to Employee or Corporate.
 - Block - Removes the ability of the device of a user to access the Space Client. The user is sent a message stating that the device is blocked by an administrator.
 - Wipe Device - Removes all Sencha Client organizational data from the device - the user is logged out of the Space Client.
 - Done - If you make a change, click **Done** to complete the change.

<a name="UsersTab"></a>
## Users Tab

Lists which users can log into the Space Client and run applications.
You can also use this tab to add users, block users, change a login password, 
and remove a user. 

{@img SpaceMgrUsers.png}
 
Tasks:

 - Click a user name to view more information about the user. 
 - Click the plus icon to invite a user. 
 - Click a checkbox and the trash icon to delete a user.
 - Search for a user or user information.

Lists:

 - Name - User name
 - Last Active - When the user last used the Space Client

### User Information Menu

Click a user in the Users tab to view information.

{@img SpaceMgrUserInfo.png}

Info tab:

 - Title - User's title in the enterprise
 - Email - User's email address
 - Apps Assigned - Which web apps the user can view on the Space Client

Groups tab:

 - Name - Groups to which the user belongs
 - Members - Users in the group
 - Modified - When the group was last modified

Devices tab:

- Info - Device name and Space Client version
- Type - Device type and device's operating system version
- Last Access - When user last used Space Client on their device, and the user's location

### Invite Users

Click the plus icon on the Users tab to invite a user to participate
in Sencha Space. You can add additional users from the Invite Users
menu, specifying each address separately.

The users you invite receive an email with information about 
how to obtain the Client app and how to log into the Sencha Space Client.

{@img Space_Ad_Users_PlusIcon.png}

To invite a user:

1. Type the user's email address and click the plus symbol. 
   {@img Space_Ad_Invite_Users.png}
2. If needed, delete an address and re-type.
3. Add each address you want to invite and click the plus symbol.
3. Click **Invite**.

You can invite a user more than once if needed. 

### Remove a User

To remove a user:

1. Click the Users tab in Space Manager.
2. Click the checkbox next to the user's name in the Users tab list.
3. Click the trash can icon at the top of the screen.

Or:

1. Click the Users tab in Space Manager.
2. Click the user in the list.
3. Click **Edit**
4. Click **Remove User** at the bottom of the page.


<a name="GroupsTab"></a>
## Groups Tab

Groups lets you organize users into organizational units, such as 
support, accounting, management, development, and so on, and assign each 
group specific apps for their exclusive use. 

The Groups tab creates or removes a group. Applications can be associated with a group
or a user. Click a checkbox and the trash icon to delete a group.

{@img SpaceMgrGroupsTab.png}

The Groups tab displays:

 - Name - Group name
 - Members - Number of users in a group
 - Modified - When the group was last modified
 - Plus icon - Add a new group

### Adding a New Group

{@img SpaceMgrCreateGroup.png}

 To add a new group:

 1. Click the Plus icon on the Groups tab
 2. Specify a group name
 3. Specify a description of the group


<a name="VPNServicesTab"></a>
## VPN Services Tab

The VPN Services tab lets you add or change the VPN service provider.
Click a checkbox and the trash icon to delete access to a service.

{@img SpaceMgrVPNServices.png}

This tab lists:

 - Name - VPN service name
 - Type - SSL VPN type
 - Modified - When the VPN service was last configured
 - Plus Icon - Add or change a VPN Service

### Configure VPN Service

{@img SpaceMgrCfgVPN.png}

To configure a VPN service:

 1. Click the Plus icon
 2. Specify a Service Name for the display on the VPN Services tab
 3. Specify the URL of the server that authenticates your VPN service access
 4. Click the SSL VPN type from the list
 5. Click Yes or No for whether public-key cryptography is enforced on the VPN
    using the [RSA](http://en.wikipedia.org/wiki/RSA_\(algorithm)) algorithm

<a name="ReportsTab"></a>
## Reports Tab

Provides interactive reports for:
<ul>
<li><b>Last Seen</b> - Number of users logging in. Lets you filter by duration, 
date range, platform OS, platform version, and whether the device is 
owned by the user or the organization. Also lists recent logins.<br><br>
{@img ManagerReportsLastSeen.png}<br></li>
<li><b>Platform Distribution</b> - Pie chart by device platform with filters for 
date range, platform OS, platform version, and whether the device is owned 
by the user or the organization. Also lists current devices.<br><br>
{@img ManagerReportsPlatDist.png}<br></li>
<li><b>App Launches</b> - Number of app launches with filters for 
date range, platform OS, platform version, and whether the device is owned 
by the user or the organization. Also lists the available apps by usage,
with columns for when the app was created (made available to 
your organization).<br><br>
{@img ManagerReportsAppLaunches.png}</li>
</ul>

<a name="SettingsTab"></a>
## Settings Tab

Enables you to set general information, manage your security policy, 
set authentication requirements, change your subscription, and change
contact and billing information. 

{@img ManagerSettings.png}

Settings sections:
<ul>
<li><a href="#GeneralSettings">General</a></li>
<li><a href="#SecuritySettings">Security Policy</a></li>
<li><a href="#AuthenticationSettings">Authentication</a></li>
<li><a href="#SubscriptionSettings">Subscription</a></li>
<li><a href="#BillingSettings">Billing</a></li>
</ul>

<a name="GeneralSettings"></a>
### General Settings

Lets you set:

<ul>
<li>Organization Name - Any string.</li>
<li>Organization Code - A string you receive from Sencha after you register 
your organization with Sencha</li>
<li>Email Method - Native email pattern or Good Enterprise email pattern.</li>
<li>Email Pattern - Indicates how to resolve <code>mailto</code> links per the
email method you select. A Native pattern starts with <code>mailto</code>, 
whereas a Good Enterprise pattern starts with the <code>gdmailto</code>
or <code>ggmailto</code> value.</li>
</ul>

{@img GeneralSettings.png}

<a name="SecuritySettings"></a>
### Security Policy

<b>Important</b>: When you change any of the 
options in the Security Policy, all users are sent 
a notice when they log in that 
they need to change their PIN.

Lets you set:
<ul>
<li>Max Idle Time Before PIN Challenge - How long a user can perform other
activities outside the Client app before being reprompted for a PIN. For 
example, a presses the Home button on their mobile device to exit the 
Space client. If the Session Duration is set to 5 minutes, the user has up
to 5 minutes before the Space Client reprompts for a PIN upon returning
to the Space Client. The tradeoff is that the Space Client data is more
vulnerable during the Session Duration interval should the device
be lost or stolen, but the duration gives users more time 
to perform other activities before being reprompted 
when returning to the Space Client. The duration can be set to minutes,
hours, days, weeks, or months.</li>
<li>PIN Type (slider) - Slide to:
<table style="width: 80%" border="1">
<tr><th>Value</th><th>Description</th><th>Example</th></tr>
<tr><td><b>None</b></td><td>Users don't use a PIN to access the Space client. 
Only use this setting during debugging.</td><td>1234</td></tr>
<tr><td><b>Numeric</b></td><td>Require users to enter a numeric
PIN. The numeric keypad appears and only permits values 
of 0 1 2 3 4 5 6 7 8 9.</td><td>1234</td></tr>
<tr><td><b>Alphabetic</b></td><td>Require users to specify a PIN consisting
of at least 4 letters. Users see the device's default keyboard.</td>
<td>abcdef</td></tr>
<tr><td><b>Alphanumeric</b></td><td>Require users to enter a PIN consisting
of at least 4 letters and numbers.  Users see the device's default keyboard.</td>
<td>a1b2c3d4</td></tr>
<tr><td><b>Complex</b></td><td>Require a secure PIN that has a combination
of letters, numbers, and special characters. A PIN must consist of at
least 4 characters, have at least one number, letter, and special character, 
and can be any length. Users see the device's default keyboard.</td>
<td>!MyP1n</td></tr>
</table></li>
<li>PIN Minimum Length - The least number of numbers a user can enter
to be authenticated in the Space client. The default is 6 digits.</li>
<li>Lock Out After Failed PIN Attempts - The number of tries a user gets to supply
the correct PIN value before being locked out. The default is 10 attempts.
If a user is locked out, the user must re-enter their username and password
to gain access to the Space client. The minimum is 2 tries.</li>
<li>Max Time Working Offline Before Automatic Log Out - Maximum 
duration that a user can perform other activities outside the Space Client 
before being automatically logged off. The duration
can be set to an amount of minutes, hours, days, weeks, or months.</li>
<li>Save - Save settings.</li>
</ul>

<b>Notes</b>: 
<ol>
<li>A PIN requiring letters can be any character on the device's
keyboard including international characters.</li>
<li>If you change a value in the security policy, a message may display
indicating that a value changed. However, the value is not changed in the
security policy until you click the <b>Save</b> button.</li>
<li>Changes to the security policy appear in the 
<a href="#SenchaLogMenu">Sencha Log Menu</a> - however, some messages
may indicate changes prior to clicking the <b>Save</b> button.</li>
</ol>

{@img SecurityPolicySettings.png}

<a name="AuthenticationSettings"></a>
### Authentication

Lets you set:
<ul>
<li>Authentication Method - <b>Invite Only</b> - Indicates that 
you can only add user access to the Space client by an email
invitation. <b>SAML 2.0</b> - Indicates that users are given access
to the Space client by virtue of access through SAML.</li>
<li>User Provisioning - Auto add user to Space - Add user automatically
to Sencha Space.</li>
<li>Identity Provider (IDP) Config:
  <ul>
  <li>Endpoint URL - Indicate the URL for how to access the IDP.</li>
  <li>Certificate - List the IDP certificate. Use this syntax:
<pre>
-----BEGIN CERTIFICATE----- Insert your certificate here -----END CERTIFICATE----- 
</pre>
 </li>
  <li>Attribute Presets - Custom, OneLogin (single sign-on), 
  or <a href="http://en.wikipedia.org/wiki/Active_Directory_Federation_Services">Active Directory Federation Services 2.0</a></li>
  <li>Email Attribute - Specify the email address of your IDP</li>
  <li>First Name Attribute - Specify the first name of the IDP</li>
  <li>Last Name Attribute - Specify the last name of the IDP</li>
  </ul></li>
</ul>

{@img AuthenticationSettings.png}

<a name="SubscriptionSettings"></a>
### Subscription

Lets you change your Space subscription, which depends on how many users,
users per device, and apps your organization requires.

{@img SubscriptionSettings.png}

To upgrade or downgrade, click <b>Change</b> for the subscription option
you require.

<a name="BillingSettings"></a>
### Billing

Information about your Space Manager subscription:
<ul>
<li>Status of Your Current Subscription - Whether you are using Space
on a trial or are active.</li>
<li>Subscription Period End - The date your Space subscription ends.</li>
<li>Billing Tier - Indicates whether your subscription applies to your
team, workforce, or enterprise.</li>
<li>Contact Information - Who to contact who is responsible for maintaining
the Sencha Space subscription.</li>
<li>Credit Card - Which credit card to use to bill for your Sencha
Space subscription.</li>
<li>Update - Click to save changed information in this form.</li>
</ul>

{@img BillingSettings.png}

<a name="AdminMenu"></a>
## Admin Menu

{@img ManagerAdminMenu.png}

Lets you:

 - Edit your administrator profile
 - Log out of Sencha Space

### Edit Your Admin Profile

Edits your administrator's profile.

{@img SpaceMgrEditAdminProfile.png}

Add or change:

 - First Name
 - Last Name
 - Title (optional)
 - Email address
 - New Password
 - Confirm New Password

You can also click <b>X Block</b> to block the administrator, or click
<b>Remove User</b> to remove your access to Sencha Space.


<a name="SenchaLogMenu"></a>
## Sencha Log Menu

Lists your recent activities. Each time the browser refreshes, 
the previous entries no longer display.

{@img Space_Ad_SenchaLogMenu.png}

Icons from left to right: 

 - Infinity - Lists recent log entries
 - Success - Lists successful actions, such as inviting a user or creating a group
 - Failure - Lists actions that failed
 - Information - Lists entries for review or that are informative
