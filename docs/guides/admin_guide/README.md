# Administration Application Guide 

The Sencha Space Administration application lets you manage user and group
access to web apps that run on a Sencha Space Client. Sencha hosts the Space
Administration app at [http://manage.space.sencha.com](http://manage.space.sencha.com).

The Space Administration application enables secure access to an enterprise's
authentication and authorization server. 

Users obtain their Sencha Space Client apps from the Apple App Store, 
Google Play, or BlackBerry World Apps market.

## Sencha Space Features

The Administration Application side menu contains these buttons:

<table border="1" style="width: 600px">
<tr><th>Icon</th><th>Description</th></tr>
<tr><td>{@img Space_Ad_Overview_Icon.png}</td>
  <td><a href="#OverviewTab">Overview</a> - Monitors organization Space usage and user 
  device and location</td></tr>
<tr><td>{@img Space_Ad_Apps_Icon.png}</td>
  <td><a href="#AppsTab">Applications</a> - Lists, adds, and
removes applications</td></tr>
<tr><td>{@img Space_Ad_Users_Icon.png}</td>
  <td><a href="#UsersTab">Users</a> - Lists, adds, and removes user access to Space</td></tr>
<tr><td>{@img Space_Ad_GroupsIcon.png}</td>
  <td><a href="#GroupsTab">Groups</a> - Lists, adds, and removes group access to Space</td></tr>
<tr><td>{@img Space_Ad_VPNServices_Icon.png}</td>
  <td><a href="#VPNServicesTab">VPN Services</a> - Manages VPN services</td></tr>
<tr><td style="vertical-align: top">{@img Space_Ad_ReportsIcon.png}</td>
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
<tr><td style="vertical-align: top">{@img Space_Ad_GearIcon.png}</td>
  <td><a href="#GearTab">Settings</a> - Provides settings for:
<ul>
<li>General - Specify organization name, organization code, email method, and billing address.</li>
<li>Security Policy - Specify user session duration, PIN length, 
and maximum attempts before lock out.</li>
<li>Authentication - Specify authentication method, Security Assertion Markup Language (SAML) 
configuration, and Identity Provider (IDP) information.</li>
</ul>
</td></tr>
</table>

The top menu contains these buttons:

<table border="1" style="width: 600px">
<tr><th>Icon</th><th>Description</th></tr>
<tr><td>{@img Space_Ad_AdminMenu.png}</td>
  <td><a href="#AdminMenu">Admin Menu</a> - Edit the admin profile, switch organizations, or log out of Sencha Space</td></tr>
<tr><td>{@img Space_Ad_SenchaIcon.png}</td>
  <td><a href="#SenchaMenu">Sencha Menu</a> - View a log of all administrator activities</td></tr>
</table>

**Icons**:

These icons provide additional controls on tabs:

<table border="1" style="width: 600px">
<tr><th>Icon</th><th>Description</th></tr>
<tr><td>{@img Space_Ad_PlusIcon.png}</td>
   <td>Plus Icon - Add an entry</td></tr>
<tr><td>{@img Space_Ad_CircleArrowIcon.png}</td>
   <td>Refresh - Refreshes the current list</td></tr>
<tr><td>{@img Space_Ad_TrashCanIcon.png}</td>
   <td>Trash Can - Remove the entry</td></tr>
</table>


<a name="OverviewTab"></a>
## Overview

The Overview tab lets you monitor system usage. The chart shows user activity 
for the current month:

{@img Space_Ad_Overview.png}

This tab lists:

 - Created users - Number of Space users who can participate
 - Active users - Number of Space users currently viewing Sencha Space
 - Opens - Indicates when a user starts a Space-managed app
 - Apps - Number of web apps available for use on Sencha Space Clients
 - Users - Number of created users
 - Groups - Number of enterprise groups
 - Active Devices - Lists:
   - Device. The device running Sencha Space Client and whether the device 
     is simulated or actual. 
   - User. User's email address and the Sencha Space Client version.
   - Last Access. When the user last accessed the Sencha Space Client and the user's location. 

You can click the red X icon to block a user from Sencha Space Client access: 

{@img Space_Ad_BlockUserButton.png}

Blocking a user sends the user a message and forces a log out from the 
Sencha Space Client. To reactivate the user, click the 
Users tab and add the user information again.

<b>Note</b> If no data appears in the Administration app at 
[http://manage.space.sencha.com](http://manage.space.sencha.com), or you wish to log out, click 
the administrator drop-down and click **Logout**:

{@img Space_Ad_LogoutMenu.png}


<a name="AppsTab"></a>
## Applications Tab

The Applications tab lists all apps that can run on a Sencha Space Client. Click an
application to [view more information](#appinfo) about the application. 
You can give each user a different set of applications within your organization. 

The first application you set for a user 
becomes the first screen they see when they log into the Space Client. As administrator,
you can use a web page as the starting application, for example, as a welcome screen
with ways for users to get help or learn how to use their client.

The Applications tab lists:

 - Name - Application name
 - Members - Number of users who are permitted to use the application
 - Modified - Last modification date of the application

<a name="appinfo"></a>
### Application Information Menu

View the HTML web applications that can run on a Sencha Space Client app. 
The web applications can be served using HTTP or HTTPS. 

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

 - App Name - Specify the name of the app as it will appear on the 
   Applications tab and on the Space Client.
 - App URL - The URL of the app. This can be an HTTP or HTTPS address.
 - Icon URL - The URL of the icon for the app. This can be an HTTP or HTTPS address.
 - Intents - List the apps that this app can invoke. Separate app names with commas. 
 This field is case sensitive.
 - Description - Description of the app. This description appears on the Applications 
   tab and in the Space Client.

<a name="UsersTab"></a>
## Users Tab

Lists which users can log into the Space Client and run applications. 
Click a user name to view more information about the user. 
Click the plus icon to invite a user. 
Click a checkbox and the trash icon to delete a user.

 - Name - User name
 - Last Active - When the user last used the Space Client

### User Information Menu

Click a user in the Users tab to view information:

Info tab:

 - Title - User's title in the enterprise
 - Email - User's email address
 - Apps Assigned - Which web apps the user can view on the Space Client

Groups tab:

 - Name - Groups to which the user belongs
 - Members - Users in the group
 - Modified - When the group was last modified

### Invite Users

Click the plus icon on the Users tab to invite one or more users to participate
in Sencha Space. The users you invite receive an email with information about 
how to log into the Sencha Space Client.

{@img Space_Ad_Users_PlusIcon.png}

To invite a user:

1. Type the user's email address and click the plus symbol. You can enter
   multiple addresses separated by commas.
   {@img Space_Ad_Invite_Users.png}
2. If needed, delete an address and re-type.
3. Click Invite.

<a name="GroupsTab"></a>
## Groups Tab

Creates or removes a group. Applications can be associated with a group
or a user. Click a checkbox and the trash icon to delete a group.

The Groups tab displays:

 - Name - Group name
 - Members - Number of users in a group
 - Modified - When the group was last modified
 - Plus icon - Add a new group

### Adding a New Group

 To add a new group:

 1. Click the Plus icon on the Groups tab
 2. Specify a group name
 3. Specify a description of the group


<a name="VPNServicesTab"></a>
## VPN Services Tab

The VPN Services tab lets you add or change the VPN service provider.
Click a checkbox and the trash icon to delete access to a service.

This tab lists:

 - Name - VPN service name
 - Type - SSL VPN type
 - Modified - When the VPN service was last configured
 - Plus Icon - Add or change a VPN Service

### Configure VPN Service

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
{@img Space_Ad_ReportLastSeen.png}<br></li>
<li><b>Platform Distribution</b> - Pie chart by device platform with filters for 
date range, platform OS, platform version, and whether the device is owned 
by the user or the organization. Also lists current devices.<br><br>
{@img Space_Ad_ReportPlatDist.png}<br></li>
<li><b>App Launches</b> - Number of app launches with filters for 
date range, platform OS, platform version, and whether the device is owned 
by the user or the organization. Also lists the available apps by usage,
with columns for when the app was created (made available to 
your organization).<br><br>
{@img Space_Ad_ReportAppLaunches.png}</li>
</ul>

<a name="GearTab"></a>
## Settings Tab

Provides settings for:
<ul>
<li>General - Specify:
<ul>
<li>Organization Name - Any string.</li>
<li>Organization Code - A string you receive from Sencha after you register 
your organization with Sencha</li>
<li>Email Method - Placeholder - Changing soon</li>
<li>Billing - Placeholder - Changing soon</li>
</ul>
<br><br>
{@img Space_Ad_Settings_General.png}<br></li>
<li>Security Policy - Specify user session duration, PIN length, 
and maximum attempts before lock out.<br><br>
{@img Space_Ad_Settings_Security.png}<br></li>
<li>Authentication - Specify authentication method, 
Security Assertion Markup Language (SAML) 
configuration, and Identity Provider (IDP) information.<br><br>
{@img Space_Ad_Settings_AuthN.png}<br></li>
</ul> 


<a name="AdminMenu"></a>
## Admin Menu

{@img Space_Ad_AdminMenuOptions.png}

Lets you:

 - Edit your administrator profile
 - Switch organizations
 - Log out of Sencha Space

### Edit Your Admin Profile

Edits your administrator's profile.
Add or change:

 - First Name
 - Last Name
 - Title (optional)
 - Email address
 - New Password
 - Confirm New Password

You can also click **Remove User** to remove your access to Sencha Space.

### Switch Organizations

Click the administrator menu drop-down and click **Switch organizations**:

{@img Space_Ad_SwitchOrgs.png}

The first organization in the list is the one you are currently administering. 

When you switch organizations, all the settings from the former organization 
store while you work with other organizations.

This tab lists:

- Name - Organization name 
- Code - Organization code
- Users - Number of users in the organization 
- Groups - Number of groups in the organization
- Apps - Number of apps handled for this organization

To add or change organization information:

 - Add an organization by clicking the Add button.
 - Change the information for an organization by clicking the Pencil button: 
   {@img Space_Ad_OrgEditButton.png}
 - Change the current organization by clicking the Switching button:
   {@img Space_Ad_OrgChangeButton.png}

#### Add or Change an Organization

Add an organization by clicking **Add** in the Organizations menu. 
Change organization information by clicking the pencil icon in
the Organizations menu.

Fields:

 - Organization Name 
 - Organization Code - A keyword to describe the organization
 - Cache Max Age - The duration in minutes that you want information 
   about an organization to persist in the Client. Zero indicates
   that the information persists indefinitely and only updates when a
   user clicks the Refresh button.

<a name="SenchaMenu"></a>
## Sencha Menu

Logs all administration activities.

{@img Space_Ad_SenchaLogMenu.png}

Icons from left to right: 

 - Infinity - Lists all log entries
 - Success - Lists successful actions, such as inviting a user or creating a group
 - Failure - Lists actions that failed
 - Information - Lists entries for review or that are informative
