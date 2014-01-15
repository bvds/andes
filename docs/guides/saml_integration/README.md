# SAML Integration Guide

Security Assertion Markup Language (SAML) is a data format for exchanging 
authentication and authorization data between the Sencha Space Client
and the Sencha Space service provider.

For more information on SAML, see 
<a href="http://en.wikipedia.org/wiki/SAML_2.0">http://en.wikipedia.org/wiki/SAML_2.0</a>.

When a user enters an organization, the Space Client sends the user to the 
appropriate identity provider (IdP). Space uses an HTTP redirect for SAML requests 
and an HTTP post for SAML responses.


## SAML Settings

To enable SAML authentication support in Sencha Space, select <b>SAML 2.0</b> in 
Space Manager &gt; <b>Settings</b> &gt; <b>Authentication</b>. 

The following provide information on the Space Manager Authentication Settings.
See also <a href="#!/guide/manager_guide-section-AuthenticationSettings">Authentication Settings</a>.

### Endpoint URL

The URL to send the SAML authentication request. You can usually find this URL in your 
IdP’s <code>federationmetadata.xml</code> file in the <code>SingleSignOnService</code> node.

### Certificate

An <a href="http://en.wikipedia.org/wiki/X.509">X.509 certificate</a> in 
<a href="http://en.wikipedia.org/wiki/Privacy-enhanced_Electronic_Mail">PEM format</a>.

**Note**: Do not use for Azure ADFS, as there is a bug in the latest version.

### Attribute Presets

Commonly used attribute names for certain IdPs.

**Note**: An invalid or missing attribute won't prevent a user from being added.

### Email Attribute

The attribute name used to denote the user’s email address.

### First Name Attribute

The attribute name used to denote the user’s first name.

### Last Name Attribute

The attribute name used to denote the user’s last name.
