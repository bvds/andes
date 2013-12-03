# Sencha Space Security Architecture

Sencha Space’s primary focus is its rigorous attention to security.  
Sencha Space serves enterprises and their employees, IT administrators, 
and application engineers. These environments have very strict 
information security policies and practices.  This document 
details Space’s security model.


## Security Goals

Encryption is one of the first things one thinks of in
“security,” but in reality data encryption is merely one slice of the security pie.  
The security model of Space is unique from the traditional web, desktop, 
and mobile security models, in that Space balances the challenging task pf protection
for the organization, the organization’s users, and the users’ devices 
with flexibility and ease of use, particularly for data synchronization 
and offline authentication.  

Sencha Space:

<ul>
<li>Ensures all user data and private organization data
remains private and is not stored or 
transmitted in the clear or outside a user’s device.</li>
<li>Maintains oganizational control over the data that resides 
on a user’s device with the ability to remotely wipe the data at any time.</li>
<li>Maintains oganizational access control between applications, 
users and their devices. Space provides clear insight and control over who 
uses a system, and when and where systems are used.</li>
<li>Provides access to private data offline with no 
real-time server authentication or authorization in place.</li>
<li>Space always assumes it's running in a hostile environment 
where the device is not necessarily in the hands of a trusted user.  
No device is considered a secure or trusted execution 
environment by default.</li>
<li>Mobile only: A portion of private data can potentially reside 
in web-based storage containers such as cookies or cache, and the 
browser must have these containers and files in the clear at runtime 
to operate. Space does not control the browser source code 
to secure these assets in real-time.</li>
</ul>

Sencha Space handles these challenges:

<ul>
<li>Local data encryption with secure key management</li>
<li>Jailbreak and rooting detection on mobile devices</li>
<li>Detect and disallow debugger attachment on mobile devices</li>
<li>2-factor authentication schemes, such as PKI-based authentication, 
RSA token, SMS PIN code, etc.</li>
<li>Configurable security policies for organizations</li>
<li>Remote data wipe</li>
<li>Maintain a strong accreditation of service and security compliance and 
limit the number of client audits, such as HIPAA, SSAE-16, etc. This 
applies to both cloud and on-premise hosting options.</li>
</ul>


## Security Process

Sencha Space security process:

<ol>
<li>User opens the Sencha Space Client.</li>
<li>If this is a new session, the user specifies an organization. If the organization
has a pre-defined PIN, the user enters the PIN. 
If the PIN is not pre-defined, Sencha Space Client 
prompts the user to create a PIN, generates an encrypted 
<a href="#SyncKey">Sync Key</a>, and stores the 
secure sync key on the device. The Sync Key gives the user secure access to an
organization.</li>
<li>If the user is returning to a previously started session, Sencha Space Client prompts
the user for the user's PIN and retrieves the session key.</li>
<li>Sencha Space Client retrieves the secure Sync Key and verifies it.</li>
<li>Sencha Space Client encrypts the session.</li>
<li>When a user exits without ending a session, Sencha Space Client encrypts 
the session key and goes to the background. When the user returns, control passes
back to Step 3.</li>
<li>At session end, Sencha Space Client clears out the session token, 
private screens, data, and session token.</li>
</ol>

## Information Security & Assurance

Information security consists of:

<ul>
<li><a href="#KeyManagement">Key Management</a></li>
<li><a href="#KeyBundle">Key Bundle</a></li>
<li><a href="#RootKeyBundle">Root Key Bundle</a></li>
<li><a href="#SyncKey">SyncKey</a></li>
<li><a href="#SyncKeyBundle">Sync Key Bundle</a></li>
<li><a href="#CollectionKeys">Collection Keys</a></li>
</ul>

<a name="KeyManagement"></a>
### Key Management

Space dynamically encrypts private data using a key management system 
with the following features:

<ul>
<li>Every device generates and securely stores its own unique 
full-entropy base encryption key, and the <a href="#SyncKey">Sync Key</a>, 
which derives encryption key-pairs ad hoc.</li>
<li>Every device derives its own unique encryption key-pair ad hoc and  
derives the <a href="#RootKeyBundle">Root Key Bundle</a>, so that only the 
device itself can decrypt its stored data. 
If the data is transmitted or stored remotely, it cannot be decrypted. 
Also, the data cannot be restored to a different device without an 
authorized exchange between one device and another.</li>
<li>All key derivations are based upon knowledge the user is in possession 
of, such as a PIN code or passphrase. Without this, keys cannot be derived 
securely and thus data cannot be recovered.</li>
<li>Different types of data and storage containers use separate derived 
key-pairs for their encryption. No single derived key can be used to 
decrypt all data on the device.</li>
<li>Through a secure exchange protocol over SSL - such as 
<a href="http://en.wikipedia.org/wiki/Password_Authenticated_Key_Exchange_by_Juggling">J-PAKE</a> (Password Authenticated Key Exchange by Juggling), 
two devices can pair by exchanging the decrypted Sync Key from the primary device 
to a secondary (or tertiary, etc.) device.  The devices are then able to 
synchronize data and collaborate securely between themselves.</li>
</ul>

<a name="KeyBundle"></a>
### Key Bundle

In Sencha Space, a Key Bundle is a pair of 256-bit keys: a symmetric encryption key 
used for AES-256 encryption, and a HMAC key used for HMAC signing. 
Every piece of data that Space stores on the system is encrypted 
and signed by its own uniquely generated Key Bundle.

<a name="RootKeyBundle"></a>
### Root Key Bundle

The Root Key Bundle is a derived key-pair based on a secured master key, 
data unique to the device, the organization code, and a user provided 
PIN code or passphrase. Space uses the Root Key Bundle to encrypt 
and sign the Sync Key before storing the Sync Key on a user's device.
The key-pair is derived using a 
<a href="http://en.wikipedia.org/wiki/Scrypt">scrypt</a> by following 
this formula in pseudo-code:

<pre>
pass = SHA256(master_key + SHA256(passphrase + device_uuid + org_code + username))

// Use at most 64 MB memory or 10% of available memory, whichever is lower.
// Perform enough hash iterations to take approximately a full 1.0 seconds of CPU time.
scrypt_packet = scrypt(pass, 64 * 1024 * 1024, 0.10, 1.0)
encryption_key = scrypt_packet(112, 32) // Take last 32 bytes of packet in the key

// Take the first 112 bytes of the packet from the header
salt = SHA256(master_iv() + scrypt_packet(0, 112)) // 
hmac = PBKDF2(HMAC-SHA256, encryption_key, salt, 100000)

root_key_bundle = pair(encryption_key, hmac)
</pre>

The scrypt value returns a 144 byte packet that contains not only 
the 32 byte derived key, but also a 112 byte header. 
The header object can be used to re-derive a password 
using the same scrypt parameters and 
<a href="http://en.wikipedia.org/wiki/Salt_(cryptography)">salt</a>, and therefore validate 
a given password as being a match to the original input. 
The scrypt value is also the most advanced and resource intensive 
key derivation function in popular use, making it too expensive and 
impractical time-wise for an attacker to brute force a reasonably 
strong password even with specialized hardware.

<a name="SyncKey"></a>
### Sync Key

The Sync Key is the master private key used by Space to derive 
subsequent key-pairs, or <a href="#KeyBundle">Key Bundles</a>. The Sync Key is a unique 
full-entropy 256-bit sequence that can be shared among 
all devices that wish to synchronize data or otherwise 
collaborate securely with one another. 

Generation of this key originates from a single primary device 
and is never transmitted or stored outside of the device, with the 
exception of a cryptographically-secure exchange tunnel between 
the primary and secondary device.

The key is generated by the following formula in pseudo-code:

<pre>sync_key = randomBytes(32)</pre>

randomBytes is assumed to be a cryptographically random byte generation function. 
The Sync Key is stored encrypted on the device using AES-256-CBC and signed 
using HMAC-SHA256 with the following formula in pseudo-code:

<pre>
iv = randomBytes(16)
encrypt_key = root_bundle.encrypt_key
hmac_key = root_bundle.hmac_key

sync_ciphertext = AES-256-CBC(sync_key, encrypt_key, iv)
hmac = HMAC-SHA256(hmac_key, sync_ciphertext)

store(base64(sync_ciphertext + hmac + iv))
</pre>

<a name="SyncKeyBundle"></a>
### Sync Key Bundle

The Sync Key Bundle is used to encrypt and sign all <a href="#KeyBundle">Key Bundles</a> 
that are used to encrypt data. The bundles are stored alongside the encrypted data. 
This bundle derives from the Sync Key using HKDF and SHA-256 (RFC 5869). 
In pseudo-code:
<pre>
info = master_iv + “Sync”
encryption_key = HKDF(sync_key, info + "\x01", 32)
hmac = HKDF(sync_key, encryption_key + info + "\x02", 32)

key_bundle = pair(encryption_key, hmac)
</pre>


<a name="CollectionKeys"></a>
### Collection Keys

As noted in <a href="#KeyBundle">Key Bundles</a>, all pieces of data stored 
on the system generate their own random Key Bundle that is used to encrypt 
and sign the data. Space refers to these as random keys Collection Keys. 
After the Collection Keys encrypt and sign the payload, the Sync Key Bundle 
encrypts and signs the Collection Keys. Finally, the encrypted data and 
its encrypted Collection Keys are stored together on the device. 
This is the process in pseudo code:
<pre>
collection_keys = pair(randomBytes(32), randomBytes(32))
encryption_key = collection_keys.encryption_key
hmac_key = collection_keys.hmac_key
iv1 = randomBytes(16)
iv2 = randomBytes(16)
cleartext = “SECRET”
	
// Encrypt and sign data
ciphertext = AES256(cleartext, encryption_key, iv1)
hmac = HMAC-SHA256(hmac_key, ciphertext)

// Encrypt and sign collection keys
cipherkeys = AES256(collection_keys, sync_key_bundle.encryption_key, iv2)
hmackeys = HMAC-SHA256(sync_key_bundle.hmac_key, cipherkeys)

store(base64(ciphertext + hmac + iv1) + “|” + base64(cipherkeys + hmackeys + iv2))
</pre>

## Storage Container Encryption

As the bring your own device (BYOD) movement continues to grow in the 
enterprise, there must be more assurances that confidential corporate 
data is secured from untrusted users in possession of a provisioned 
device. Space solves this problem at the container-layer so that HTML5 
applications need not worry about complex cryptography to secure 
their data assets on a user’s device. 

HTML5 applications running in Space utilize a number of data storage options including:

<ul>
<li>Cache (implicit)</li>
<li>Cookies (implicit if originated from server)</li>
<li>File System</li>
<li>IndexedDB</li>
<li>Local Storage</li>
<li>WebSQL</li>
</ul>

As stated in <a href="#KeyManagement">Key Management</a>, each storage container 
generates its own set of <a href="#CollectionKeys">Collection Keys</a> 
specifically for itself. 

The following topics provide additional Storage Container Encryption information:

<ul>
<li><a href="#MITMPrevention<">MITM Prevention</a></li>
<li><a href="#UCCE">User Credential and Certificate Encryption</a></li>
<li><a href="#ScreenshotCache">Screenshot Cache</a></li>
</ul>

<a name="MITMPrevention"></a>
### MITM Prevention

Space communicates with the Space server via HTTP over a secure SSL tunnel. 
While it would be hard to orchestrate a MITM attack against a trusted legitimate 
user as a malicious attacker, a curious legitimate user could setup a proxy server 
with a self-signed certificate, modify DNS settings and install the certificate on 
their device, then proceed to capture decrypted Space traffic.

To prevent such attacks, the Space client supports certificate pinning, and 
ensure that the Space server certificate is a valid approved certificate before 
completing the SSL handshake. Certificate pinning has its pitfalls however, 
such as forward compatibility of old Space clients when renewing an expired 
certificate.  

<a name="UCCE"></a>
### User Credential and Certificate Encryption

Space wants the experience for end users to be as friendly, seamless and non-invasive 
to the user’s workflow as possible while still maintaining a high level of security. 
One such feature is seamless re-authentication when a user’s session has expired; 
another being offline authentication. User credentials and/or client authentication 
certificates must be stored securely on the device using their own Collection Keys 
and by following the formula described in <a href="#CollectionKeys">Collection Keys</a>.

<a name="ScreenshotCache"></a>
### Screenshot Cache

On iOS specifically, the OS takes a screenshot of the application’s current view 
to perform the animation of the app entering into or returning from the background. 
If an attacker jailbreaks the device, the attacker can sift through the device cache and 
find screenshots containing potentially sensitive data.

On iOS, Space hides all application web views in the 
<code>applicationDidEnterBackground:</code> and 
<code>applicationWillResignActive:</code> 
<code>AppDelegate</code> 
methods so that sensitive information is not visible on screen when the OS takes 
the screenshot of the application.


## Device Intrusion Detection & Prevention

Detecting a hacker in possession of a Space device and preventing the hacker
from breaking into the Space code to gain access to private data is a 
very difficult problem to solve across all supported platforms.
While it is possible to make it very difficult to 
break an application’s code, a determined hacker with a lot of time can
crack almost any other piece of software, but not Space.

To help thwart attacks, Space:

<ul>
<li>Detects that the device has been jailbroken, rooted, or otherwise had its 
walled-garden environment of the OS compromised.</li>
<li>Detects and/or disallows debuggers from attaching to the running process.</li>
<li>Stores master encryption keys securely in the binary and securely in-memory 
only for the duration of cryptographic operations, immediately being purged after.</li>
<li>Limits class and header exposure for cryptographic operations so function 
prototypes and class definitions cannot be dumped.</li>
</ul>

## Authentication

Space interoperates with many existing authentication and identity 
providers, such as LDAP, Active Directory, RADIUS, 
Kerberos, OAuth2, SAML, etc. In addition to integration with existing 
authentication providers, Space provides a layer of additional authentication 
security and effectively provides a 2+ factor authentication
as described in the following sections:

<ul>
<li><a href="#TFUS">Two-Factor Using SMS</a></li>
<li><a href="#TFURSID">Two-Factor Using RSA SecurID</a></li>
<li><a href="#TWCC">Two-Way Client Certificates</a></li>
</ul>

<a name="TFUS"></a>
### Two-Factor Using SMS

Organizations can opt-in to require users to perform a 2-factor 
authentication using SMS text messages.  If a user's mobile phone number 
is in the system, Space performs the following:

<ol>
<li>User authenticates with the system using their normal passphrase-based credentials</li>
<li>Space generates a short-TTL PIN code that is SMS messaged to the user</li>
<li>User is prompted to enter in their PIN code, and must do it within the TTL (e.g. 60-90 seconds)</li>
<li>If the PIN is correct, the user receives an authenticated session token</li>
</ol>

<a name="TFURSID"></a>
### Two-Factor Using RSA SecurID

EMC's RSA SecurID is a popular hardware-token authentication service for 
performing 2-factor authentication.  EMC issues a hardware token generator 
to a trusted user, and maintains an association in its database of the user 
and their key.  

Space performs the following:

<ol>
<li>User authenticates with the system using their normal passphrase-based credentials</li>
<li>User generates a token from their hardware device and submits that to Space</li>
<li>Space submits the token to RSA SecurID and receives a yes or no response</li>
<li>If the token is valid, the user receives an authenticated session token</li>
</ol>

<a name="TWCC"></a>
### Two-Way Client Certificates

Client certificate authentication is one of the most common 2-factor authentication 
schemes deployed in enterprises. Most if not all security devices 
(firewalls, SSL proxies, etc.) are able to perform client certificate authentication. 
Like other 2-factor schemes, it is an additional layer of security to verify that the 
user "has" something (certificate) and that the user "knows" something (passphrase, etc.).

Space provides organizations several options to effectively use 
client certificates for authentication:

<ol>
<li>Turn on client certificate authentication in the Space management console:
	<ol>
		<li>Client devices automatically are required to submit a CSR to 
		the Space server, after validating their identity</li>
		<li>Space issues a certificate for the client and store the certificate public key</li>
		<li>Admins can export user certificates + public key via the management 
		console to install on external security devices</li>
	</ol></li>
<li>Import existing client certificates and a public key using the Space management console:
	<ol>
		<li>Install certificates and private keys manually on each client device</li>
		<li>Importing integrates well with existing security devices</li>
	</ol></li>
<li>Configure bookmark settings to specify that client certificate authentication 
is required in order to access the application</li>
</ol>

Subsequently, client certificates can be used in combination with a 
user PIN code or passphrase to do offline authentication.


## Organization Security Policy Configurations

In addition to information security through cryptographic algorithms, another 
major part of Space security is the ability to optionally set policies and rules 
around user authentication, behavior and locality, in addition to the standard 
application access control features of Space.

This section contains these topics:
<ul>
<li><a href="#PasswordPolicies">Password Policies</a></li>
<li><a href="#ClientPINCodePolicies">Client PIN Code Policies</a></li>
<li><a href="#FailedAuthenticationPolicies">Failed Authentication Policies</a></li>
<li><a href="#SessionSecurityPolicies">Session Security Policies</a></li>
<li><a href="#SecurityPolicyRuntime">Security Policy Runtime</a></li>
<li><a href="#PolicyTamperingDetection">Policy Tampering Detection</a></li>
<li><a href="#PolicyImplementation">Policy Implementation</a></li>
</ul>

<a name="PasswordPolicies"></a>
### Password Policies

<ol>
<li>Character rules:
	<ol>
		<li>Minimum total characters</li>
		<li>Types of required characters (alpha, numeric, casing, special)</li>
		<li>Minimum number of each required character type</li>
		<li>Password must be at least 12 characters with at least 
			1 uppercase letter, 1 digit, and 1 special character</li>
	</ol></li>
<li>Password TTL - force a password change on a specified interval:
	<ol>
		<li>Specify a length of time that a past password cannot be reused</li>
	</ol></li>
<li>Stricter “forgot password” requirements in addition to access to 
	the reset email or reset token:
	<ol>
		<li>Answer previously answered security questions</li>
		<li>Verify some other piece of information</li>
	</ol></li>
</ol>

<a name="ClientPINCodePolicies"></a>
### Client PIN Code Policies

<ol>
<li>Require Passphrase (full keyboard) over PIN code (digits)</li>
<li>Character rules:
	<ol>
		<li>Minimum total characters</li>
		<li>Types of required characters (alpha, numeric, casing, special)</li>
		<li>Minimum number of each required character type</li>
		<li>Password must be at least 12 characters with at least 1 uppercase letter, 
		1 digit, and 1 special character</li>
	</ol><li>
<li>Minimum number of digits (PIN code only)</li>
</ol>

<a name="FailedAuthenticationPolicies"></a>
### Failed Authentication Policies

After a defined set of authentication failures:

<ol>
<li>Force a captcha (human element) to attempt another authentication request</li>
<li>Temporarily lock out the user and/or device for a specified duration</li>
<li>Permanently lock out the user and/or device, requiring admin intervention to unlock</li>
</ol>

<a name="SessionSecurityPolicies"></a>
### Session Security Policies

Security policies prevent common attack scenarios such as 
password theft or session hijacking:

<ol>
<li>Set custom session TTL values:
	<ol>
		<li>Number of minutes</li>
		<li>Turn off long-lived sessions, expiring them immediately 
		after the client app is closed</li>
		<li>Set device-session TTL before requiring a manual re-authentication 
		from the device (no automatic re-auth)</li>
	</ol></li>
<li>Pin a session to its initiating IP address:
	<ol>
		<li>Only initiating IP is authorized to use the session token</li>
		<li>Optionally, revoke a session that moves from one network 
		to another network</li>
	</ol></li>
<li>Geofencing:
	<ol>
		<li>Pin a session within a specified radius of the initiating geographic location:
			<ol>
				<li>Friendlier to cellular networks while on the go</li>
			</ol></li>
		<li>Only authorize users when their geolocation meets the specified criteria:
			<ol>
				<li>Within a radius (miles or kilometers) around a particular zip
				or postal code location</li>
				<li>Whitelist of geolocations at different granularity:
					<ol>
						<li>Continent</li>
						<li>Countries</li>
						<li>Provinces/States</li>
						<li>Postal/Zip codes</li>
					</ol></li>
				<li>Optionally alert admins to grant temporary access for 
				users out of the country on vacation or on business</li>
			</ol></li>
	</ol></li>
<li>Subnet Masking:
	<ol>
		<li>Only authorize users on a whitelist of subnets</li>
	</ol></li>
<li>Work hours:
	<ol>
		<li>Only authorize users during particular times of the day, 
		days of the week, etc.</li>
	</ol></li>
<li>Online/Offline rules:
	<ol>
		<li>Can the client operate when it is not connected to the network?</li>
		<li>PIN rules for offline access</li>
		<li>How long a device is allowed to operate offline 
		using a PIN code before re-authorizing with the server.</li>
	</ol></li>
</ol>

These features differ by each organization's security policy.  

<a name="SecurityPolicyRuntime"></a>
### Security Policy Runtime

Part of the security policy executes on the Client. 
Each client maintains a current copy of the security policy 
in a secure location. The policy runtime executes when the device 
is not connected to the network. 

Space securely pushes the policy file to 
to clients and pulls the policy file from the server. 

When an administrator makes changes to the organization's security policy, 
Space regenerates and distributes the policy file to each device in that 
organization. 

Space ensures that each device checks for updated security policies 
on an interval specified in the security policy file. 

<a name="PolicyTamperingDetection"></a>
### Policy Tampering Detection

The policy runtime verifies the authenticity of a security policy file 
each time the policy is read into memory. If the policy file is missing 
or invalid, the application presents an error message and does not allow 
access to the organization. 

<a name="PolicyImplementation"></a>
### Policy Implementation

Storing security configurations assumes the use of a new table that 
associates to an organization, where all previously listed policies 
are presented as a columns (keys). 

While subnet-masking settings are stored as a serialized field 
in policies table, a list of allowed geolocations are presented 
as a separate new table with following columns: continent, country, 
region, city, postal_code.

To enforce the geolocation restriction, use a third party 
database of geographical locations such as 
<a href="http://www.geonames.org">geonames.org</a>. 
A user's location can be verified by a simple infliction.

Setting and retrieving security configurations assumes creation of a 
new RPC service to provide all necessary APIs.

Setting of the actual values of policies is performed from the 
admin panel using the server-side API of a newly created service.

Each client can get a current copy of the security policy 
by calling the server-side API from a newly created service. For such a call, 
the server generates a signed policy-file, which can be verified 
on the client side and stored encrypted in a secure location.
