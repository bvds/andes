# Sencha Space Security Architecture

Sencha Space’s primary focus is its rigorous attention to security.  
Sencha Space serves enterprises and their employees, IT administrators, 
and application engineers. These environments have very strict 
information security policies and practices. This document 
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
<li>Configurable security policies for organizations</li>
<li>Remote data wipe</li>
</ul>

# Architecture

{@img architecture.png}

Applications: Each application has its own web view, isolating an application from all of the other applications running in space.

Security Policy: limits what APIs each application can access.

Encryption Backplane: Low-level security library that handles the encryption of all persistent data.

Encrypted File System: All persistant data within space is automatically encrypted before it is written to the application's un-encrypted filesystem.  Further each application has access to its own isolated encrypted file system via Space's <a href="#!/guide/secure_file_api">Secure File API</a>.

Encrypted Database: Each application can be granted access to an encrypted database. An application can access this database via Space's <a href="#!/guide/sec_lcl_strg">Secure Local Storage API</a>.

{@img databases.png}


# Key Management

Sencha Space can be configured to require a security pass phrase or pin before granting access to the organization's applications. For maximum security Space does not store the decryption keys in any persistent form. When the user enters their PIN, it is combined with other unique data to generate a decryption key. Once the decryption key is generated it is used to then decrypt the actual data. Without the user's PIN the data cannot be easily decrypted without resorting to brute force decryption methods. Space can also be instructed to delete the organization's data if too many incorrect pin entries are given.

{@img keys.png}

# Industry Standards

Sencha Space relies on industry standard encryption and security algorithms and libraries. 
Space uses AES-256, HMAC-SHA-256, SHA-256, PKDBF2, scrypt, OpenSSL, SQLCipher to ensure that data stored in space is kept secure.

{@img standards.png}


# HTML5 local data 

HTML5 applications can persist data on the client using cookies, caches, local storage and databases. In most browsers these assets are left unencrypted on the user's device. In Sencha Space we solve this problem by encrypting all of this data. So by simply deploying your existing HTML5 applications in space the local information they store will be kept encrypted. Further because Space supports multiple organizations, the data from each organization is isolated. 

{@img isolated-orgs.png}


