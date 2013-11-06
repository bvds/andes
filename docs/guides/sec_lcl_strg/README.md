# Secure Local Storage

This guide describes the Sencha Space secure local storage API and how 
to code your application to use this storage.

Secure local storage encrypts data before storing in a mobile device's local memory.
This feature lets applications store organizational data securely. Sencha Space uses
the <code>Ext.Promise</code> API to asynchronously process reads, writes, and deletes
through its quick encryption and decryption mechanism. 

Data in the local storage is represented as a collection of JSON key-value pairs. 

Sencha Space secure local storage improves on the 
<code>localstorage</code> feature of HTML5:

<ul>
<li>All data is encrypted before being persisted to local storage</li>
<li>Much higher storage limits than the 2-3 MB allocated for <code>localstorage</code></li>
<li>Multiple key collections per application</li>
<li>Asynchronous using <code>Ext.Promise</code></li>
</ul>

Secure local storage uses these APIs: 

<table style="width: 80%" border="1">
<tr><th>API</th><th>Description</th></tr>
<tr><td><a href="#!/api/Ext.Promise">Ext.Promise</a></td>
<td>Provides the eventual result of an asynchronlus operation. For more
information, see the <a href="http://promisesaplus.com/">Promises/A+</a> site.
A promise registers callbacks to receive after the operation completes.</td></tr>
<tr><td>
<a href="#!/api/Ext.space.SecureLocalStorage">Ext.space.SecureLocalStorage</a></td>
<td>Encrypted key-value store modeled on HTML5 <code>localstorage</code>.</td></tr>
<tr><td><code>Ext.space.Sqlite</code></td><td>Provides the backend storage mechanism for 
Sencha Space.</td></tr>
</table>

## Using Secure Local Storage

The following storage function points to a storage area using a key value. 
If the caller provides data, the function stores the
data. If no data is present, the function reads data from the key location. 

<pre>
store: function (key, data) {                         
	var mystore = Ext.space.SecureLocalStorage.get(key);
	if(data){
	        return mystore.set('items', data);
	} else {
	        return mystore.get('items');
	}
}
</pre>

## Deleting Secure Local Storage

You can delete a key-value pair from memory using:

<pre>
delete: function (key) {
	var mystore = Ext.space.SecureLocalStorage.get(key);
	return mystore.delete('items');
}
</pre>

You can call this function with:
<pre>
mystore.delete('items').then(function(isDeleted){ ... })
</pre>
 

## Searching for Stored Data

The following API lets you find data:

<pre>
find: function (key) {
    var mystore = Ext.space.SecureLocalStorage.get(key);
    return mystore.get('items', data);
}
</pre>

You could then search using:
<pre>
find('key').then(function(WhatToDoWhenFound){
	// Code for when key is found
});
</pre>



