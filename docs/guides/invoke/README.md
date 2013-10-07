# Invoke API

Sencha Space is a new way to manage, deploy and secure HTML5 applications 
on mobile devices. 
Invoke is a JavaScript API that lets one app securely run and communicate 
with another app. 
You can add the Invoke API to apps that you deploy in Sencha Space.
Invoke represents a new way of building HTML5 applications. 
Your applications no longer have to be islands connected only 
by a round trip to a server. Using Invoke, you can build simpler, 
single purpose applications that expose a simple API.

You can see Invoke in action at: 
<a href="http://vimeo.com/73736732">http://vimeo.com/73736732</a>

The video shows:
<ol>
<li>An application invoking the Photo app</li>
<li>Successful invocation of the Photo app from the first app</li>
<li>Selecting photos using the externally invoked Photo app</li>
<li>Returning the selected photos to the app that first initiated the invoke call</li>
</ol>


## Using Invoke in the Foreground and Background

Applications can communicate using Invoke:
<ul>
<li><b>Foreground</b> - Invoke calls enable a user to switch from one application 
to another. The user can then do work in the second application and when done, 
Sencha Space returns the user to the application they started from. A simple example 
of this is photos. In Sencha Space, you can have an application that knows where 
all of your organization's photos are. When another application needs a photo, 
it redirects the user to the Photos application. The user can select the photos 
they want, and then the user is returned to the application they started with,
and the application has the list of photos the user selected.</li>

<li><b>Background</b> - Invoke calls open up the possibility of a new class 
of application communication. Applications can exchange data in the background 
asynchronously without the user needing to leave the application they are 
currently in. For example, a Contacts application can communicate with a chat 
application to get the online/offline status of the current contact and update 
the contact record. The Contacts application need not integrate a chat library 
or maintain a connection with a chat/presence server. It only needs to make 
a simple API call to the chat application running in Sencha Space.</li>
</ol>

## Include the Sencha Space APIs

You can add the Invoke API by including one statement in your HTML5 or web site
application. The application need not have been created using a Sencha product.

<pre>&lt;script src="http://space.sencha.io/space.js"&gt;&lt;/script&gt;</pre>
 
## Invoke Applications in the Foreground

To communicate with another application, get a connection to it:
 
    Ext.space.Invoke.get('photos').then(send, failure);

If the application doesn't exist or your application doesn't have 
permission to call that application, Sencha Space calls your app's failure callback:
<pre>  
var failure = function(error) {
    console.log('Could not find photos app', error);
}
</pre>

When you have a connection, your application can start sending messages, in
this case, requesting all the photos taken in the current day (<tt>time: 1d</tt>):

<pre>    
var send = function(connection) {
    connection.send({tags: ['keynote', 'space'], 
                     time: '1d'}, true).then(usePhoto, failure);
};
</pre>

The first parameter of <tt>send</tt> is the JSON data you want to send to the application. 
The second parameter is the foreground and background Boolean. 
A value of <tt>true</tt> indicates foreground and <tt>false</tt> is background.

The user is taken to the Photos application, allowed to select photos, 
and then the Photos application returns the list of photos to your application using 
a callback:

<pre> 
var usePhoto = function(photos) {
    log('user selected photos', photos);
}
</pre>

See the  
<a href="https://github.com/sencha/SpaceExamples/tree/master/Photos">full source code for the Photos application</a>.

## Invoke Application in the Background

In the next example, an app calls the chat application in the background 
to get the presence of a user. The API calls are nearly identical to the 
previous example, except that second parameter of the <tt>send</tt> function
is set to <tt>false</tt>.

<pre>
Ext.space.Invoke.get('chat').then(send, failure);
 
var send = function(connection) {
        connection.send(
            {type: 'presence', 
                 user: 'joe@example.com'}, false)
            .then(
        success,
        failure
    );
};
 
var success = function(message) {
    console.log(message);
};
 
//output
{user: "polly@example.com", connected: true, status: 'away' }
</pre>
     
## Handling Incoming Messages From Another Application

Handling messages from other applications is accomplished with the 
<tt>onMessage</tt> API. The <tt>onMessage</tt> function receives the JSON message
and creates and returns an <tt>Ext.Promise</tt>. The <tt>Promise<tt> must be 
resolved to return data to the calling application.

<pre> 
Ext.space.Invoke.onMessage(function(senderId, message) {
 
    var promise = new Ext.Promise(); 
    handleMessage(message, promise); 
    return promise; 
});
</pre>

In <tt>handleMessage</tt>, the user info is fetched asynchronously and 
the response returns to fulfill the promise, else a rejection message
is sent back to the calling app indicating an error.

<pre>     
function handleMessage(message, promise) {
 
    if(message.type == "presence") {
 
        this.getUser(message.user, function(user){
            var response = {user: user.email, 
                            connected: user.isConnected, 
                            status: user.status};
 
            promise.fulfill(response);
 
        })
 
    } else {
         promise.reject('Message is not understood');
    }
 
}
</pre>     
