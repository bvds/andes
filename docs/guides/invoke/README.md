# Invoke

Sencha Space is a new way to manage, deploy and secure HTML5 applications on mobile devices. We wanted a way for HTML5 applications running inside of Sencha Space to communicate with each other directly and securely. To do that, we developed a simple JavaScript API that developers can quickly add to any application they deploy in Sencha Space.

So see Invoke in action check out this video: http://vimeo.com/73736732

The video shows: 1) an application that can invoke the photo app, 2) successful invocation of the photo application from the first app, 3) selection of photos using the externally invoked photo application, 4) returning the selected photos to the app that initiated the invoke call in the first place.


Using Invoke in Foreground and Background
---

There are two ways applications can communicate using Invoke: foreground or background.


An Invoke call in the foreground will switch the user from one application to another. The user can then do some work in the second application and when done, Sencha Space will return the user to the application they started with. A simple example of this is photos. In Sencha Space, you can have an application that knows where all of your organization's photos are. When any other application needed a photo, it would be able to redirect the user to the Photos application. The user can select the photos they want, and then they are returned to the application they started with, and the application has the list of photos they selected.


Background Invoke calls open up the possibility of a new class of application communication. Two applications can exchange data in the background asynchronously without the user needing to leave the application they are currently in. In the example we presented at SenchaCon, our Contacts application communicated with the chat application to get the online/offline status of the current contact and update the contact record. The Contacts application did not need to integrate a chat library or maintain a connection with a chat/presence server. It only needed to make a simple API call to the chat application running in Sencha Space.

Include the Sencha Space APIs
---

We also wanted to make adding Sencha Space APIs to your application as easy as possible. There are zero downloads required to get started. Simply include the CDN hosted copy of space.js in your application (and it doesn't have to be a Sencha Touch app, it can be any HTML5 app or website):

 
    <script src="http://space.sencha.io/space.js"></script>
 
How to Use Invoke Application to Foreground
To communicate with another application, you must first get a connection to it:

 
    Ext.space.Invoke.get('photos').then(send, failure);
 
If there isn't a Photos application or your application doesn't have permission to call that application, the failure callback will be called:

     
    var failure = function(error) {
        console.log('Could not find photos app', error);
    }
 
Once you have a connection, your application can then start sending it messages:

     
    var send = function(connection) {
        connection.send({tags: ['keynote', 'space'], 
                         time: '1d'}, true).then(usePhoto, failure);
    };
 
The first parameter of send is the JSON data you want to send to the application you are calling. The second parameter is the foreground/background boolean. A value of true is foreground and false is background.

The user will then be taken to the Photos application, allowed to select photos, and then the application will return the list of photos to your application via a callback:

 
    var usePhoto = function(photos) {
        log('user selected photos', photos);
    }
     
We provide the full sourcecode for the Photos application. To see how that code is written, check out our example repository on github: https://github.com/sencha/SpaceExamples/tree/master/Photos.

How to Use Invoke Application in the Background
---

In the next example, we will call the chat application in the background to get the presence of a user. The API calls are nearly identical to the previous example, except that we are passing false as the second parameter to the Send function.

     
    Ext.space.Invoke.get('chat').then(send, failure);
     
    var send = function(connection) {
            connection.send(
                {type: 'presence', 
                     user: 'joe@sencha.com'}, false)
                .then(
            success,
            failure
        );
    };
     
    var success = function(message) {
        console.log(message);
    };
     
    //output
    {user: "jason.cline@sencha.com", connected: true, status: 'away' }
     
Handling Incoming Messages From Another Application
---

Handling messages from other applications is accomplished with the onMessage API. The onMessage function receives the JSON message. The onMessage function must create and return an Ext.Promise. That Promise must be resolved to return data back to the calling application.

 
    Ext.space.Invoke.onMessage(function(senderId, message) {
     
        var promise = new Ext.Promise();
     
        handleMessage(message, promise);
     
        return promise;
     
    });
 
In handleMessage, the user info is fetched asynchronously and the response is returned by fulfilling the promise:

     
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
     
Invoke represents a new way of building HTML5 applications. Your applications no longer have to be islands that are connected only via a round trip to a server. By using Invoke, you can build simpler single purpose applications that expose a simple API.