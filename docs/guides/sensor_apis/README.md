# Sensor APIs in Sencha Space

Sencha Space enables an app running in a Sencha Space Client device
to determine what sensors are available in the device and to 
enable or disable access to the sensors as appropriate for an 
organization's security policy.

The following sensors are supported for use on the Space Client:

<ul>
	<li><a href="#Camera">Camera</a></li>
	<li><a href="#Connection">Connection</a></li>
</ul>

<a name="Camera"></a>
## Camera

The <code>Ext.space.Camera</code> API lets your application capture a photo from either the device's camera or 
to use the user's photo library.

To capture a photo, call <code>Ext.space.Camera.capture</code>. 
This method returns an <code>Ext.Promise</code> that will resolve after the user selects a photo.

Capture takes a number of options that allow you to specify how you would like the selected photo returned to you:

      var promise = Ext.space.Camera.capture({
          quality: 75,
          width: 200,
          height: 200,
          destination: 'data'
      });
 
      promise.then(function(imageData){ 
            // Either a URI to the image or a data URI of the selected image.
       });

<b>Note</b>: Specifying a destination of 'data' will return a raw base64-encoded string. This string can be uploaded to a server for storage and processing. It can also be assigned to an image tag, but you will need to first prepend the appropriate data URI scheme: 

	var img = document.getElementById("resultImage");
	img.src = "data:image/jpeg;base64," + imageData; 

To see a complete example of capturing a photo see the 
<a href="https://github.com/sencha/SpaceExamples/blob/master/Sensors/index.html">Sensors Example</a> in GitHub.

<a name="Connection"></a>
## Connection

The <code>Ext.space.Connection</code> API lets your application check what kind of connection the device currently has to the network:

 	Ext.space.Connection.getStatus().then(function(status){
        	log("is online" + status.online + " " + status.type)
	});

To see a complete example checking connection status see the 
<a href="https://github.com/sencha/SpaceExamples/blob/master/Sensors/index.html">Sensors Example</a> in GitHub.



