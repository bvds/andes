# Sensor APIs in Sencha Space

Sencha Space enables an app running in a Sencha Space Client device
to determine what sensors are available in the device and to 
enable or disable access to the sensors as appropriate for an 
organization's security policy.

The following sensors are supported for use on the Space Client:

<ul>
	<li>Camera</li>
	<li>Connection</li>
</ul>


#Camera

Ext.space.Camera allows your application capture a photo from either the devices camera or the users photo library

To capture a photo call Ext.space.Camera.capture. This method will return an Ext.Promise that will resolve after the user selects a photo

Capture takes a number of options that allow you to specify how you would like the selected photo returned to you:

      var promise = Ext.space.Camera.capture({
          quality: 75,
          width: 200,
          height: 200,
          destination: 'data'
      });
 
      promise.then(function(imageData){ 
            //either URI to the image or data URI of the selected image.
       })

Please note that specifying a destination of 'data' will return a raw base64 encoded string. This string can be updated to the server for storage and processing. It can also be assigned to an image tag but you will need to first prepend the appropriate data uri scheme: 

		var img = document.getElementById("resultImage");
		img.src = "data:image/jpeg;base64," + imageData; 

To see a complete example of capturing a photo see our example on github:

https://github.com/sencha/SpaceExamples/blob/master/Sensors/index.html


#Connection

Ext.space.Connection allows your application to check what kind of connection the device currently has to the network:

	 	Ext.space.Connection.getStatus().then(function(status){
	        log("is online" + status.online + " " + status.type)
	    });

To see a complete example checking connection status see our example on github:

https://github.com/sencha/SpaceExamples/blob/master/Sensors/index.html



