# Secure File API

Sencha Space provides an encrypted file system where applications can store files. These files are isolated from all other applications in the user's organization. Files stored in one application cannot be read by or written to from another application. 

SecureFiles
---

To make reading and writing files easy, Sencha Space exposes a simple key value store for files. This API is optimized for the common file operations of read, write, and list. 

See Ext.space.SecureFiles for details on the API.  We also provide the SecureFiles example application in our github repository: 
<a href="https://github.com/sencha/SpaceExamples/tree/master/SecureFiles">https://github.com/sencha/SpaceExamples/tree/master/SecureFiles</a>.

In the SecureFiles example we use our camera API to fetch files from the phone's library and store them in the application's encrypted filesystem. The images are then retrieved and displayed in a list. 

First, we need to create a collection where we want to store all of the photos. Think of collections as folders, an application can have as many folders as it needs. If a collection does not exist it's automatically created:

	Ext.onSpaceReady(function(){
	   
	    photos = Ext.space.SecureFiles.get('photos');

	});


Next, to fetch a photo from the user's library, we use the Ext.space.Camera API:


	var result = Ext.space.Camera.capture({
            quality: 75,
            width: 200,
            height: 200,
            source: 'library',
            encoding: 'jpeg',
            destination: 'data'
    });

In the above code we create a 200x200 pixel thumbnail of the selected image before storing it. 
By specifying the destination as 'data', we instruct the capture call to return the image data 
as a base-64 encoded string. 

When the promise returns, we can store the image by calling a set on the photos collection:

    result.then(function(image){
        log("user chose image");
        photos.set(new Date().getTime()+".jpg", image).then(function(){
            log("image saved");
            loadImageList();
        });
    })


Next, to display all of the photos in the collection, we use Ext.space.files.Collection.keys:

	photos.keys().then(function(fileNames){
        log("image count: " + fileNames.length);
        if(fileNames.length == 0){
            div.innerHTML = "No Images";
        }
        for(var i =0, l = fileNames.length; i<l; i++){
            photos.get(fileNames[i]).then(function(content){
                var img = document.createElement("img");
                img.src = "data:image/jpeg;base64," + content;
                div.appendChild(img);
            });
        }
    });


In this code, we loop over each of the image file names and fetch the contents of each photo. Once we have the contents of the photo we create a data URI and assign it to the source of an image.

For complete source code, see 
<a href="https://github.com/sencha/SpaceExamples/tree/master/SecureFiles">https://github.com/sencha/SpaceExamples/tree/master/SecureFiles</a>.

