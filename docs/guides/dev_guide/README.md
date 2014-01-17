# Developer's Guide


Sencha Space provides a number of APIs that allow applications to have many new capabilities when running inside of a Sencha Space container. 
When designing your applications make sure your applications will degrade gracefully when not running inside of Sencha Space. 

To start using Sencha Space's APIs you simply need to include this script tag in the head of the HTML page of your application.

	<script 
	    src="http://space.sencha.io/space.js">
	</script>

If your application is a Sencha Touch application using Sencha Cmd and the Micro loader, make sure to include Sencha Space after the microloader:


 	<!-- The line below must be kept intact for Sencha Command to build your application -->
    <script id="microloader" type="text/javascript" src="touch/microloader/development.js"></script>

    <script type="text/javascript" src="http://space.sencha.io/space.js"></script>


To know if your application is currently running inside of Sencha Space, check:

	if(Ext.isSpace) {
		// Space Specific 

	} else {
		// Fallback 
	}

Ext.isSpace will be true only when inside of Sencha Space. 

You must wait until Sencha Space has fully initialized the webview the application is running in before making any Space API calls.
To do this, you can register a callback function to execute after initialization is complete.

	Ext.onSpaceReady(function(){

		// Space APIS.

	});

This method will not execute if the application is not running inside of Sencha Space.
If Sencha Space is already ready when Ext.onSpaceReady is called, then the passed function calls immediately. 



