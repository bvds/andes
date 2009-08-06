dojo.provide("andes.messages");

	//		This files contains general message strings
	//		used for error handling (and could be used
	//		in other places).
	//		If and when Andes becomes internationalized,
	//		this file would switch to using the i18n
	//		mechanism.
	
	
	andes.messages = {
		// get message based on error type
		server: function(){
			return {
				title:"Server Error",
				message:"The server reported an error:",
				action:"You will need to click the browser refresh button and try again."
			};
		},
		general: function(){
			return {
				title:"General Error",
				message:"The server reported an error:",
				action:"You will need to click the browser refresh button and try again."
			};
		},
		connection: function(amt){
			return {
				title: "Connection Error",
				message: "The connection to the server failed and couldn't be re-established after retrying " + amt + " times; giving up.",
				action:"Check your internet connection and try again. There also may be server problems that will be corrected in a few minutes."
			};
		}
	}