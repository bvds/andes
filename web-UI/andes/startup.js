// This is the body of the pre-AMD version of main.js
define([
    "dojo/cookie",
    "dojo/ready",
    "dojo/io-query",
    "dojo/json",
    'dojo/_base/unload',
    "andes/WordTip",
	 "dojo/on"
],function(cookie,ready,ioQuery,json,baseUnload,wordTip,on){ // Pre-AMD version had a function wrapper.

    // In the pre-AMD version, andes was a global variable
    // Here we make it the object returned by this module.
    var andes={};

	// summary:
	//	Handles loading of app and the timing of how items load.
	
	var devMode = true, query;
	if(!window.location.search){
		query = {
			p:"s2e",
			//p:"s2esolved",
			u:"joe1"
		};
	}else{
		query = ioQuery.queryToObject(window.location.search.substring(1));
	}
	if(!query.u || !query.p){
		ready(function(){
			console.error("FIXME: Finalize the error message for needing to return to WebAssign.");
			andes.error({
				title: "Fatal Error",
				message: "No user and/or problem data was provided; cannot continue. Please click on your browser's back button.",
				dialogType: andes.error.FATAL
			});
		});
	};
        // FNV-1a for string, 32 bit version, returning hex.
	var FNV1aHash = function(x){
		var hash = 0x811c9dc5; // 2166136261
		for (i = 0; i < x.length; i++) {
			hash ^= x.charCodeAt(i);
			hash *= 0x01000193; // 16777619
		}
		hash &= hash; // restrict to lower 32 bits.
		// javascript doesn't handle negatives correctly
		// when converting to hex.
		if(hash<0){
			hash = 0xffffffff + hash + 1;
		}
		return Number(hash).toString(16);
	};
	var setCookie = function(){
		// Andes database requires that clientID be 50 characters.
		andes.sessionId = FNV1aHash(andes.userId+andes.projectId) + 
			'_' + new Date().getTime();
		var andesCookie = {
			u:andes.userId,
			p:andes.projectId,
			sid:andes.sessionId,
			closed:false
		};
		cookie("andes", json.stringify(andesCookie), { expires: 999 });
	};
       
       andes.closeFirst = false;
       andes.userId = query.u;
       andes.projectId = query.p;
       andes.sectionId = query.s || 1234;
       andes.extra = query.e; //extra field for Raj

	var ck = cookie("andes");
	if(ck && ck.u){
		// There was already a cookie here
		if(ck.u==andes.userId && ck.p==andes.projectId){
			// we can continue the same session
			andes.sessionId = ck.sid;
		}else{
			andes.closeFirst = true;
		  console.warn("Closing previous session", ck.u, andes.userId, ck.p, andes.projectId);
			setCookie();
		}
	}else{
		setCookie();	
	}
	
	baseUnload.addOnUnload(function(){
		andes.api.close({});
		// but don't clear cookie
	});

  // WordTip needs to be added before conEdit is removed by drawing
  andes.WordTip = new wordTip();
  console.log("Got WordTip=",andes.WordTip);

	ready(function(){
		console.info("andes/startup.js:  submit button.");
		
		// Problem close actions set
	        submitButton=dojo.byId("submitButton");
	        console.log("About to connect submit button ",submitButton);
		on(submitButton, "click", function(){
			// Needs to be non-blocking
			var closer = andes.api.close({});
			closer.then(function(result){
					// console.log("Made the trip", result);
					// Look for url from server, if it doesn't
					// exist, default takes user back one page
					var url, found = false;
					dojo.forEach(result, function(entry){
						if(entry.url){
							found = true;
							//console.log("Found url: ",entry.url);
							url = entry.url;
						}
					});
					
					found ? window.location = url : history.go(-1);
				}, function(error){
					console.warn("Server Error", error);
					console.log("Returning to previous page");
					history.go(-1);
			});
			cookie("andes", null, { expires: -1 });
			// should look for url from server that
			// can overrride default.
		});
		
		// Splash animation
		var splashNode = dojo.byId("splashOverlay"),
		anim = dojo.fadeOut({node:dojo.byId("splashOverlay")}),
		_h = on(anim, "onEnd", function(){
         		_h.remove();
			dojo.style(splashNode, "display", "none");
			console.log("andes.main loaded");
		});
		anim.play();
	});

    return andes;
});
