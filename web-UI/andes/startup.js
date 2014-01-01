// This is the body of the pre-AMD version of main.js
define([
    "dojo/dom",
    "dojo/dom-style",
    "dojo/_base/fx",
    "dojo/_base/array",
    "dojo/cookie",
    "dojo/ready",
    "dojo/io-query",
    "dojo/json",
    'dojo/_base/unload',
	 "dojo/on",
    "dojo/require",
    // pre-AMD version had the following require:
    "andes/WordTip"
],function(dom,domStyle,fx,array,cookie,ready,ioQuery,json,baseUnload,on,require,wordTip){ // Pre-AMD version had a function wrapper.

	// summary:
	//	Handles loading of app and the timing of how items load.

    // AMD conversion: this appears to be first module where
    // andes global variable is accessed.
        window.andes={};
        console.assert(window.andes,"startup.js:  window.andes not defined.");
	
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
			window.andes.error({
				title: "Fatal Error",
				message: "No user and/or problem data was provided; cannot continue. Please click on your browser's back button.",
				dialogType: window.andes.error.FATAL
			});
		});
	};

        // FNV-1a for string, 32 bit version, returning hex.
	var FNV1aHash = function(x){
		var hash = 0x811c9dc5; // 2166136261
		for (var i = 0; i < x.length; i++) {
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
		window.andes.sessionId = FNV1aHash(window.andes.userId+window.andes.projectId) + 
			'_' + new Date().getTime();
	    console.log("andes.sessionId new value ",window.andes.sessionId);
		var andesCookie = {
			u:window.andes.userId,
			p:window.andes.projectId,
			sid:window.andes.sessionId,
			closed:false
		};
		cookie("andes", json.stringify(andesCookie), { expires: 999 });
	};
       
       window.andes.closeFirst = false;
       window.andes.userId = query.u;
       window.andes.projectId = query.p;
       window.andes.sectionId = query.s || 1234;
       window.andes.extra = query.e; //extra field for Raj

	var ck = cookie("andes");
	if(ck && ck.u){
		// There was already a cookie here
		if(ck.u==window.andes.userId && ck.p==window.andes.projectId){
			// we can continue the same session
			window.andes.sessionId = ck.sid;
		    console.log("andes.sessionId set from cookie to ",window.andes.sessionId);
		}else{
			window.andes.closeFirst = true;
		  console.warn("Closing previous session", ck.u, window.andes.userId, ck.p, window.andes.projectId);
			setCookie();
		}
	}else{
		setCookie();	
	}
	
	baseUnload.addOnUnload(function(){
		window.andes.api.close({});
		// but don't clear cookie
	});


    // AMD conversion:  move this to earlier.
    // WordTip needs to be added before conEdit is removed by drawing
    window.andes.WordTip = new wordTip();
    console.log("Got WordTip=",window.andes.WordTip);

	ready(function(){

		console.info("andes/startup.js:  submit button.");
		
		// Problem close actions set
	        var submitButton=dom.byId("submitButton");
	        console.log("About to connect submit button ",submitButton);
		on(submitButton, "click", function(){
			// Needs to be non-blocking
			var closer = window.andes.api.close({});
			closer.then(function(result){
					// console.log("Made the trip", result);
					// Look for url from server, if it doesn't
					// exist, default takes user back one page
					var url, found = false;
					array.forEach(result, function(entry){
						if(entry.url){
							found = true;
							//console.log("Found url: ",entry.url);
							url = entry.url;
						}
					});
					
					found ? window.location = url : window.history.go(-1);
				}, function(error){
					console.warn("Server Error", error);
					console.log("Returning to previous page");
					window.history.go(-1);
			});
			cookie("andes", null, { expires: -1 });
			// should look for url from server that
			// can overrride default.
		});
		
		// Splash animation
		var splashNode = dom.byId("splashOverlay");
	        console.assert(splashNode,"splashOverlay element missing in html");
		var anim = fx.fadeOut({
		    node:splashNode,
		    onEnd: function(node){
			domStyle.set(node, "display", "none");
			console.log("andes.main loaded, splash removed");
		    }
		});
		anim.play();
	});
});
