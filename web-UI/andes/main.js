dojo.provide("andes.main");
dojo.require("andes.WordTip");

(function(){
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
		query = dojo.queryToObject(window.location.search.substring(1));
	}
	if(!query.u || !query.p){
		dojo.addOnLoad(function(){
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
		var andesCookie = {
			u:window.andes.userId,
			p:window.andes.projectId,
			sid:window.andes.sessionId,
			closed:false
		};
		dojo.cookie("andes", dojo.toJson(andesCookie), { expires: 999 });
	};
	
	window.andes.closeFirst = false;
	window.andes.userId = query.u;
	window.andes.projectId = query.p;
	window.andes.sectionId = query.s || 1234;
	window.andes.extra = query.e; //extra field for Raj
	var ck = dojo.cookie("andes");
	if(ck && ck.u){
		// There was already a cookie here
		if(ck.u==window.andes.userId && ck.p==window.andes.projectId){
			// we can continue the same session
			window.andes.sessionId = ck.sid;
		}else{
			window.andes.closeFirst = true;
			console.warn("Closing previous session", ck.u, window.andes.userId, ck.p, window.andes.projectId);
			setCookie();
		}
	}else{
		setCookie();	
	}
	
	dojo.addOnUnload(function(){
		window.andes.api.close({});
		// but don't clear cookie
	});
	
	dojo.addOnLoad(function(){
		// WordTip needs to be added before conEdit is removed by drawing
		window.andes.WordTip = new window.andes.WordTip();
		
		// Problem close actions set
		dojo.connect(dojo.byId("submitButton"), "click", function(){
			// Needs to be non-blocking
			var closer = window.andes.api.close({});
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
					
					found ? window.location = url : window.history.go(-1);
				}, function(error){
					console.warn("Server Error", error);
					console.log("Returning to previous page");
					window.history.go(-1);
			});
			dojo.cookie("andes", null, { expires: -1 });
			// should look for url from server that
			// can overrride default.
		});
		
		// Splash animation
		var splashNode = dojo.byId("splashOverlay"),
		anim = dojo.fadeOut({node:dojo.byId("splashOverlay")}),
		_h = dojo.connect(anim, "onEnd", function(){
			dojo.disconnect(_h);
			dojo.style(splashNode, "display", "none");
			console.log("andes.main loaded");
		});
		anim.play();
	});
	
})();

dojo.require("andes.defaults");
dojo.require("andes.PreferenceRegistry");
dojo.require("andes.convert");
dojo.require("andes.drawing");
dojo.require("andes.menu");
dojo.require("andes.help");
dojo.require("andes.api");
dojo.require("andes.error");
dojo.require("andes.variablename");

window.andes.drawing.load();
