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
			andes.error({
				title: "Fatal Error",
				message: "No user and/or problem data was provided; cannot continue. Please click on your browser's back button.",
				dialogType: andes.error.FATAL
			});
		});
	}
	var setCookie = function(){
		// Andes database requires that clientID be 50 characters.
		andes.sessionId = andes.projectId + new Date().getTime();
		andes.sessionId = andes.userId.substr(0,50-andes.sessionId.length) + andes.sessionId;
		var andesCookie = {
			u:andes.userId,
			p:andes.projectId,
			sid:andes.sessionId,
			closed:false
		};
		dojo.cookie("andes", dojo.toJson(andesCookie), { expires: 999 });
	}
	
	andes.closeFirst = false;
	andes.userId = query.u;
	andes.projectId = query.p;
	andes.sectionId = query.s || "no-section";
	andes.extra = query.e; //extra field for Raj
	var ck = dojo.cookie("andes");
	if(ck && ck.u){
		// There was already a cookie here
		if(ck.u==andes.userId && ck.p==andes.projectId){
			// we can continue the same session
			andes.sessionId = ck.sid;
		}else{
			andes.closeFirst = true;
			console.warn("Closing previous session", ck.u, andes.userId, ck.p, andes.projectId)
			setCookie();
		}
	}else{
		setCookie();	
	}
	
	dojo.addOnUnload(function(){
		andes.api.close({});
		// but don't clear cookie
	});
	
	dojo.addOnLoad(function(){
		// WordTip needs to be added before conEdit is removed by drawing
		andes.WordTip = new andes.WordTip();
		
		// Problem close actions set
		dojo.connect(dojo.byId("submitButton"), "click", function(){
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

andes.drawing.load();
